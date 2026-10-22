# =============================================================================
# epa_core.R
# Core functions of the EPApp (Stage 5, Analytic Cycle II).
#
#   1. Protocol      : the configuration of a study (codebook, hierarchy,
#                      temporal phases, experiential structures, coding groups).
#                      It is created once and saved as a .json file.
#   2. Hierarchy     : how each code of the codebook enters the analysis
#                      (phase, structure, unit of analysis, collapsed, excluded).
#   3. Coding data   : reading the ATLAS.ti exports of each researcher.
#   4. Matrices      : alignment of the researchers of each coding group
#                      (dyads or triads).
#   5. Agreement     : percentage agreement, PSA, kappa, AC1, alpha.
#   6. Report        : rendering the HTML report from a protocol + exports.
#
# These functions can also be used without the app, e.g.:
#   source("R/epa_core.R")
#   epa_render_report("my_protocol.json",
#                     files = c(c1 = "c1.xlsx", c2 = "c2.xlsx"),
#                     output_file = "Triangulation.html")
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(readxl)
})

# Columns exported by ATLAS.ti that are quotation metadata, not codes.
ATLAS_METADATA <- c("CASENO", "D", "Document Name", "MediaType", "quote_start",
                    "quote_end", "start_time", "end_time", "SY", "SX", "EY", "EX")

# -----------------------------------------------------------------------------
# 1. Codebook
# -----------------------------------------------------------------------------

# Numeric prefix of a code label: "2.3.5 Calm" -> "2.3.5"; "0. Structure" -> "0".
# Labels without a numeric prefix (e.g. "M1.8.1 Hug") return NA.
code_number <- function(label) {
  num <- str_match(trimws(label), "^([0-9]+(?:\\.[^.\\s]+)*)\\.?(?:\\s|$)")[, 2]
  num
}

# Sorting key that orders numbers naturally: 1.2 < 1.9 < 1.10.
code_key <- function(number) {
  vapply(number, function(n) {
    if (is.na(n)) return(NA_character_)
    parts <- suppressWarnings(as.integer(strsplit(n, ".", fixed = TRUE)[[1]]))
    parts[is.na(parts)] <- 999L   # non-numeric parts (e.g. "5.?") go last
    paste(sprintf("%04d", c(parts, rep(0L, 6 - length(parts)))[1:6]), collapse = ".")
  }, character(1), USE.NAMES = FALSE)
}

# Reads the code labels from an uploaded file. Accepts:
#  - a codebook exported from ATLAS.ti (first column "Code"/"Código"/...), or
#  - an export of a researcher (one column per code), whose code columns are used.
read_codebook <- function(path) {
  x <- read_excel(path)
  if ("Document Name" %in% names(x)) {
    labels <- setdiff(names(x), ATLAS_METADATA)
  } else {
    col <- intersect(c("Code", "Codes", "Código", "Códigos", "Codigo", "Codigos", "Name"), names(x))
    col <- if (length(col)) col[1] else names(x)[1]
    labels <- as.character(x[[col]])
  }
  labels <- trimws(labels[!is.na(labels) & trimws(labels) != ""])
  unique(labels)
}

# Builds the code tree from the labels.
codebook_tree <- function(labels) {
  num <- code_number(labels)
  tibble(Code = labels, Number = num) %>%
    mutate(Depth  = ifelse(is.na(Number), NA_integer_, str_count(Number, fixed(".")) + 1L),
           Family = str_extract(Number, "^[0-9]+"),
           Parent = ifelse(Depth > 1, sub("\\.[0-9]+$", "", Number), NA_character_),
           Key    = code_key(Number)) %>%
    mutate(HasChildren = map_lgl(Number, ~ !is.na(.x) &&
                                   any(startsWith(Number[!is.na(Number)], paste0(.x, "."))))) %>%
    arrange(is.na(Key), Key)
}

label_of <- function(tree, number) tree$Code[match(number, tree$Number, incomparables = NA)]

# -----------------------------------------------------------------------------
# 2. Protocol
# -----------------------------------------------------------------------------

# Default protocol for a codebook and an assignment table.
new_protocol <- function(codebook, assignment = NULL, project = "EPA study") {
  tree <- codebook_tree(codebook)
  list(
    project          = project,
    created          = format(Sys.time(), "%Y-%m-%d %H:%M"),
    codebook         = codebook,
    match_by         = "number",       # "number" or "position"
    diachronic       = TRUE,           # does the study have a diachronic axis (temporal phases)?
    phases           = character(0),   # labels of the temporal phases analyzed
    unphased         = "ignore",       # quotations without a phase: "ignore" or "include"
    structure_family = "",             # top-level number of the experiential structures
    excluded         = character(0),   # numbers of codes excluded (with descendants)
    collapse_at      = character(0),   # numbers whose descendants are merged into them
    max_depth        = max(tree$Depth, na.rm = TRUE),
    id_regex         = "[0-9]+",       # extracts the interview ID
    drop_documents   = "memo|test",    # documents ignored (regex, case-insensitive)
    kappa_threshold  = 0.80,
    consensus_rule   = "all",          # stabilized dataset: "all" researchers agree or "majority"
    assignment       = assignment      # data frame: Interview, Dyad
  )
}

save_protocol <- function(protocol, path) {
  jsonlite::write_json(protocol, path, auto_unbox = TRUE, pretty = TRUE,
                       dataframe = "rows", null = "null")
}

load_protocol <- function(path) {
  p <- jsonlite::read_json(path, simplifyVector = TRUE)
  for (f in c("codebook", "phases", "excluded", "collapse_at"))
    p[[f]] <- as.character(unlist(p[[f]] %||% character(0)))
  p$structure_family <- as.character(p$structure_family %||% "")
  p$diachronic <- isTRUE(p$diachronic %||% TRUE)       # protocols saved before this option
  p$unphased   <- as.character(p$unphased %||% "ignore")
  p$consensus_rule <- as.character(p$consensus_rule %||% "all")
  if (!is.null(p$assignment)) p$assignment <- tibble::as_tibble(p$assignment)
  p
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# --- Diachronic axis ----------------------------------------------------------
# Studies without a diachronic axis analyze each interview as a whole: every
# quotation belongs to a single pseudo-phase. In studies with phases, the
# quotations that were not assigned to any phase can be ignored (default) or
# analyzed as an additional pseudo-phase.
WHOLE_INTERVIEW <- "Whole interview (no diachronic axis)"
NO_PHASE        <- "No phase identified"

is_diachronic <- function(protocol) isTRUE(protocol$diachronic %||% TRUE)

# Phases used to build the matrices, including pseudo-phases.
analysis_phases <- function(protocol) {
  if (!is_diachronic(protocol)) return(WHOLE_INTERVIEW)
  c(protocol$phases, if (identical(protocol$unphased, "include")) NO_PHASE)
}

# Adds the pseudo-phase columns to a quotation-by-code matrix.
add_pseudo_phases <- function(codes, protocol) {
  if (!is_diachronic(protocol)) {
    codes[[WHOLE_INTERVIEW]] <- 1
  } else if (identical(protocol$unphased, "include")) {
    # A quotation has a phase if it carries any code of the phase family
    # (e.g. 1.4 Post-Interaction counts as a phase even if it is not analyzed).
    fam  <- unique(str_extract(code_number(protocol$phases), "^[0-9]+"))
    real <- names(codes)[str_extract(code_number(names(codes)), "^[0-9]+") %in% fam]
    in_any <- if (length(real)) rowSums(as.matrix(codes[real]) == 1, na.rm = TRUE) > 0 else FALSE
    codes[[NO_PHASE]] <- as.numeric(!in_any)
  }
  codes
}

# Reads the assignment of interviews to coding groups ("Entrevista | Dupla"):
# dyads written as "A-B", triads as "A-B-C".
read_assignment <- function(path) {
  x <- read_excel(path)
  int_col <- intersect(c("Entrevista", "Interview", "ID", "Participant", "Participante"), names(x))
  dy_col  <- intersect(c("Dupla", "Dyad", "Pair", "Coders", "Grupo", "Group", "Codificadores", "Triada", "Triad"), names(x))
  int_col <- if (length(int_col)) int_col[1] else names(x)[1]
  dy_col  <- if (length(dy_col)) dy_col[1] else names(x)[2]
  tibble(Interview = as.character(x[[int_col]]), Dyad = as.character(x[[dy_col]])) %>%
    filter(!is.na(Interview), !is.na(Dyad), Interview != "", Dyad != "") %>%
    mutate(Interview = trimws(Interview), Dyad = str_replace_all(trimws(Dyad), "\\s*-\\s*", "-"))
}

# Researchers named in the coding groups ("c1-c2-c3" -> c1, c2, c3).
protocol_coders <- function(protocol) {
  if (is.null(protocol$assignment)) return(character(0))
  sort(unique(unlist(strsplit(protocol$assignment$Dyad, "-", fixed = TRUE))))
}

# Interview key used to match the assignment table with the exports.
interview_key <- function(x, regex) {
  k <- str_extract(x, regex)
  ifelse(!is.na(k) & grepl("^[0-9]+$", k), as.character(as.integer(k)), toupper(k))
}

# -----------------------------------------------------------------------------
# 3. Hierarchy: role of each code in the analysis
# -----------------------------------------------------------------------------

# Returns the code tree with, for each code:
#   Role : "Phase", "Phase family", "Structure", "Unit", "Parent",
#          "Collapsed", "Excluded" or "Not numbered"
#   Unit : number of the unit of analysis the code is counted as.
code_roles <- function(protocol) {
  tree <- codebook_tree(protocol$codebook)
  phase_num   <- code_number(protocol$phases)
  phase_fam   <- unique(str_extract(phase_num, "^[0-9]+"))
  struct_fam  <- protocol$structure_family
  is_under <- function(n, roots) {
    if (is.na(n) || !length(roots)) return(FALSE)
    any(n == roots | startsWith(n, paste0(roots, ".")))
  }
  # Maps each code to the unit it is counted as.
  map_unit <- function(n) {
    if (is.na(n)) return(NA_character_)
    parts <- strsplit(n, ".", fixed = TRUE)[[1]]
    ancestors <- vapply(seq_along(parts), function(i) paste(parts[1:i], collapse = "."), "")
    hit <- ancestors[ancestors %in% protocol$collapse_at]
    if (length(hit)) return(hit[1])
    ancestors[min(length(ancestors), protocol$max_depth)]
  }
  tree <- tree %>%
    mutate(Excluded  = map_lgl(Number, is_under, roots = protocol$excluded),
           InPhaseFam = Family %in% phase_fam,
           InStructFam = struct_fam != "" & Family %in% struct_fam,
           Unit = map_chr(Number, map_unit))
  # A mapped unit that has other mapped units below it is a parent: it is not
  # analyzed by itself (its sub-categories are).
  analyzable <- tree %>% filter(!is.na(Number), !Excluded, !InPhaseFam)
  units <- unique(analyzable$Unit)
  parent_units <- units[map_lgl(units, ~ any(startsWith(units, paste0(.x, "."))))]
  tree %>%
    mutate(Role = case_when(
      is.na(Number)                         ~ "Not numbered",
      Excluded                              ~ "Excluded",
      Code %in% protocol$phases             ~ "Phase",
      InPhaseFam                            ~ "Phase family (not analyzed)",
      Unit %in% parent_units                ~ "Parent",
      InStructFam & Unit != Number          ~ "Structure (collapsed)",
      InStructFam                           ~ "Structure",
      Unit != Number                        ~ "Collapsed",
      TRUE                                  ~ "Unit"
    ),
    Unit = ifelse(Role %in% c("Unit", "Collapsed", "Structure", "Structure (collapsed)"),
                  Unit, NA_character_),
    `Counted as` = label_of(tree, Unit)) %>%
    select(Code, Number, Depth, Family, Role, Unit, `Counted as`, HasChildren, Key)
}

# Units of analysis with their hierarchy: Dimension (level 1), Higher-order
# category (parent of the unit) and Category (the unit).
analysis_units <- function(protocol, roles = code_roles(protocol)) {
  tree <- codebook_tree(protocol$codebook)
  roles %>%
    filter(!is.na(Unit)) %>%
    distinct(Unit, Role) %>%
    mutate(Type = ifelse(str_detect(Role, "^Structure"), "Structure", "Category")) %>%
    distinct(Unit, Type) %>%
    mutate(Category     = label_of(tree, Unit),
           Family       = str_extract(Unit, "^[0-9]+"),
           Dimension    = label_of(tree, Family),
           ParentNum    = ifelse(str_count(Unit, fixed(".")) >= 2, sub("\\.[0-9]+$", "", Unit), Family),
           Higher_order = label_of(tree, ParentNum),
           Dimension    = coalesce(Dimension, paste0(Family, ".")),
           Higher_order = coalesce(Higher_order, Dimension),
           SortKey      = code_key(Unit)) %>%
    arrange(SortKey) %>%
    select(Unit, Type, Dimension, Higher_order, Category, SortKey)
}

# Checks a protocol and returns a character vector of problems (empty = OK).
check_protocol <- function(protocol) {
  msgs <- character(0)
  if (!length(protocol$codebook)) msgs <- c(msgs, "The codebook is empty.")
  if (is_diachronic(protocol) && !length(protocol$phases))
    msgs <- c(msgs, "Select at least one temporal phase, or answer that the study has no diachronic axis.")
  if (is.null(protocol$assignment) || !nrow(protocol$assignment)) {
    msgs <- c(msgs, "Upload the assignment of interviews to coding groups (dyads or triads).")
  } else {
    a <- protocol$assignment
    parts <- strsplit(a$Dyad, "-", fixed = TRUE)
    bad <- a$Dyad[lengths(parts) < 2 | vapply(parts, anyDuplicated, 0L) > 0]
    if (length(bad)) msgs <- c(msgs, paste0("Coding groups must name at least two different researchers ('A-B' or 'A-B-C'): ",
                                           paste(unique(bad), collapse = ", ")))
    keys <- interview_key(a$Interview, protocol$id_regex)
    if (any(is.na(keys))) msgs <- c(msgs, "The interview ID rule does not find an ID in some interviews.")
    if (anyDuplicated(keys)) msgs <- c(msgs, paste0("Interviews assigned twice: ",
                                                   paste(unique(a$Interview[duplicated(keys)]), collapse = ", ")))
  }
  dup <- code_number(protocol$codebook); dup <- dup[!is.na(dup) & duplicated(dup)]
  if (length(dup) && protocol$match_by == "number")
    msgs <- c(msgs, paste0("Duplicated code numbers in the codebook (use matching by position or fix them): ",
                           paste(unique(dup), collapse = ", ")))
  msgs
}

# -----------------------------------------------------------------------------
# 4. Coding data
# -----------------------------------------------------------------------------

# Reads one researcher's export and returns its quotation-by-code matrix with
# the codebook labels. With match_by = "number" codes are matched by their
# numeric prefix; with "position" the code columns are renamed in codebook order.
read_export <- function(path, protocol) {
  raw  <- read_excel(path)
  if (!"Document Name" %in% names(raw)) stop(basename(path), ": no 'Document Name' column.")
  codes <- raw %>% select(-any_of(setdiff(ATLAS_METADATA, "Document Name")))
  code_cols <- setdiff(names(codes), "Document Name")
  report <- list(file = basename(path), unmatched = character(0), missing = character(0))
  if (protocol$match_by == "position") {
    if (length(code_cols) != length(protocol$codebook))
      stop(basename(path), ": ", length(code_cols), " code columns but ",
           length(protocol$codebook), " codes in the codebook (matching by position).")
    names(codes) <- c("Document Name", protocol$codebook)
  } else {
    cb_num  <- code_number(protocol$codebook)
    col_num <- code_number(code_cols)
    new_names <- protocol$codebook[match(col_num, cb_num, incomparables = NA)]
    report$unmatched <- code_cols[is.na(new_names) & !is.na(col_num)]
    report$missing   <- protocol$codebook[!is.na(cb_num) & !cb_num %in% col_num]
    keep <- !is.na(new_names)
    codes <- codes[, c(TRUE, keep)]
    names(codes) <- c("Document Name", new_names[keep])
  }
  codes <- codes %>%
    rename(Document = `Document Name`) %>%
    filter(!is.na(Document),
           !str_detect(Document, regex(protocol$drop_documents %||% "^$", ignore_case = TRUE))) %>%
    mutate(across(-Document, ~ suppressWarnings(as.numeric(.x))),
           Key = interview_key(Document, protocol$id_regex))
  codes <- add_pseudo_phases(codes, protocol)
  attr(codes, "check") <- report
  codes
}

# Coded elements of one export: one row per Interview x Phase x Unit coded at
# least once. Several quotations pointing to the same element count once.
export_elements <- function(codes, protocol, roles = code_roles(protocol)) {
  phases <- intersect(analysis_phases(protocol), names(codes))
  unit_of <- roles %>% filter(!is.na(Unit), Code %in% names(codes)) %>% select(Code, Unit)
  if (!length(phases) || !nrow(unit_of)) return(tibble(Key = character(), Phase = character(), Unit = character()))
  codes %>%
    select(Key, all_of(phases), all_of(unit_of$Code)) %>%
    mutate(.row = row_number()) %>%
    pivot_longer(all_of(phases), names_to = "Phase", values_to = "in_phase") %>%
    filter(in_phase %in% 1) %>%
    pivot_longer(all_of(unit_of$Code), names_to = "Code", values_to = "value") %>%
    filter(value > 0) %>%
    inner_join(unit_of, by = "Code") %>%
    distinct(Key, Phase, Unit)
}

export_phases <- function(codes, protocol) {
  phases <- intersect(analysis_phases(protocol), names(codes))
  codes %>%
    select(Key, all_of(phases)) %>%
    pivot_longer(all_of(phases), names_to = "Phase", values_to = "value") %>%
    filter(value %in% 1) %>%
    distinct(Key, Phase)
}

# -----------------------------------------------------------------------------
# 5. Study data: design, matrices and agreement
# -----------------------------------------------------------------------------

# Builds every object needed by the report.
#   files: named character vector, names = researchers, values = paths.
#
# Each interview is coded by a group of researchers given in the assignment
# ("c1-c2" for a dyad, "c1-c2-c3" for a triad). In the matrices, the decision
# of each member of the group is stored in columns R1, R2, R3... (in the order
# written in the assignment); members that a smaller group does not have are NA.
build_study <- function(protocol, files) {
  roles <- code_roles(protocol)
  units <- analysis_units(protocol, roles)
  exports <- imap(files, ~ read_export(.x, protocol))
  checks  <- map(exports, ~ attr(.x, "check"))

  design <- protocol$assignment %>%
    mutate(Key = interview_key(Interview, protocol$id_regex),
           Coder = strsplit(Dyad, "-", fixed = TRUE),
           GroupSize = lengths(Coder)) %>%
    unnest_longer(Coder, indices_to = "Pos") %>%
    mutate(Role = paste0("R", Pos))
  rcols <- paste0("R", seq_len(max(design$GroupSize)))

  available <- imap_dfr(exports, ~ tibble(Coder = .y, Key = unique(.x$Key)))
  missing_codings <- design %>% anti_join(available, by = c("Coder", "Key"))

  from_exports <- function(fun, ...) imap_dfr(exports, function(x, coder) fun(x, protocol, ...) %>% mutate(Coder = coder))
  elements <- from_exports(export_elements, roles = roles)
  phases_c <- from_exports(export_phases)

  # Coded elements of each group, one column per member (1 = coded).
  to_group <- function(coded, keys) {
    out <- design %>%
      inner_join(coded, by = c("Key", "Coder")) %>%
      mutate(value = 1) %>%
      select(Dyad, Interview, Key, all_of(keys), Role, value) %>%
      distinct() %>%
      pivot_wider(names_from = Role, values_from = value)
    for (r in setdiff(rcols, names(out))) out[[r]] <- NA_real_
    out
  }
  group_int <- distinct(design, Dyad, Interview, Key, GroupSize)
  # 0 = not coded, for the members of the group; NA for absent members.
  fill0 <- function(x) {
    for (i in seq_along(rcols)) {
      r <- rcols[i]
      x[[r]] <- ifelse(i <= x$GroupSize, coalesce(x[[r]], 0), NA_real_)
    }
    x
  }
  coded_by_any <- function(x) rowSums(as.matrix(x[rcols]) == 1, na.rm = TRUE) > 0

  phase_matrix <- group_int %>%
    crossing(Phase = analysis_phases(protocol)) %>%
    left_join(to_group(phases_c, "Phase"), by = c("Dyad", "Interview", "Key", "Phase")) %>%
    fill0()

  cat_units <- units %>% filter(Type == "Category")
  category_matrix <- phase_matrix %>%
    filter(coded_by_any(.)) %>%
    select(Dyad, Interview, Key, GroupSize, Phase) %>%
    crossing(Unit = cat_units$Unit) %>%
    left_join(to_group(elements, c("Phase", "Unit")), by = c("Dyad", "Interview", "Key", "Phase", "Unit")) %>%
    fill0() %>%
    left_join(units, by = "Unit")

  struct_units <- units %>% filter(Type == "Structure")
  structure_matrix <- if (nrow(struct_units)) {
    sm <- design %>%
      inner_join(elements %>% filter(Unit %in% struct_units$Unit), by = c("Key", "Coder")) %>%
      left_join(units %>% select(Unit, Category), by = "Unit") %>%
      group_by(Dyad, Interview, Key, Role) %>%
      summarise(Structure = paste(sort(unique(Category)), collapse = " + "), .groups = "drop") %>%
      pivot_wider(names_from = Role, values_from = Structure) %>%
      right_join(group_int, by = c("Dyad", "Interview", "Key"))
    for (i in seq_along(rcols)) {
      r <- rcols[i]
      if (!r %in% names(sm)) sm[[r]] <- NA_character_
      sm[[r]] <- ifelse(i <= sm$GroupSize, coalesce(sm[[r]], "Not assigned"), NA_character_)
    }
    sm
  } else NULL

  triangulation <- to_group(elements, c("Phase", "Unit")) %>%
    left_join(distinct(group_int, Dyad, Interview, Key, GroupSize), by = c("Dyad", "Interview", "Key")) %>%
    fill0() %>%
    left_join(units, by = "Unit") %>%
    mutate(`Coded by` = rowSums(as.matrix(across(all_of(rcols))) == 1, na.rm = TRUE),
           Agreement = ifelse(`Coded by` == GroupSize, "Yes", "No"),
           PhaseOrder = match(Phase, analysis_phases(protocol))) %>%
    arrange(Dyad, suppressWarnings(as.numeric(Key)), Key, PhaseOrder, SortKey)

  list(protocol = protocol, roles = roles, units = units, design = design, rcols = rcols,
       checks = checks, missing_codings = missing_codings,
       phase_matrix = phase_matrix, category_matrix = category_matrix,
       structure_matrix = structure_matrix, triangulation = triangulation)
}

# --- Agreement indices --------------------------------------------------------

safe_num <- function(expr) tryCatch(suppressWarnings(expr), error = function(e) NA_real_)

# ratings: one row per unit and one column per researcher (0/1 or nominal
# labels; NA when a researcher did not code that unit, e.g. dyads pooled with
# triads). kappa = "cohen" gives Cohen's kappa for two fixed researchers and
# Fleiss' kappa otherwise (any number of researchers, also when they vary
# across units); "fleiss" always gives Fleiss' kappa.
agreement_indices <- function(ratings, kappa = c("cohen", "fleiss")) {
  kappa <- match.arg(kappa)
  r <- as.data.frame(ratings)
  r <- r[, colSums(!is.na(r)) > 0, drop = FALSE]
  vals <- unlist(r, use.names = FALSE); vals <- vals[!is.na(vals)]
  binary <- is.numeric(vals) && all(vals %in% c(0, 1))
  n_r <- rowSums(!is.na(r))
  same <- apply(r, 1, function(x) length(unique(x[!is.na(x)])) <= 1)
  var_ok <- length(unique(vals)) > 1
  complete <- all(n_r == ncol(r))
  k <- if (!var_ok) NA_real_ else if (kappa == "cohen" && ncol(r) == 2 && complete)
    safe_num(irr::kappa2(r)$value) else
    safe_num(as.numeric(irrCAC::fleiss.kappa.raw(r)$est$coeff.val))
  if (binary) {
    n_pos <- rowSums(r == 1, na.rm = TRUE)
    den <- sum(n_pos * (n_r - 1))
    psa <- if (den == 0) NA_real_ else sum(n_pos * (n_pos - 1)) / den  # specific positive agreement
  }
  tibble(
    Units          = nrow(r),
    `Coded by any` = if (binary) sum(n_pos > 0) else NA_integer_,
    Disagreements  = sum(!same),
    `Agreement %`  = if (nrow(r)) 100 * mean(same) else NA_real_,
    PSA            = if (binary) psa else NA_real_,
    Kappa          = k,
    `Gwet AC1`     = if (!var_ok) NA_real_ else safe_num(as.numeric(irrCAC::gwet.ac1.raw(r)$est$coeff.val)),
    `Kripp. alpha` = if (!var_ok) NA_real_ else safe_num(as.numeric(irrCAC::krippen.alpha.raw(r)$est$coeff.val))
  )
}

indices_by <- function(data, ..., kappa = "cohen") {
  if (is.null(data) || !nrow(data)) return(NULL)
  data %>% group_by(...) %>%
    summarise(agreement_indices(pick(matches("^R[0-9]+$")), kappa = kappa), .groups = "drop")
}

# Summary by analytic level, pooled (Fleiss) or by coding group (Cohen's kappa
# for dyads, Fleiss' kappa for triads).
level_summary <- function(study, by_dyad = FALSE) {
  k <- if (by_dyad) "cohen" else "fleiss"
  g <- if (by_dyad) "Dyad" else character(0)
  bind_rows(
    # Without a diachronic axis there is no phase-level decision to compare.
    if (is_diachronic(study$protocol))
      indices_by(study$phase_matrix, across(all_of(g)), kappa = k) %>% mutate(Level = "Temporal phases"),
    indices_by(study$category_matrix, across(all_of(g)), kappa = k) %>% mutate(Level = "Synchronic categories"),
    indices_by(study$structure_matrix, across(all_of(g)), kappa = k) %>% mutate(Level = "Experiential structures")
  ) %>% relocate(Level)
}

# Agreed coded elements (phenomenologically stabilized dataset). With
# rule = "all" an element is kept when every researcher of the group coded it;
# with "majority", when more than half of them did (only differs for triads).
stabilized_dataset <- function(study, rule = study$protocol$consensus_rule %||% "all") {
  t <- study$triangulation
  keep <- if (identical(rule, "majority")) t$`Coded by` > t$GroupSize / 2 else t$Agreement == "Yes"
  t[keep, ] %>%
    transmute(Interview, Dyad, `Temporal phase` = Phase, Type, Dimension,
              `Higher-order category` = Higher_order, Category)
}

# -----------------------------------------------------------------------------
# 6. Report
# -----------------------------------------------------------------------------

# Renders the HTML report. `protocol` may be a list or the path to a .json file.
epa_render_report <- function(protocol, files, output_file = "Triangulation_report.html",
                              output_dir = dirname(output_file), quiet = TRUE) {
  if (is.character(protocol)) protocol <- load_protocol(protocol)
  app_dir <- getOption("epa.app_dir", ".")
  work <- tempfile("epa_report_"); dir.create(work)
  file.copy(file.path(app_dir, "report", "epa_report.Rmd"), work)
  file.copy(file.path(app_dir, "R", "epa_core.R"), work)
  prot_path <- file.path(work, "protocol.json")
  save_protocol(protocol, prot_path)
  files <- setNames(normalizePath(files), names(files))
  out <- rmarkdown::render(file.path(work, "epa_report.Rmd"),
                           params = list(protocol = prot_path, files = as.list(files)),
                           output_file = basename(output_file),
                           output_dir = normalizePath(output_dir, mustWork = FALSE),
                           intermediates_dir = work, knit_root_dir = work,
                           envir = new.env(parent = globalenv()), quiet = quiet)
  invisible(out)
}

# -----------------------------------------------------------------------------
# 7. Descriptive analysis of the stabilized dataset
# -----------------------------------------------------------------------------
# All percentages are percentages of participants (interviews): an element
# counts once per interview, however many quotations point to it.
#   stab: stabilized dataset (stabilized_dataset() or its CSV export).
#   n   : number of interviews (default: interviews present in `stab`).

stab_n <- function(stab, n = NULL) n %||% n_distinct(stab$Interview)

# Temporal phases: participants who described each phase.
analysis_phases_table <- function(stab, n = NULL, phase_order = NULL) {
  N <- stab_n(stab, n)
  out <- stab %>%
    distinct(Interview, `Temporal phase`) %>%
    count(`Temporal phase`, name = "n") %>%
    mutate(`%` = round(100 * n / N, 1))
  if (!is.null(phase_order)) out <- out %>% arrange(match(`Temporal phase`, phase_order))
  out
}

# Categories by phase: % of participants who described each category in each phase.
analysis_category_table <- function(stab, n = NULL, phase_order = NULL) {
  N <- stab_n(stab, n)
  phases <- phase_order %||% sort(unique(stab$`Temporal phase`))
  stab %>%
    filter(Type == "Category") %>%
    distinct(Interview, `Temporal phase`, Dimension, `Higher-order category`, Category) %>%
    count(`Temporal phase`, Dimension, `Higher-order category`, Category, name = "k") %>%
    mutate(pct = round(100 * k / N, 1)) %>%
    select(-k) %>%
    pivot_wider(names_from = `Temporal phase`, values_from = pct, values_fill = 0) %>%
    select(Dimension, `Higher-order category`, Category, any_of(phases)) %>%
    arrange(code_key(code_number(Category)))
}

# Experiential structure of each interview (agreed structures only).
interview_structures <- function(stab, interviews = unique(stab$Interview)) {
  s <- stab %>%
    filter(Type == "Structure") %>%
    distinct(Interview, Category) %>%
    group_by(Interview) %>%
    summarise(Structure = paste(sort(Category), collapse = " + "), .groups = "drop")
  tibble(Interview = interviews) %>%
    left_join(s, by = "Interview") %>%
    mutate(Structure = coalesce(Structure, "No agreed structure"))
}

# Categories by structure: % of the participants of each structure who
# described each category in any of the selected phases.
analysis_structure_table <- function(stab, phases = NULL, interviews = unique(stab$Interview)) {
  st <- interview_structures(stab, interviews)
  n_st <- st %>% count(Structure, name = "n")
  cats <- stab %>% filter(Type == "Category")
  if (length(phases)) cats <- cats %>% filter(`Temporal phase` %in% phases)
  cats %>%
    distinct(Interview, Dimension, `Higher-order category`, Category) %>%
    inner_join(st, by = "Interview") %>%
    count(Structure, Dimension, `Higher-order category`, Category, name = "k") %>%
    left_join(n_st, by = "Structure") %>%
    mutate(pct = round(100 * k / n, 1),
           Structure = paste0(Structure, " (n = ", n, ")")) %>%
    select(-k, -n) %>%
    pivot_wider(names_from = Structure, values_from = pct, values_fill = 0) %>%
    arrange(code_key(code_number(Category)))
}

# Radar chart: selected categories (rows of analysis_structure_table) for
# each selected structure (its columns).
plot_structure_radar <- function(tbl, categories, structures, ncol = 2) {
  stopifnot(length(categories) >= 3)
  d <- tbl %>% filter(Category %in% categories)
  d <- d[match(categories, d$Category), ]
  labels <- str_wrap(sub("^[0-9.]+\\s+", "", d$Category), 18)
  cols <- c("#1f4e79", "#2e86c1", "#8e2c5a", "#d35400", "#1e8449", "#7d3c98", "#b7950b", "#566573")
  nr <- ceiling(length(structures) / ncol)
  op <- par(mfrow = c(nr, min(ncol, length(structures))), mar = c(1, 2, 3, 2), xpd = TRUE)
  on.exit(par(op))
  for (i in seq_along(structures)) {
    v <- as.numeric(d[[structures[i]]]); v[is.na(v)] <- 0
    m <- as.data.frame(rbind(rep(100, length(v)), rep(0, length(v)), v))
    names(m) <- labels
    fmsb::radarchart(m, axistype = 1, caxislabels = c(0, 25, 50, 75, 100), axislabcol = "grey40",
                     pcol = grDevices::adjustcolor(cols[(i - 1) %% length(cols) + 1], 0.8),
                     pfcol = grDevices::adjustcolor(cols[(i - 1) %% length(cols) + 1], 0.3),
                     plwd = 2, cglcol = "grey75", cglty = 1, cglwd = 0.8, vlcex = 0.8,
                     title = str_wrap(sub("^[0-9.]+\\s+", "", structures[i]), 28))
  }
}
