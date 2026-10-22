# =============================================================================
# EPApp: Experimental Phenomenological Analysis app
# Shiny interface for Stage 5 (Interjudge Agreement Analysis) of Experimental
# Phenomenological Analysis.
#
#   Tab 1 - Protocol: upload the codebook and the assignment of interviews to
#           coding groups, check the hierarchy of the working structure, adjust it and
#           save the protocol (.json). Done once per study.
#   Tab 2 - Report:   upload the protocol and the researchers' exports from
#           ATLAS.ti and generate the HTML report. Repeat whenever new codings
#           are available; the protocol keeps the analysis unchanged.
#
# Run with:  shiny::runApp()
# =============================================================================

library(shiny)
library(bslib)
library(reactable)

options(shiny.maxRequestSize = 200 * 1024^2,
        epa.app_dir = normalizePath("."))
source("R/epa_core.R")

# Example study shipped with the app (the reference example of the article).
EXAMPLE_DIR      <- file.path("examples", "phenomenology")
EXAMPLE_PROTOCOL <- file.path(EXAMPLE_DIR, "phenomenology_protocol.json")
EXAMPLE_FILES    <- c(`Coder 1` = "Coder1.xlsx", `Coder 2` = "Coder2.xlsx", `Coder 3` = "Coder3.xlsx")

ROLE_COLORS <- c(
  "Phase" = "#1f77b4", "Unit" = "#2ca02c", "Collapsed" = "#7fbf7f",
  "Structure" = "#9467bd", "Structure (collapsed)" = "#c5b0d5",
  "Parent" = "#7f7f7f", "Phase family (not analyzed)" = "#aec7e8",
  "Excluded" = "#d62728", "Not numbered" = "#bbbbbb"
)

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
ui <- page_navbar(
  title = "EPApp",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  fillable = FALSE,

  # ---- Tab 1: protocol -------------------------------------------------------
  nav_panel(
    "1 · Protocol",
    layout_sidebar(
      sidebar = sidebar(
        width = 370,
        actionButton("load_example", "Try the example study", icon = icon("flask"),
                     class = "btn-outline-primary w-100 mb-3"),
        h6("Edit a saved protocol"),
        fileInput("prot_in", NULL, accept = ".json", placeholder = "protocol.json (optional)"),
        hr(),
        h6("Or create a new one"),
        textInput("project", "Project name", "EPA study"),
        fileInput("codebook", "Codebook (.xlsx)", accept = ".xlsx",
                  placeholder = "codebook or one export"),
        helpText("A codebook exported from ATLAS.ti (column 'Code'/'Código'), ",
                 "or the export of one researcher (its code columns are used)."),
        fileInput("assign", "Interviews and coding groups (.xlsx)", accept = ".xlsx",
                  placeholder = "Entrevista | Dupla"),
        helpText("Two columns: the interview ID and its coding group, written with the ",
                 "researchers' names: 'A-B' for a dyad (e.g. c1-c2), 'A-B-C' for a triad. ",
                 "Dyads and triads can be mixed."),
        accordion(
          open = FALSE,
          accordion_panel(
            "Matching and IDs",
            radioButtons("match_by", "Match the codes of each export by",
                         c("Code number" = "number", "Column position" = "position")),
            helpText("Use 'Column position' if researchers named or numbered codes ",
                     "differently but kept the same code tree."),
            textInput("id_regex", "Interview ID rule (regex)", "[0-9]+"),
            helpText("Extracts the ID from the interview name and from ATLAS.ti ",
                     "'Document Name'. Default: first number (P0007 = sub-gutslei0007 = 7)."),
            textInput("drop_docs", "Ignore documents matching", "memo|test")
          ),
          accordion_panel(
            "Agreement",
            numericInput("kappa_thr", "Kappa threshold", 0.80, min = 0, max = 1, step = 0.05),
            radioButtons("consensus_rule", "Stabilized dataset: keep an element when",
                         c("All researchers of the group coded it" = "all",
                           "Most researchers coded it (2 of 3 in triads)" = "majority"))
          )
        ),
        hr(),
        downloadButton("save_prot", "Save protocol (.json)", class = "btn-success w-100")
      ),

      uiOutput("boxes"),
      card(
        card_header("Analytic levels"),
        layout_columns(
          col_widths = c(5, 7),
          radioButtons("diachronic", "Does the study have a diachronic axis (temporal phases)?",
                       c("Yes: quotations are coded with temporal phases" = "yes",
                         "No: analyze each interview as a whole" = "no")),
          conditionalPanel(
            "input.diachronic == 'yes'",
            radioButtons("unphased", "Quotations without a phase code",
                         c("Ignore them" = "ignore",
                           "Analyze them as an extra phase ('No phase identified')" = "include"))
          ),
          conditionalPanel(
            "input.diachronic == 'no'",
            helpText("Each interview is analyzed as a whole (synchronic analysis only) and the ",
                     "temporal phase level is not reported. If the codebook contains phase codes, ",
                     "add them to 'Exclude codes' so they are not treated as categories.")
          )
        ),
        layout_columns(
          col_widths = c(7, 5),
          conditionalPanel(
            "input.diachronic == 'yes'",
            selectInput("phase_fam", "Code family of the temporal phases", choices = NULL,
                        width = "100%"),
            checkboxGroupInput("phases", "Temporal phases analyzed (click to include or leave out)",
                               choices = character(0), width = "100%"),
            div(class = "d-flex gap-2 mb-2",
                actionLink("phases_all", "Select all"), span("\u00b7"),
                actionLink("phases_none", "Clear")),
            helpText("Phases left out (e.g. 1.0 Preparation, 1.4 Post-Interaction) are not analyzed, ",
                     "and their codes are not treated as categories.")
          ),
          selectInput("struct_fam", "Experiential structures (code family)",
                      choices = c("Not analyzed" = ""), width = "100%")
        ),
        layout_columns(
          col_widths = c(2, 5, 5),
          numericInput("max_depth", "Deepest level", 3, min = 1, max = 6, width = "100%"),
          selectizeInput("collapse_at", "Analyze at a higher level: merge sub-codes into",
                         choices = NULL, multiple = TRUE, width = "100%",
                         options = list(placeholder = "e.g. 2.1 Affective Quality",
                                        plugins = list("remove_button"))),
          selectizeInput("excluded", "Exclude codes (with their sub-codes)",
                         choices = NULL, multiple = TRUE, width = "100%",
                         options = list(placeholder = "e.g. a family not used in this study",
                                        plugins = list("remove_button")))
        ),
        helpText("Codes deeper than 'Deepest level' are counted as their ancestor at that level. ",
                 "Merging a code counts all its sub-codes as that code (e.g. triangulate ",
                 "'2.1 Affective Quality' instead of each affective quality). Codes that keep ",
                 "sub-categories are not analyzed by themselves.")
      ),
      uiOutput("checks"),
      navset_card_underline(
        full_screen = TRUE,
        nav_panel(
          "Hierarchy tree",
          div(class = "d-flex gap-3 align-items-end",
              selectInput("tree_filter", "Show", width = "260px",
                          choices = c("All codes", "Analyzed (units, phases, structures)",
                                      "Merged or excluded"))),
          reactableOutput("tree")
        ),
        nav_panel("Units of analysis", reactableOutput("units")),
        nav_panel("Interviews and coding groups", reactableOutput("design"))
      )
    )
  ),

  # ---- Tab 2: report ---------------------------------------------------------
  nav_panel(
    "2 · Triangulation report",
    layout_sidebar(
      sidebar = sidebar(
        width = 370,
        actionButton("run_example", "Generate the example report", icon = icon("flask"),
                     class = "btn-outline-primary w-100 mb-3"),
        fileInput("prot_run", "Protocol (.json)", accept = ".json",
                  placeholder = "or use the one from tab 1"),
        uiOutput("prot_status"),
        fileInput("exports", "Researchers' exports (.xlsx)", multiple = TRUE, accept = ".xlsx",
                  placeholder = "c1.xlsx, c2.xlsx, ..."),
        uiOutput("file_map"),
        actionButton("run", "Generate report", class = "btn-primary w-100", icon = icon("play")),
        br(), br(),
        uiOutput("downloads")
      ),
      uiOutput("run_checks"),
      card(full_screen = TRUE, card_header("Report"), uiOutput("preview"), min_height = "80vh")
    )
  ),

  # ---- Tab 3: analysis -------------------------------------------------------
  nav_panel(
    "3 \u00b7 Analysis",
    layout_sidebar(
      sidebar = sidebar(
        width = 370,
        radioButtons("an_source", "Data",
                     c("Stabilized dataset of the report (tab 2)" = "study",
                       "Upload a stabilized dataset (.csv)" = "csv")),
        conditionalPanel("input.an_source == 'csv'",
                         fileInput("an_csv", NULL, accept = ".csv",
                                   placeholder = "stabilized_dataset.csv")),
        uiOutput("an_status"),
        hr(),
        selectizeInput("an_dims", "Show dimensions", choices = NULL, multiple = TRUE,
                       options = list(placeholder = "All", plugins = list("remove_button"))),
        checkboxGroupInput("an_phases", "Phases used for the structure analysis",
                           choices = character(0)),
        helpText("Percentages are percentages of participants: an element counts once per ",
                 "interview. Only the agreed elements (stabilized dataset) are used.")
      ),
      navset_card_underline(
        full_screen = TRUE,
        nav_panel("Temporal phases",
                  reactableOutput("an_phase_tbl"),
                  downloadLink("dl_an_phase", "Download (CSV)")),
        nav_panel("Categories by phase",
                  helpText("% of participants who described each category in each phase."),
                  reactableOutput("an_cat_tbl"),
                  downloadLink("dl_an_cat", "Download (CSV)")),
        nav_panel("Experiential structures",
                  reactableOutput("an_st_n"),
                  helpText("% of the participants of each structure who described each category ",
                           "in the selected phases."),
                  reactableOutput("an_st_tbl"),
                  downloadLink("dl_an_st", "Download (CSV)")),
        nav_panel("Radar chart",
                  layout_columns(
                    col_widths = c(4, 8),
                    div(
                      checkboxGroupInput("rad_st", "Structures", choices = character(0)),
                      selectizeInput("rad_cat", "Categories (at least 3)", choices = NULL,
                                     multiple = TRUE, width = "100%",
                                     options = list(plugins = list("remove_button"),
                                                    placeholder = "Select categories")),
                      numericInput("rad_ncol", "Charts per row", 2, min = 1, max = 4),
                      downloadButton("dl_radar", "Download (PNG)", class = "btn-outline-success")
                    ),
                    plotOutput("radar", height = "750px")
                  ))
      )
    )
  ),

  nav_panel("Help", card(includeMarkdown("README.md")))
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------
server <- function(input, output, session) {

  rv <- reactiveValues(codebook = NULL, assignment = NULL, pending = NULL, phase_selection = NULL,
                       report = NULL, study = NULL)

  # ---- Loading inputs --------------------------------------------------------
  observeEvent(input$codebook, {
    rv$codebook <- tryCatch(read_codebook(input$codebook$datapath), error = function(e) {
      showNotification(conditionMessage(e), type = "error"); NULL })
    if (is.null(rv$codebook)) return()
    tree <- codebook_tree(rv$codebook)
    # Sensible defaults: phase family and structure family guessed from labels.
    fam_lab <- tree %>% filter(Depth == 1)
    ph_fam <- fam_lab$Family[grepl("phase|fase|scene|escena|tempor", fam_lab$Code, ignore.case = TRUE)][1]
    st_fam <- fam_lab$Family[grepl("structure|estructura", fam_lab$Code, ignore.case = TRUE)][1]
    guess_phases <- if (!is.na(ph_fam)) tree$Code[tree$Family %in% ph_fam & !tree$HasChildren & tree$Depth > 1] else character(0)
    updateRadioButtons(session, "diachronic", selected = if (length(guess_phases)) "yes" else "no")
    rv$pending <- list(phases = guess_phases, structure_family = if (is.na(st_fam)) "" else st_fam,
                       collapse_at = character(0), excluded = character(0),
                       max_depth = max(tree$Depth, na.rm = TRUE))
  })

  observeEvent(input$assign, {
    rv$assignment <- tryCatch(read_assignment(input$assign$datapath), error = function(e) {
      showNotification(conditionMessage(e), type = "error"); NULL })
  })

  observeEvent(input$prot_in, load_protocol_ui(input$prot_in$datapath))
  observeEvent(input$load_example, load_protocol_ui(EXAMPLE_PROTOCOL))

  load_protocol_ui <- function(path) {
    p <- tryCatch(load_protocol(path), error = function(e) {
      showNotification(paste("Invalid protocol:", conditionMessage(e)), type = "error"); NULL })
    if (is.null(p)) return()
    rv$codebook <- p$codebook
    rv$assignment <- p$assignment
    updateTextInput(session, "project", value = p$project)
    updateRadioButtons(session, "match_by", selected = p$match_by)
    updateTextInput(session, "id_regex", value = p$id_regex)
    updateTextInput(session, "drop_docs", value = p$drop_documents)
    updateNumericInput(session, "kappa_thr", value = p$kappa_threshold)
    updateRadioButtons(session, "consensus_rule", selected = p$consensus_rule)
    updateRadioButtons(session, "diachronic", selected = if (is_diachronic(p)) "yes" else "no")
    updateRadioButtons(session, "unphased", selected = p$unphased)
    rv$pending <- p[c("phases", "structure_family", "collapse_at", "excluded", "max_depth")]
    showNotification("Protocol loaded. You can edit it and save it again.", type = "message")
  }

  # Phase codes of a family as clickable checkboxes.
  phase_choices <- function(fam) {
    tree <- codebook_tree(rv$codebook)
    tree$Code[!is.na(tree$Number) & tree$Family %in% fam & tree$Depth > 1]
  }
  fill_phase_choices <- function(fam, selected) {
    updateCheckboxGroupInput(session, "phases", choices = phase_choices(fam),
                             selected = intersect(selected, phase_choices(fam)))
  }
  observeEvent(input$phase_fam, {
    req(rv$codebook, input$phase_fam)
    ch <- phase_choices(input$phase_fam)
    keep <- intersect(c(rv$phase_selection, input$phases), ch)
    if (!length(keep)) {
      # Default: every phase without sub-phases.
      tree <- codebook_tree(rv$codebook)
      keep <- tree$Code[tree$Code %in% ch & !tree$HasChildren]
    }
    fill_phase_choices(input$phase_fam, keep)
  }, ignoreInit = TRUE)
  observeEvent(input$phases, rv$phase_selection <- input$phases, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$phases_all, {
    req(input$phase_fam); fill_phase_choices(input$phase_fam, phase_choices(input$phase_fam))
  })
  observeEvent(input$phases_none, {
    req(input$phase_fam); rv$phase_selection <- character(0)
    updateCheckboxGroupInput(session, "phases", selected = character(0))
  })

  # Update the code selectors whenever a codebook arrives.
  observeEvent(rv$pending, {
    req(rv$codebook)
    tree <- codebook_tree(rv$codebook)
    numbered <- tree %>% filter(!is.na(Number))
    p <- rv$pending
    fams <- numbered %>% filter(Depth == 1)
    fam_choices <- c("Not analyzed" = "", setNames(fams$Family, fams$Code))
    num_choices <- setNames(numbered$Number, numbered$Code)
    parents <- numbered %>% filter(HasChildren)
    # Family of the phases: the one of the saved/guessed phases, if any.
    ph_fam <- unique(str_extract(code_number(p$phases), "^[0-9]+"))[1]
    rv$phase_selection <- p$phases
    updateSelectInput(session, "phase_fam", choices = setNames(fams$Family, fams$Code),
                      selected = if (is.na(ph_fam)) fams$Family[1] else ph_fam)
    fill_phase_choices(if (is.na(ph_fam)) fams$Family[1] else ph_fam, p$phases)
    updateSelectInput(session, "struct_fam", choices = fam_choices, selected = p$structure_family)
    updateSelectizeInput(session, "collapse_at", choices = setNames(parents$Number, parents$Code),
                         selected = p$collapse_at, server = TRUE)
    updateSelectizeInput(session, "excluded", choices = num_choices, selected = p$excluded,
                         server = TRUE)
    updateNumericInput(session, "max_depth", value = p$max_depth,
                       max = max(numbered$Depth))
  })

  # ---- The protocol being edited --------------------------------------------
  protocol <- reactive({
    req(rv$codebook)
    p <- new_protocol(rv$codebook, rv$assignment, input$project)
    p$match_by         <- input$match_by
    p$diachronic       <- identical(input$diachronic, "yes")
    p$phases           <- if (p$diachronic) input$phases %||% character(0) else character(0)
    p$unphased         <- input$unphased %||% "ignore"
    p$structure_family <- input$struct_fam %||% ""
    p$excluded         <- input$excluded %||% character(0)
    p$collapse_at      <- input$collapse_at %||% character(0)
    p$max_depth        <- input$max_depth %||% p$max_depth
    p$id_regex         <- input$id_regex
    p$drop_documents   <- input$drop_docs
    p$kappa_threshold  <- input$kappa_thr
    p$consensus_rule   <- input$consensus_rule %||% "all"
    p
  })
  roles <- reactive(code_roles(protocol()))

  output$boxes <- renderUI({
    if (is.null(rv$codebook)) {
      return(card(class = "bg-light", card_body(
        h5("Start by uploading the codebook and the assignment of interviews to coding groups,"),
        p("or load a protocol saved before (left panel)."))))
    }
    r <- roles(); p <- protocol()
    n_int <- if (is.null(p$assignment)) 0 else nrow(p$assignment)
    n_dy  <- if (is.null(p$assignment)) 0 else length(unique(p$assignment$Dyad))
    layout_columns(
      value_box("Temporal phases", if (is_diachronic(p)) length(p$phases) else "No axis",
                theme = "primary"),
      value_box("Categories analyzed", length(unique(r$Unit[r$Role %in% c("Unit", "Collapsed")])),
                theme = "success"),
      value_box("Experiential structures", length(unique(r$Unit[grepl("^Structure", r$Role)])),
                theme = "purple"),
      value_box("Interviews", n_int, theme = "secondary"),
      value_box("Coding groups", n_dy, theme = "secondary")
    )
  })

  output$checks <- renderUI({
    req(rv$codebook)
    msgs <- check_protocol(protocol())
    if (!length(msgs)) {
      div(class = "alert alert-success", icon("check"),
          " The protocol is complete. Review the hierarchy tree and save it.")
    } else {
      div(class = "alert alert-warning", tags$ul(lapply(msgs, tags$li)))
    }
  })

  output$tree <- renderReactable({
    r <- roles()
    r <- switch(input$tree_filter,
                "Analyzed (units, phases, structures)" =
                  r %>% filter(Role %in% c("Unit", "Phase", "Structure")),
                "Merged or excluded" =
                  r %>% filter(Role %in% c("Collapsed", "Structure (collapsed)", "Excluded",
                                           "Phase family (not analyzed)", "Not numbered")),
                r)
    reactable(
      r %>% transmute(Code, Depth, Role, `Counted as` = coalesce(`Counted as`, "")),
      pagination = FALSE, height = 600, compact = TRUE, searchable = TRUE, highlight = TRUE,
      columns = list(
        Code = colDef(minWidth = 380, style = function(value, index) {
          list(paddingLeft = paste0(8 + 22 * (coalesce(r$Depth[index], 1L) - 1), "px"),
               fontWeight = if (isTRUE(r$Depth[index] == 1)) "bold" else "normal")
        }),
        Depth = colDef(maxWidth = 80, align = "center"),
        Role = colDef(minWidth = 190, cell = function(value) {
          span(style = paste0("background:", ROLE_COLORS[[value]] %||% "#999",
                              ";color:white;border-radius:10px;padding:2px 9px;font-size:0.85em"),
               value)
        }),
        `Counted as` = colDef(minWidth = 260)
      )
    )
  })

  output$units <- renderReactable({
    u <- analysis_units(protocol(), roles())
    reactable(u %>% select(Type, Dimension, `Higher-order category` = Higher_order, Category),
              groupBy = c("Type", "Dimension"), pagination = FALSE, height = 600, compact = TRUE,
              defaultExpanded = TRUE)
  })

  output$design <- renderReactable({
    a <- protocol()$assignment
    validate(need(!is.null(a), "Upload the assignment of interviews to coding groups."))
    a <- a %>% mutate(`ID used` = interview_key(Interview, protocol()$id_regex))
    a <- a %>% rename(`Coding group` = Dyad)
    reactable(a, groupBy = "Coding group", pagination = FALSE, height = 600, compact = TRUE)
  })

  output$save_prot <- downloadHandler(
    filename = function() paste0(gsub("[^A-Za-z0-9]+", "_", input$project), "_protocol.json"),
    content  = function(file) save_protocol(protocol(), file)
  )

  # ---- Tab 2: report ---------------------------------------------------------
  run_protocol <- reactive({
    if (!is.null(input$prot_run)) {
      tryCatch(load_protocol(input$prot_run$datapath), error = function(e) NULL)
    } else if (!is.null(rv$codebook)) protocol() else NULL
  })

  output$prot_status <- renderUI({
    p <- run_protocol()
    if (is.null(p)) return(div(class = "text-muted small", "No protocol yet."))
    msgs <- check_protocol(p)
    div(class = if (length(msgs)) "alert alert-warning small" else "alert alert-info small",
        strong(p$project), br(),
        paste0(if (is_diachronic(p)) paste(length(p$phases), "phases") else "no diachronic axis", " · ", nrow(p$assignment %||% data.frame()),
               " interviews · researchers: ", paste(protocol_coders(p), collapse = ", ")),
        if (length(msgs)) tags$ul(lapply(msgs, tags$li)))
  })

  # One selector per uploaded export: which researcher it belongs to.
  output$file_map <- renderUI({
    req(input$exports, run_protocol())
    coders <- protocol_coders(run_protocol())
    norm <- function(x) tolower(gsub("[^A-Za-z0-9]", "", x))
    tagList(
      h6("Researcher of each file"),
      lapply(seq_len(nrow(input$exports)), function(i) {
        f <- tools::file_path_sans_ext(input$exports$name[i])
        guess <- coders[norm(coders) == norm(f)]
        if (!length(guess)) guess <- coders[vapply(coders, function(cd) grepl(norm(cd), norm(f), fixed = TRUE), TRUE)]
        selectInput(paste0("map_", i), input$exports$name[i],
                    choices = c("(not used)" = "", coders), selected = guess[1] %||% "")
      })
    )
  })

  observeEvent(input$run, {
    p <- run_protocol()
    if (is.null(p)) { showNotification("Load or create a protocol first.", type = "error"); return() }
    msgs <- check_protocol(p)
    if (length(msgs)) { showNotification("The protocol is incomplete (see the left panel).", type = "error"); return() }
    req(input$exports)
    map <- vapply(seq_len(nrow(input$exports)), function(i) input[[paste0("map_", i)]] %||% "", "")
    if (any(duplicated(map[map != ""]))) {
      showNotification("Each researcher can only have one file.", type = "error"); return()
    }
    if (!any(map != "")) { showNotification("Assign each file to a researcher.", type = "error"); return() }
    # Copies the uploads with their original names (used in the report messages).
    in_dir <- file.path(tempdir(), paste0("exports_", as.integer(Sys.time())))
    dir.create(in_dir)
    used <- which(map != "")
    dest <- file.path(in_dir, input$exports$name[used])
    file.copy(input$exports$datapath[used], dest)
    files <- setNames(dest, map[used])
    generate_report(p, files)
  })

  observeEvent(input$run_example, {
    generate_report(load_protocol(EXAMPLE_PROTOCOL),
                    setNames(normalizePath(file.path(EXAMPLE_DIR, EXAMPLE_FILES)), names(EXAMPLE_FILES)))
  })

  generate_report <- function(p, files) {
    out_dir <- file.path(tempdir(), paste0("report_", format(Sys.time(), "%H%M%S")))
    dir.create(out_dir)
    out_file <- file.path(out_dir, paste0(gsub("[^A-Za-z0-9]+", "_", p$project), "_triangulation.html"))
    withProgress(message = "Generating the report", value = 0.2, {
      res <- tryCatch({
        rv$study <- build_study(p, files)
        incProgress(0.3, detail = "Rendering HTML")
        epa_render_report(p, files, out_file)
      }, error = function(e) { showNotification(paste("Error:", conditionMessage(e)),
                                                type = "error", duration = NULL); NULL })
      incProgress(0.5)
    })
    if (is.null(res)) return()
    prefix <- paste0("rep", as.integer(Sys.time()))
    addResourcePath(prefix, out_dir)
    rv$report <- list(file = out_file, url = paste0(prefix, "/", basename(out_file)), protocol = p)
  }

  output$run_checks <- renderUI({
    s <- rv$study; req(s)
    miss <- s$missing_codings
    if (!nrow(miss)) return(NULL)
    div(class = "alert alert-warning",
        strong("No codings found for: "),
        paste(sprintf("%s (%s)", miss$Interview, miss$Coder), collapse = ", "))
  })

  output$preview <- renderUI({
    if (is.null(rv$report))
      return(div(class = "text-muted p-4",
                 "Upload the protocol and the exports, assign each file to a researcher and click 'Generate report'."))
    tagList(
      a(href = rv$report$url, target = "_blank", icon("up-right-from-square"), " Open in a new tab"),
      tags$iframe(src = rv$report$url, style = "width:100%;height:80vh;border:none;")
    )
  })

  output$downloads <- renderUI({
    req(rv$report)
    tagList(
      downloadButton("dl_html", "Report (HTML)", class = "btn-success w-100 mb-2"),
      downloadButton("dl_data", "Stabilized dataset (CSV)", class = "btn-outline-success w-100 mb-2"),
      downloadButton("dl_idx", "Agreement indices (CSV)", class = "btn-outline-success w-100 mb-2"),
      downloadButton("dl_tri", "Triangulation table (CSV)", class = "btn-outline-success w-100")
    )
  })
  output$dl_html <- downloadHandler(filename = function() basename(rv$report$file),
                                    content = function(f) file.copy(rv$report$file, f))
  output$dl_data <- downloadHandler(filename = "stabilized_dataset.csv",
                                    content = function(f) write.csv(stabilized_dataset(rv$study), f, row.names = FALSE))
  output$dl_idx <- downloadHandler(
    filename = "agreement_indices.csv",
    content = function(f) {
      s <- rv$study
      out <- bind_rows(
        level_summary(s) %>% mutate(Group = "Pooled (Fleiss)"),
        level_summary(s, TRUE) %>% mutate(Group = "Coding group (Cohen for dyads, Fleiss for triads)"),
        if (is_diachronic(s$protocol))
          indices_by(s$phase_matrix, Phase, kappa = "fleiss") %>% mutate(Level = "Temporal phase", Group = "Pooled (Fleiss)"),
        indices_by(s$category_matrix, Dimension, Higher_order, kappa = "fleiss") %>%
          mutate(Level = "Higher-order category", Group = "Pooled (Fleiss)")
      )
      write.csv(out, f, row.names = FALSE)
    })
  output$dl_tri <- downloadHandler(
    filename = "triangulation_table.csv",
    content = function(f) write.csv(rv$study$triangulation %>%
                                      select(Dyad, Interview, Phase, Dimension, Higher_order, Category,
                                             any_of(rv$study$rcols), `Coded by`, GroupSize, Agreement) %>%
                                      rename(`Coding group` = Dyad), f, row.names = FALSE))

  # ---- Tab 3: analysis -------------------------------------------------------
  an_data <- reactive({
    if (identical(input$an_source, "csv")) {
      req(input$an_csv)
      stab <- tryCatch(read.csv(input$an_csv$datapath, check.names = FALSE, stringsAsFactors = FALSE),
                       error = function(e) NULL)
      need_cols <- c("Interview", "Temporal phase", "Type", "Dimension", "Higher-order category", "Category")
      validate(need(!is.null(stab) && all(need_cols %in% names(stab)),
                    "The CSV must be the stabilized dataset downloaded from tab 2."))
      list(stab = tibble::as_tibble(stab), interviews = unique(stab$Interview),
           phases = unique(stab$`Temporal phase`))
    } else {
      s <- rv$study
      validate(need(!is.null(s), "Generate a report in tab 2 first, or upload a stabilized dataset."))
      list(stab = stabilized_dataset(s), interviews = unique(s$design$Interview),
           phases = analysis_phases(s$protocol))
    }
  })
  an_n <- reactive(length(an_data()$interviews))

  output$an_status <- renderUI({
    d <- tryCatch(an_data(), error = function(e) NULL)
    if (is.null(d)) return(NULL)
    div(class = "alert alert-info small",
        paste0(length(d$interviews), " interviews \u00b7 ", nrow(d$stab), " agreed coded elements"))
  })

  observeEvent(an_data(), {
    d <- an_data()
    dims <- sort(unique(d$stab$Dimension[d$stab$Type == "Category"]))
    updateSelectizeInput(session, "an_dims", choices = dims, selected = character(0))
    updateCheckboxGroupInput(session, "an_phases", choices = d$phases, selected = d$phases)
  })

  filter_dims <- function(tbl) {
    if (length(input$an_dims)) tbl %>% filter(Dimension %in% input$an_dims) else tbl
  }
  # Shades each percentage cell from white (0%) to teal (100%).
  pct_cols <- function(cols) {
    setNames(lapply(cols, function(cl) colDef(
      minWidth = 120, align = "center", format = colFormat(digits = 1),
      style = function(v) {
        if (is.na(v)) return(NULL)
        list(background = grDevices::rgb(0.10, 0.62, 0.55, alpha = min(v, 100) / 100 * 0.75),
             color = if (v > 60) "white" else "black")
      })), cols)
  }
  cat_table <- function(tbl, value_cols) {
    reactable(tbl, groupBy = "Dimension", defaultExpanded = TRUE, pagination = FALSE,
              compact = TRUE, striped = FALSE, highlight = TRUE, height = 650, bordered = TRUE,
              columns = c(list(Dimension = colDef(minWidth = 170),
                               `Higher-order category` = colDef(minWidth = 190),
                               Category = colDef(minWidth = 250)), pct_cols(value_cols)))
  }

  an_phase <- reactive({
    d <- an_data()
    analysis_phases_table(d$stab, an_n(), d$phases)
  })
  output$an_phase_tbl <- renderReactable({
    reactable(an_phase(), compact = TRUE, pagination = FALSE,
              columns = list(`Temporal phase` = colDef(minWidth = 300),
                             `%` = colDef(format = colFormat(digits = 1))))
  })

  an_cat <- reactive({
    d <- an_data()
    filter_dims(analysis_category_table(d$stab, an_n(), d$phases))
  })
  output$an_cat_tbl <- renderReactable({
    t <- an_cat()
    cat_table(t, setdiff(names(t), c("Dimension", "Higher-order category", "Category")))
  })

  an_st <- reactive({
    d <- an_data()
    analysis_structure_table(d$stab, input$an_phases, d$interviews)
  })
  output$an_st_n <- renderReactable({
    d <- an_data()
    interview_structures(d$stab, d$interviews) %>%
      count(`Experiential structure` = Structure, name = "n") %>%
      mutate(`%` = round(100 * n / an_n(), 1)) %>%
      reactable(compact = TRUE, pagination = FALSE,
                columns = list(`Experiential structure` = colDef(minWidth = 350)))
  })
  output$an_st_tbl <- renderReactable({
    t <- filter_dims(an_st())
    validate(need(ncol(t) > 3, "No agreed experiential structures in the data."))
    cat_table(t, setdiff(names(t), c("Dimension", "Higher-order category", "Category")))
  })

  observeEvent(an_st(), {
    t <- an_st()
    st_cols <- setdiff(names(t), c("Dimension", "Higher-order category", "Category"))
    st_cols <- st_cols[!startsWith(st_cols, "No agreed structure")]
    updateCheckboxGroupInput(session, "rad_st", choices = st_cols,
                             selected = intersect(input$rad_st %||% st_cols, st_cols) %||% st_cols)
    updateSelectizeInput(session, "rad_cat", choices = t$Category,
                         selected = intersect(input$rad_cat, t$Category), server = TRUE)
  })

  radar_ready <- reactive({
    validate(need(length(input$rad_cat) >= 3, "Select at least 3 categories."),
             need(length(input$rad_st) >= 1, "Select at least 1 structure."))
    TRUE
  })
  output$radar <- renderPlot({
    radar_ready()
    plot_structure_radar(an_st(), input$rad_cat, input$rad_st, ncol = input$rad_ncol)
  }, res = 96)

  output$dl_radar <- downloadHandler(
    filename = "radar_structures.png",
    content = function(f) {
      radar_ready()
      nc <- min(input$rad_ncol, length(input$rad_st)); nr <- ceiling(length(input$rad_st) / nc)
      png(f, width = 5 * nc, height = 5 * nr, units = "in", res = 300)
      plot_structure_radar(an_st(), input$rad_cat, input$rad_st, ncol = input$rad_ncol)
      dev.off()
    })
  output$dl_an_phase <- downloadHandler(filename = "phases_percentages.csv",
                                        content = function(f) write.csv(an_phase(), f, row.names = FALSE))
  output$dl_an_cat <- downloadHandler(filename = "categories_by_phase.csv",
                                      content = function(f) write.csv(an_cat(), f, row.names = FALSE))
  output$dl_an_st <- downloadHandler(filename = "categories_by_structure.csv",
                                     content = function(f) write.csv(filter_dims(an_st()), f, row.names = FALSE))
}

shinyApp(ui, server)
