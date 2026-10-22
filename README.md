# EPApp
**Experimental Phenomenological Analysis app**

EPApp implements Stage 5 of Experimental Phenomenological Analysis (EPA): the triangulation of disagreements and the assessment of inter-analyst agreement. Researchers define the study protocol once (codebook hierarchy, temporal phases, experiential structures and coding dyads) and then upload the ATLAS.ti exports of each researcher to obtain an interactive HTML report. The report has wide triangulation tables for each dyad and agreement indices for each analytic level. Agreement is computed on the independent codings before discussion. The agreed elements form a phenomenologically stabilized dataset, which the app describes with percentages and radar charts. EPApp runs online without installing anything, or locally in R so that data never leave the researcher's computer.

### Workflow

**Step 1: Create the protocol (once per study).** In the *Protocol* tab:

1. Upload the **codebook** (.xlsx). This can be the codebook exported from ATLAS.ti, with a column *Code*/*Código*, or the export of one researcher; in that case its code columns are used.
2. Upload the **assignment of interviews to coding groups** (.xlsx). It needs two columns: the interview (*Entrevista*) and the group of researchers who coded it independently (*Dupla*). Write a dyad as `A-B` (e.g. `c1-c2`) and a triad as `A-B-C` (e.g. `c1-c2-c3`). Dyads and triads can be mixed in the same study. In a triad, an element is in agreement when all three researchers coded it, and the report shows how many did (e.g. 2/3). Under *Agreement* you can choose whether the stabilized dataset keeps only the elements coded by all the researchers of the group or those coded by most of them.
3. Define the **analytic levels**:
   * *Diachronic axis*: does the study have temporal phases? If not, each interview is analyzed as a whole (synchronic categories and experiential structures only). If it does, choose what to do with quotations that carry no phase code: ignore them, or analyze them as an extra phase called *No phase identified*.
   * *Temporal phases analyzed*: choose the code family of the phases. Its codes then appear as checkboxes; click to include or leave out each phase (e.g. leave out 1.0 Preparation or 1.4 Post-Interaction). Phases left out are not analyzed, and their codes are not treated as categories.
   * *Experiential structures*: the code family that contains the structures (e.g. `0.`). Each researcher's structure is compared at the interview level.
   * *Deepest level*: codes below this level are counted as their ancestor (e.g. `3.1.1.2 Father` is counted as `3.1.1 Character related` with level 3).
   * *Merge sub-codes into*: to triangulate a category at a higher level of the hierarchy (e.g. `2.1 Affective Quality` instead of each affective quality).
   * *Exclude*: codes not analyzed, together with their sub-codes (e.g. `1.4 Post-Interaction`).
4. Check the **hierarchy tree**. Each code shows its role (phase, unit of analysis, merged, structure, excluded…) and the unit it is counted as. Codes that keep sub-categories are not analyzed by themselves.
5. **Save the protocol** (.json). It contains the codebook, the hierarchy decisions and the assignment, so every report of the study uses exactly the same analysis.

**Step 2: Generate the report (every time new codings are available).** In the *Triangulation report* tab:

1. Upload the protocol (.json), or use the one just created.
2. Upload the exports of the researchers (one .xlsx per researcher, exported from ATLAS.ti with one column per code).
3. Check which researcher each file belongs to. The app guesses it from the file name.
4. Click **Generate report**. You can then download the HTML report, the stabilized dataset (agreed elements only), the agreement indices and the full triangulation table.

To add interviews later, load the protocol in step 1, upload the new assignment file and save the protocol again.

**Step 3: Describe the stabilized dataset (optional).** The *Analysis* tab uses the report just generated, or a stabilized dataset (CSV) downloaded before. Only the elements on which both researchers agreed are used, and all values are percentages of participants (an element counts once per interview).

* *Temporal phases*: number and % of participants who described each phase.
* *Categories by phase*: % of participants who described each category in each phase (dynamic lines).
* *Experiential structures*: participants by structure, and % of the participants of each structure who described each category in the selected phases (e.g. only the 1.3 sub-phases).
* *Radar chart*: choose the structures and the categories (at least 3) to compare. It can be downloaded as a PNG.

Tables can be filtered by dimension and downloaded as CSV.

### Input formats

| File | Content |
|---|---|
| Codebook | One code per row (e.g. `2.3.5 Calm`). The hierarchy is read from the numeric prefix. Codes without a numeric prefix (e.g. `M1.8.1 Hug`) are ignored. |
| Assignment | `Entrevista \| Dupla` (or `Interview \| Dyad`). |
| Exports | ATLAS.ti code-document export: one row per quotation, a *Document Name* column and one 0/1 column per code. |

**Interview IDs.** The interview ID is taken from both the assignment and the *Document Name* of the exports with a regular expression. By default it is the first number, so `P0007`, `S7` and `sub-gutslei0007_ses-01` are the same interview.

**Matching codes.** By default, codes are matched by their number. If researchers named or numbered some codes differently but kept the same code tree, choose *Column position*.

### How to use it

**Online, without installing anything.** Open the app at **[ONLINE LINK]**. The example study can be explored with *Try the example study* (tab 1) and *Generate the example report* (tab 2).

**In R, directly from GitHub.** No download is needed; R runs the current version of the repository:

```r
source("https://raw.githubusercontent.com/alejandrotroncoso/EPApp/main/install_packages.R")  # once
shiny::runGitHub("EPApp", "alejandrotroncoso")
```

**In R, from a local copy.** Download the repository (*Code → Download ZIP*) or clone it, then:

```r
setwd("path/to/EPApp")
source("install_packages.R")   # once
shiny::runApp()
```

Running it locally keeps all files on your own computer.

**Without the interface.** The report can also be generated from an R script:

```r
source("R/epa_core.R")
epa_render_report("examples/phenomenology/phenomenology_protocol.json",
                  files = c(`Coder 1` = "examples/phenomenology/Coder1.xlsx",
                            `Coder 2` = "examples/phenomenology/Coder2.xlsx",
                            `Coder 3` = "examples/phenomenology/Coder3.xlsx"),
                  output_file = "Triangulation_report.html")
```

### Privacy

The ATLAS.ti exports contain neither the text of the interviews nor the quotations, only 0/1 codes, document names and quotation positions. Still, use anonymized document names (e.g. S1, S2…). If your ethics approval does not allow uploading data to an external server, run the app locally.

### Authors and citation

Alejandro Troncoso, Antonia Zepeda and David Martínez-Pernía.

If you use this app, please cite the article: [REFERENCE OF THE ARTICLE].

### License

MIT (see `LICENSE`).
