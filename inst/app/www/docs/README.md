# Universal Blinder (R and Shiny)

Universal Blinder makes **blinded copies of pharmacokinetic (PK) datasets** (SDTM and ADaM: PC, EX, ADSL, ADNCA/ADPC, ADPP, ...) that you can send to a CRO, a vendor, another team or a review tool. The copies keep their structure, so NCA and review programs still run on them. Subjects, sites, treatment and analyte names, dates, covariates and concentrations are disguised consistently across all datasets of a study. A private **key**, which never leaves your computer, maps every finding back to the original records.

Everything is done in a **Shiny app** that runs on your own computer. You do not need to be a programmer.

## Install (once)

In R (4.1 or later):

```r
install.packages(c("haven", "jsonlite", "shiny", "bslib", "shinyFiles", "DT", "httr2", "readxl", "openxlsx", "commonmark", "remotes"))
remotes::install_github("irubets/Universal_blinder_R")
```

## Start

```r
universalblinder::run_app()
```

or double-click `run_app.bat` (Windows). The app opens in your browser at `http://127.0.0.1:3838`.

## Two workflows

Open the **Study** tab, choose your study's folder (folder tree, or paste its full path, with or without quotes), and let the app create `UB_<StudyNumber>` inside it. Copy the datasets to blind into its `input` folder from the list the app shows. The app then tells you which workflow applies:

| Your PC dataset | Workflow |
|---|---|
| **has the concentrations** (PCORRES, PCSTRESC, PCSTRESN filled in) | **Workflow 1:** *Blind & QC* → *Codebook review* → *Blind & QC* again |
| **has the samples but no concentrations**, and the lab sent the results in a **separate CSV or Excel file** | **Workflow 2:** copy the results file into `input` too, then *Prepare PC* first, then as Workflow 1 |

Share only the files in `output`. Never share `input` or `private`.

After a review of the blinded data, the **Retrieve** tab returns the original records behind the reviewer's findings. The **AI assistant** tab connects to Claude (Anthropic API) or a local model (Ollama, LM Studio, ...) to explain QC findings, suggest settings and codebook decisions, and review the blinded output. Real values never go to the cloud: the app checks every request and blocks it if one is found.

**Full instructions: [USERGUIDE.md](USERGUIDE.md)** (also available inside the app).

## Without the app

Each `UB_` folder also holds `blind.R`, `prepare_pc.R` and `retrieve.R`: open one in RStudio and click Source. They use the same settings (`blind_options.json`).

## For programmers

```r
library(universalblinder)
ub <- ub_setup_study("C:/projects/ABC-123", "ABC-123")$path     # creates UB_ABC-123
ub_copy_to_input(ub, ub_scan_study("C:/projects/ABC-123", ub)$path)
ub_check_pc_results(ub)
ub_blind_folder(ub, ub_options_for_engine(ub_read_options(ub)))
ub_retrieve_folder(ub, "findings.csv")
ub_publish_check("path/to/repo-copy", c("C:/projects/ABC-123/UB_ABC-123/private/maps"))
```

The staged pipeline (`ub_batch_run()`, `ub_batch_status()`) runs the six steps (build_maps, blind, qc, unblind, verify, release) one at a time on a `study.json` file; see `inst/examples/synthetic/study.json`.

**Tests:** `devtools::test()`. They cover the full pipeline on synthetic data, the attacks of the disclosure QC, both workflows, the BQL/BLQ texts, the study setup, the reports, the app modules and the AI data gate (with mocked HTTP; no real API calls).

**What is in this repository:** the package (`R`, `inst`), the study template (`inst/template_folder`), synthetic data (`inst/examples`, `tests/testthat/fixture*`), the tests and the documentation. Real study folders, keys and data are excluded by `.gitignore`; before publishing, `ub_publish_check()` scans the files for any value held in a private key.

Change history: [CHANGELOG.md](CHANGELOG.md). Licence: MIT ([LICENSE.md](LICENSE.md)).
