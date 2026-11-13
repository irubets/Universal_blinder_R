# Universal Blinder (R and Shiny)

Universal Blinder makes **blinded copies of pharmacokinetic (PK) datasets** (SDTM and ADaM: PC, EX, ADSL, ADNCA/ADPC, ADPP, ...) that you can send to a CRO, a vendor, another team or a review tool. The copies keep their structure, so NCA and review programs still run on them. Subjects, sites, treatment and analyte names, dates, covariates and concentrations are disguised consistently across all datasets of a study. A private **key**, which never leaves your computer, maps every finding back to the original records.

Everything is done in a **Shiny app** that runs on your own computer. You do not need to be a programmer.

## Download version 3.1.0

- [Release notes and all downloads](https://github.com/irubets/Universal_blinder_R/releases/tag/v3.1.0)
- [R source package: universalblinder_3.1.0.tar.gz](https://github.com/irubets/Universal_blinder_R/releases/download/v3.1.0/universalblinder_3.1.0.tar.gz)
- [Illustrated USERGUIDE.html](https://github.com/irubets/Universal_blinder_R/releases/download/v3.1.0/USERGUIDE.html): download and open in your browser. The guide works offline and includes screenshots with a full-size viewer.

## Install (once)

You need **R 4.1 or later** and a web browser. RStudio is optional.

1. Download the [3.1.0 R package](https://github.com/irubets/Universal_blinder_R/releases/download/v3.1.0/universalblinder_3.1.0.tar.gz). Keep the `.tar.gz` archive intact.
2. Install the dependencies in R:

   ```r
   install.packages(c(
     "haven", "jsonlite", "shiny", "bslib", "shinyFiles", "DT",
     "httr2", "readxl", "openxlsx", "commonmark", "openssl"
   ), repos = "https://cloud.r-project.org")
   ```

3. Install the downloaded package. The file chooser lets you select it wherever your browser saved it:

   ```r
   install.packages(file.choose(), repos = NULL, type = "source")
   # Select universalblinder_3.1.0.tar.gz.
   packageVersion("universalblinder")  # Expected: 3.1.0
   ```

Install dependencies first: installing a local source archive does not download missing dependencies automatically. This app contains no compiled C/C++ code. On Windows, use CRAN's available binary dependencies; building dependencies from source may require Rtools. After updating a package already loaded in R, restart R before launching the app.

Alternatively, install the same tagged version directly from GitHub:

```r
install.packages("remotes", repos = "https://cloud.r-project.org")
remotes::install_github("irubets/Universal_blinder_R@v3.1.0")
```

## Start

```r
universalblinder::run_app()
```

The app opens in your browser at `http://127.0.0.1:3838` (or a free port if 3838 is busy). Keep R running while using it; closing the browser alone does not stop the app. Stop it with **Esc** in RStudio or **Ctrl+C** in a terminal.

If you downloaded a repository checkout, you can also double-click its `run_app.bat` on Windows after installing the package in the R version used by that launcher.

## Workflows

Open the **Study** tab, choose your study's folder (folder tree, or paste its full path, with or without quotes), and let the app create `UB_<StudyNumber>` inside it. Copy the datasets to blind into its `input` folder from the list the app shows. The app then tells you which workflow applies:

| Your PC dataset | Workflow |
|---|---|
| **has the concentrations** (PCORRES, PCSTRESC, PCSTRESN filled in) | **Workflow 1:** *Blind & QC* → *Codebook review* → *Blind & QC* again |
| **has the samples but no concentrations**, and the lab sent the results in a **separate CSV or Excel file** | **Workflow 2:** copy the results file into `input` too, then *Prepare PC* first, then as Workflow 1 |

If the lab results cannot be matched into PC, **Workflow 3** releases the lab file blinded alongside PC, using the same study key. In **Prepare PC**, review **Check matching** and enable the separate blinded lab release.

Share only the files in `output`. Never share `input` or `private`.

After a review of the blinded data, the **Retrieve** tab returns the original records behind the reviewer's findings. The **AI assistant** tab connects to Claude (Anthropic API) or a local model (Ollama, LM Studio, ...) to explain QC findings, suggest settings and codebook decisions, and review the blinded output. Real values never go to the cloud: the app checks every request and blocks it if one is found.

**Full instructions:** [download USERGUIDE.html](https://github.com/irubets/Universal_blinder_R/releases/download/v3.1.0/USERGUIDE.html), read [USERGUIDE.md](USERGUIDE.md), or open the guide inside the app.

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

**Tests:** `devtools::test()`. They cover the full pipeline on synthetic data, the attacks of the disclosure QC, all three workflows, the BQL/BLQ texts, the study setup, the reports, the app modules and the AI data gate (with mocked HTTP; no real API calls).

**What is in this repository:** the package (`R`, `inst`), the study template (`inst/template_folder`), synthetic data (`inst/examples`, `tests/testthat/fixture*`), the tests and the documentation. Real study folders, keys and data are excluded by `.gitignore`; before publishing, `ub_publish_check()` scans the files for any value held in a private key.

Change history: [CHANGELOG.md](CHANGELOG.md). Licence: MIT ([LICENSE.md](LICENSE.md)).
