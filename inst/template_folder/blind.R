# blind.R -- blind every dataset in the input folder next to this script, without the app.
#   The app (universalblinder::run_app()) does the same with more help; use one or the other.
#   1. Copy the unblinded datasets (.xpt or .sas7bdat) to <this folder>/input
#      (PC without results and a separate lab file: run prepare_pc.R first).
#   2. In RStudio: open this file and click Source. The first run stops once: it has listed every drug, analyte,
#      arm and cohort name in private/codebook.csv with the code that replaces it.
#   3. Review the codebook (in the app: tab "Codebook review"), then CONFIRM the review, in the app with
#      "I have reviewed the codebook", or in the R console:
#        universalblinder::ub_codebook_confirm_review("<this folder>", "<your name>")
#      This script runs again only on a confirmed codebook; a changed or new code needs a new confirmation.
#   4. Click Source again. The blinded datasets are in <this folder>/output with qc_summary.md and RELEASE_MANIFEST.md.
# Settings: blind_options.json (edit it in the app, tab "Study settings").
# <this folder>/private holds THE KEY and the logs. Never share it.

# If PC has no results and a lab file in input was not merged, the run stops. To blind PC without results
# on purpose, set this to TRUE:
CONTINUE_WITHOUT_PC_RESULTS <- FALSE

# ---- nothing to change below this line ------------------------------------------------------------
local({
  here <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NULL)
  if (is.null(here) && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable())
    here <- dirname(rstudioapi::getSourceEditorContext()$path)
  if (is.null(here)) here <- getwd()
  library(universalblinder)
  opts <- ub_options_for_engine(ub_read_options(here))
  res <- ub_blind_folder(here, opts, continue_without_results = CONTINUE_WITHOUT_PC_RESULTS,
                         require_codebook_review = TRUE)
  if (!isTRUE(res$released)) message("Nothing was released: see the messages above and private/logs/blind.log.")
})
