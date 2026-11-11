# retrieve.R -- get the ORIGINAL records behind the findings of a review of the blinded data.
#   1. Save the findings as <this folder>/findings.csv, one row per finding. Columns (each row needs at least
#      one identifier): DATASET, UBRECID, SUBJID, USUBJID, APROFILE, ATEST, PCTESTCD, PARAMCD, AVISIT, VISIT, ...
#      and FINDING (the reviewer's text). Several values for one variable: separate them with ";".
#   2. In RStudio click Source. The original records go to <this folder>/private/retrieved (PRIVATE).
#   The app (tab "Retrieve") does the same.

# ---- nothing to change below this line ------------------------------------------------------------
local({
  here <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NULL)
  if (is.null(here) && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable())
    here <- dirname(rstudioapi::getSourceEditorContext()$path)
  if (is.null(here)) here <- getwd()
  library(universalblinder)
  f <- file.path(here, "findings.csv")
  if (!file.exists(f)) stop("Put the findings in ", f, " (see the top of this script).")
  ub_retrieve_folder(here, f, ub_options_for_engine(ub_read_options(here)))
})
