# prepare_pc.R -- Workflow 2: put the results from the bioanalytical lab file into PC before blinding.
#   The app (tab "Prepare PC") does the same with dropdowns and highlighted reports; use one or the other.
#   Settings are in blind_options.json under "prepare_pc" (the app writes them). In RStudio click Source.
#   Report: private/reports/prepare_pc_report.html and private/logs/prepare_pc.log

# ---- nothing to change below this line ------------------------------------------------------------
local({
  here <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NULL)
  if (is.null(here) && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable())
    here <- dirname(rstudioapi::getSourceEditorContext()$path)
  if (is.null(here)) here <- getwd()
  library(universalblinder)
  s <- ub_read_options(here)$prepare_pc
  if (!length(s)) stop("Fill in the Prepare PC settings first (app, tab 'Prepare PC').")
  res <- ub_prepare_pc(here, s)
  ub_prepare_pc_report(res, file.path(here, "private", "reports", "prepare_pc_report.html"))
  cat(res$messages, sep = "\n")
})
