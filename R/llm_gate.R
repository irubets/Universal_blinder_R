# AI layer, part 3: the data gate (spec Section 7).
# Every piece of a prompt is a "part" whose class comes from the code that read it, never from the model:
#   PUBLIC   docs, settings structure, dataset/column names, counts, pre-flight/QC messages -- masked for Claude
#   RELEASED files in output/ from a run that ended QC PASS/WARN -- Claude only after a one-time consent per study
#   PRIVATE  anything else read from input/ or private/ (codebook rows, sample values, QC details) -- local model only
# For Claude, PRIVATE parts are dropped, PUBLIC parts are masked, and the whole prompt is leak-checked: any hit
# blocks the request (no override).

.UB_CLASSES <- c("PUBLIC", "RELEASED", "PRIVATE")

.ub_part <- function(class, label, text) {
  stopifnot(class %in% .UB_CLASSES)
  list(class = class, label = label, text = paste(text, collapse = "\n"))
}

.ub_consent_path <- function(folder) file.path(folder, "private", "logs", "ai_consent.json")

.UB_CONSENT_TEXT <- paste("The blinded datasets in output/ are pseudonymised, not anonymised. Sending them to the Claude API",
                          "is allowed by the data-protection rules that apply to this study.")

#' Has the user agreed to send this study's released (blinded) datasets to the Claude API?
#' @param folder the Universal Blinder folder of a study
#' @return TRUE or FALSE
#' @export
ub_ai_consent_get <- function(folder) {
  p <- .ub_consent_path(folder)
  if (!file.exists(p)) return(FALSE)
  isTRUE(tryCatch(.read_json(p)$consent, error = function(e) FALSE))
}

#' Record (or withdraw) the consent to send released datasets to the Claude API
#'
#' Stored in `private/logs/ai_consent.json` with the time and the statement agreed to.
#' @param folder the Universal Blinder folder of a study
#' @param value TRUE to agree, FALSE to withdraw
#' @return `value`, invisibly
#' @export
ub_ai_consent_set <- function(folder, value = TRUE) {
  p <- .ub_consent_path(folder)
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  rec <- list(consent = isTRUE(value), time = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
              user = Sys.getenv("USERNAME", Sys.getenv("USER", "")), statement = .UB_CONSENT_TEXT)
  writeLines(jsonlite::toJSON(rec, auto_unbox = TRUE, pretty = TRUE), p)
  invisible(isTRUE(value))
}

# QC status of the files in output/: "PASS"/"WARN" when they are a valid release, else "FAIL" or NA (no release).
# output/qc_summary.md is copied there by the release step only when QC passed; the run empties output/ first.
.ub_ai_qc_status <- function(folder) {
  p <- file.path(folder, "output", "qc_summary.md")
  if (!file.exists(p)) return(NA_character_)
  x <- readLines(p, warn = FALSE)
  m <- regmatches(x, regexpr("Overall: (PASS|WARN|FAIL)", x))
  st <- if (length(m)) sub("Overall: ", "", m[1]) else NA_character_
  # a later QC run that failed (private/qc newer than the release) invalidates the release
  q <- file.path(folder, "private", "qc", "qc_report.json")
  if (!is.na(st) && file.exists(q) && file.mtime(q) > file.mtime(p) + 1) {
    qs <- tryCatch(.read_json(q)$status, error = function(e) NULL)
    if (identical(qs, "FAIL")) st <- "FAIL"
  }
  st
}

# Apply the gate to a list of parts for a provider. Returns list(parts (kept, texts final), dropped labels)
.ub_ai_gate_parts <- function(parts, provider, values) {
  if (provider != "claude") return(list(parts = parts, dropped = character()))
  keep <- Filter(function(p) p$class != "PRIVATE", parts)
  dropped <- vapply(Filter(function(p) p$class == "PRIVATE", parts), function(p) p$label, "")
  keep <- lapply(keep, function(p) { if (p$class == "PUBLIC") p$text <- ub_ai_mask(p$text, values); p })
  list(parts = keep, dropped = dropped)
}

# Leak check of every text that would leave the machine. Returns NULL (clean) or a plain reason.
.ub_ai_leak_reason <- function(texts, values) {
  hits <- NULL
  for (i in seq_along(texts)) {
    h <- ub_ai_leak_check(texts[[i]], values)
    if (nrow(h)) { h$block <- .ub_or(names(texts)[i], as.character(i)); hits <- rbind(hits, h) }
  }
  if (is.null(hits)) return(NULL)
  where <- utils::head(sprintf("%s in %s, line %d (character %d)", hits$kind, hits$block, hits$line, hits$position), 10)
  kinds <- table(hits$kind)
  paste0("Blocked: the prompt still contains ", nrow(hits), " private value(s) (",
         paste(paste0(names(kinds), " x", as.integer(kinds)), collapse = ", "), "): ",
         paste(where, collapse = "; "), if (nrow(hits) > 10) "; ..." else "",
         ". Nothing was sent. Use the local model for this question.")
}

.ub_or <- function(a, b) if (is.null(a) || !length(a) || (length(a) == 1 && is.na(a))) b else a
