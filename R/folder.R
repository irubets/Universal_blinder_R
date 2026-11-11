#' Blind a whole study folder in one call
#'
#' Runs build_maps -> blind -> qc -> unblind -> verify -> release on a study folder
#' with this layout:
#' \itemize{
#'   \item `input/` the unblinded datasets (`.xpt` or `.sas7bdat`), one file per dataset;
#'   \item `output/` the blinded datasets (`.xpt`, same names), `qc_summary.md`, `RELEASE_MANIFEST.md`;
#'   \item `private/` THE KEY (`maps/`), QC reports, unblinded check copies and step logs -- never share.
#' }
#' Each file is blinded with the rules of the configuration dataset of the same name
#' (ADSL, DM, EX, EC, PC, PP, VS, ADEX, ADEC, ADNCA, ADPC, ADPD, ADPP;
#' SUPPxx files use SUPPQUAL). A name without rules stops the run
#' before anything is written, unless `options$dataset_aliases` maps it to one.
#'
#' Files in `input/` other than `.xpt` / `.sas7bdat` (e.g. a bioanalytical results file in CSV or
#' Excel) are never blinded. When PC has no results (PCORRES, PCSTRESC and PCSTRESN empty) and such a
#' file is in `input/`, the run stops before anything is built: run Prepare PC first to put the results
#' into PC, or set `continue_without_results = TRUE` to blind PC without them (see [ub_check_pc_results()]).
#'
#' @param folder the study folder
#' @param options optional study-file entries: `dataset_aliases`, `identities`,
#'   `rename_vars`, `extra_suppress_vars` (`"*"` = every dataset), `qc_waivers`, `drop_vars`,
#'   `blank_where`, `config`, `config_overrides`
#' @param steps optional subset of the batch steps
#' @param log_file file that receives a copy of the messages; `NULL` (default) =
#'   `<folder>/private/logs/blind.log`
#' @param quiet if `TRUE`, print nothing (messages still go to the log file)
#' @param continue_without_results if `TRUE`, blind even when PC has no results and a results file
#'   is in `input/` (the log records the choice)
#' @param on_progress optional `function(step, message)`, called when each step starts
#'   (`message = "started"`) and ends (`message` = its status)
#' @param require_codebook_review if `TRUE` (as in `blind.R` and the app), a folder that already has a codebook
#'   is blinded only when a person has confirmed its review ([ub_codebook_confirm_review()]) and no code changed
#'   since. Otherwise the run stops before anything is written. The first run (no codebook yet) is not affected.
#' @return invisibly, a list with `released`, `qc`, `results`, `output_dir` (and `error`)
#' @export
ub_blind_folder <- function(folder, options = list(), steps = NULL, log_file = NULL, quiet = FALSE,
                            continue_without_results = FALSE, on_progress = NULL, require_codebook_review = FALSE) {
  folder <- normalizePath(folder, mustWork = FALSE)
  if (is.null(log_file)) log_file <- file.path(folder, "private", "logs", "blind.log")
  dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)
  lines <- character()
  say <- function(...) {
    msg <- paste0(...)
    lines <<- c(lines, msg)
    if (!quiet) cat(msg, "\n", sep = "")
    writeLines(lines, log_file)
  }
  writeLines(character(), log_file)
  out_dir <- file.path(folder, "output")
  res <- tryCatch({
                    if (isTRUE(require_codebook_review)) .ub_require_codebook_review(folder)
                    .blind_folder(folder, options, steps, say, isTRUE(continue_without_results), on_progress)
                  },
                  ub_error = function(e) {
                    say("ERROR: ", conditionMessage(e))
                    say("NOTHING WAS RELEASED")
                    list(released = FALSE, qc = NA_character_, results = NULL, error = conditionMessage(e),
                         output_dir = out_dir)
                  })
  invisible(res)
}

# stops (nothing written, output untouched) unless the codebook review is confirmed; no codebook yet = first run, allowed
.ub_require_codebook_review <- function(folder) {
  rv <- ub_codebook_review_status(folder)
  if (rv$status %in% c("none", "confirmed")) return(invisible(rv))
  why <- switch(rv$status,
    open = paste0(rv$open, " codebook row(s) still need an answer (red or amber)"),
    changed = paste0("codes changed after the review confirmed by ", rv$reviewer, " on ", rv$time),
    "the codebook review has not been confirmed")
  ub_error("The codebook review is not confirmed: ", why, ". Nothing was run. Review ",
           file.path(folder, "private", "codebook.csv"), " (in the app: tab 'Codebook review'), then confirm it ",
           "there with 'I have reviewed the codebook', or in R: universalblinder::ub_codebook_confirm_review(\"",
           gsub("\\\\", "/", folder), "\", \"<your name>\"). Then run again.")
}

.FOLDER_PATHS <- list(source_dir = "input", release_dir = "output", maps_dir = "private/maps",
                      blinded_dir = "private/blinded", unblinded_dir = "private/unblinded",
                      qc_dir = "private/qc", log_dir = "private/logs")
.OPTION_KEYS <- c("dataset_aliases", "identities", "rename_vars", "extra_suppress_vars", "qc_waivers", "drop_vars", "blank_where",
                  "config", "config_overrides", "lab_release", "prepare_pc")

.discover_datasets <- function(input_dir) {
  fs <- sort(list.files(input_dir))
  fs <- fs[file.exists(file.path(input_dir, fs)) & !dir.exists(file.path(input_dir, fs))]
  ext <- tolower(tools::file_ext(fs))
  stem <- tools::file_path_sans_ext(fs)
  keep <- ext %in% c("xpt", "sas7bdat") & !grepl(".", stem, fixed = TRUE)
  fs <- fs[keep]; stem <- toupper(stem[keep])
  dup <- unique(stem[duplicated(stem)])
  if (length(dup)) ub_error("both an .xpt and a .sas7bdat file for ", paste(dup, collapse = " "),
                            " in ", input_dir, " -- keep only one file per dataset")
  stats::setNames(as.list(fs), stem)
}

.UB_RESULTS_EXT <- c("csv", "xlsx", "xls", "txt")   # a separate bioanalytical results file (never blinded)
.UB_PC_RESULT_VARS <- c("PCORRES", "PCSTRESC", "PCSTRESN")

#' Does PC have its concentration results?
#'
#' Checks the PC dataset in `<folder>/input` (the file named `pc.xpt` or `pc.sas7bdat`) for results,
#' and lists the other data files there (CSV, Excel, TXT) that may hold the bioanalytical results.
#' PC delivered without results needs Prepare PC before blinding, otherwise it is released without them.
#'
#' @param folder the study folder (with `input/`)
#' @return a list:
#'   \item{has_pc}{a PC dataset is in `input/`}
#'   \item{has_results}{PC has at least one non-blank PCORRES or PCSTRESC, or a non-missing PCSTRESN}
#'   \item{lab_files}{names of the `.csv`, `.xlsx`, `.xls` and `.txt` files in `input/`}
#'   \item{merged}{Prepare PC has run (the delivered PC is kept as `private/source_original/pc.xpt` or `.sas7bdat`)}
#'   \item{status}{`"no_pc"`, `"ok"`, `"no_results"` (PC without results, no results file) or
#'     `"lab_file_not_merged"` (PC without results and a results file in `input/`)}
#'   \item{message}{the same in plain words}
#' @export
ub_check_pc_results <- function(folder) {
  folder <- normalizePath(folder, mustWork = FALSE)
  input <- file.path(folder, "input")
  datasets <- if (dir.exists(input)) .discover_datasets(input) else list()
  .ub_pc_results(folder, datasets)
}

.ub_pc_results <- function(folder, datasets) {
  input <- file.path(folder, "input")
  fs <- if (dir.exists(input)) list.files(input) else character()
  fs <- fs[!dir.exists(file.path(input, fs))]
  lab <- sort(fs[tolower(tools::file_ext(fs)) %in% .UB_RESULTS_EXT & !grepl("^readme", fs, ignore.case = TRUE)], method = "radix")
  so <- file.path(folder, "private", "source_original")
  merged <- dir.exists(so) && any(tolower(list.files(so)) %in% c("pc.xpt", "pc.sas7bdat"))
  out <- list(has_pc = !is.null(datasets[["PC"]]), has_results = FALSE, lab_files = lab, merged = merged,
              status = "no_pc", message = "There is no PC dataset in the input folder, so there are no PC results to check.")
  if (!out$has_pc) return(out)
  fn <- datasets[["PC"]]
  pc <- ub_read_dataset(file.path(input, fn))$data
  cols <- intersect(.UB_PC_RESULT_VARS, names(pc))
  has <- any(vapply(cols, function(c) {
    x <- pc[[c]]
    if (is.numeric(x)) any(!is.na(x)) else any(!is.na(x) & nzchar(trimws(as.character(x))))
  }, logical(1)))
  out$has_results <- has
  if (has || !nrow(pc)) {
    out$status <- "ok"
    out$message <- paste0(fn, " has concentration results", if (nrow(pc)) "" else " (it has no records)", ".")
    return(out)
  }
  empty <- paste0(fn, " has no concentration results (",
                  if (length(cols)) paste(cols, collapse = ", ") else "no result variables", " empty).")
  if (!length(lab)) {
    out$status <- "no_results"
    out$message <- paste(empty, "It will be blinded without results. If the bioanalytical results came as a",
                         "separate file, put that file in the input folder and run Prepare PC first.")
  } else {
    out$status <- "lab_file_not_merged"
    out$message <- paste0(empty, " ", paste(lab, collapse = ", "), if (length(lab) > 1) " are" else " is",
                          " in the input folder. If ", if (length(lab) > 1) "one of them holds" else "it holds",
                          " the bioanalytical results, run Prepare PC first to put them into PC, then blind again.",
                          if (merged) " (Prepare PC has run before, but PC still has no results.)" else "",
                          " If not, move the file out of the input folder, or continue without PC results.")
  }
  out
}

.blind_folder <- function(folder, options, steps, say, continue_without_results = FALSE, on_progress = NULL) {
  bad_opt <- setdiff(names(options), .OPTION_KEYS)
  if (length(bad_opt)) ub_error("unknown option(s) ", paste(bad_opt, collapse = ", "),
                                "; allowed: ", paste(.OPTION_KEYS, collapse = ", "))
  paths <- lapply(.FOLDER_PATHS, function(p) file.path(folder, p))
  for (p in paths) dir.create(p, recursive = TRUE, showWarnings = FALSE)
  # output/ holds only the result of this run: empty it before anything else
  old <- list.files(paths$release_dir, pattern = "(\\.(xpt|md)|_blinded\\.csv)$", ignore.case = TRUE, full.names = TRUE)
  if (length(old)) file.remove(old)

  datasets <- .discover_datasets(paths$source_dir)
  if (!length(datasets)) ub_error("no .xpt or .sas7bdat files in ", paths$source_dir,
                                  " -- copy the datasets to blind there and run again")
  cfg <- ub_load_config(options[["config", exact = TRUE]])
  profiles <- c(names(cfg$datasets), names(options$dataset_aliases))
  bad <- setdiff(names(datasets), profiles)
  bad <- bad[vapply(bad, function(n) is.null(ub_pattern_profile(n, cfg)), logical(1))]
  if (length(bad)) ub_error("no blinding profile for ", paste(sort(bad), collapse = " "),
                            " -- supported names: ", paste(sort(names(cfg$datasets)), collapse = " "), " ",
                            paste(sort(grep("^_", names(cfg$dataset_patterns), value = TRUE, invert = TRUE)), collapse = " "),
                            " -- rename the file or add a dataset alias")
  pcr <- .ub_pc_results(folder, datasets)
  lab_on <- .ub_lab_release_on(options)
  if (lab_on) {
    # Workflow 3: the lab file is released blinded next to PC, as the dataset LABPC
    lp <- ub_lab_release_prepare(folder, options, write = TRUE)
    datasets$LABPC <- normalizePath(file.path(folder, "private", "lab", "labpc.xpt"))
    kt <- lp$layout$columns$labpc_name[lp$layout$columns$kind == "kept_text"]
    if (length(kt)) options$config_overrides$datasets$LABPC$text_vars <- as.list(stats::setNames(rep("any", length(kt)), kt))
    say("Workflow 3: ", lp$lab_file, " is released blinded (LABPC): ", lp$counts[["rows"]], " row(s), ",
        lp$counts[["matched"]], " matched to PC, ", lp$counts[["left_out"]], " left out (private/reports/lab_release_report.html)")
  } else if (pcr$status == "lab_file_not_merged") {
    if (!continue_without_results)
      ub_error(pcr$message, " Nothing was blinded.")
    say("WARNING: ", pcr$message)
    say("Continuing without PC results (user choice)")
  } else if (pcr$status == "no_results") {
    say("WARNING: ", pcr$message)
  }

  raw <- options[setdiff(names(options), c("lab_release", "prepare_pc"))]   # not study-file entries
  raw$paths <- paths
  raw$datasets <- datasets
  raw$output_format <- "xpt"
  raw$codebook <- file.path(folder, "private", "codebook.csv")   # every replaced value and its code (codebook.R)
  study <- ub_study_from_list(raw, folder)

  say("Universal Blinder -- study folder ", folder)
  old_tab <- file.path(folder, "private", "aliases.csv")
  if (file.exists(old_tab)) say("note: ", old_tab, " is no longer used -- the codebook ",
                                file.path(folder, "private", "codebook.csv"), " replaces it (you can delete the old file)")
  say("datasets to blind (", length(datasets), "): ", paste(unlist(datasets[sort(names(datasets))]), collapse = " "))
  results <- ub_batch_run(study, steps = steps,
                          on_progress = function(s, m) {
                            if (m != "started") say("  [", s, "] ", m)
                            if (is.function(on_progress)) on_progress(s, m)
                          })
  st <- ub_batch_status(study)
  say("")
  say(sprintf("%-11s %-8s %s", "step", "status", "log"))
  for (i in seq_len(nrow(st))) {
    r <- results[results$step == st$step[i], , drop = FALSE]
    say(sprintf("%-11s %-8s %s", st$step[i], st$status[i], if (nrow(r)) r$log else ""))
    if (nrow(r) && !r$status %in% c("PASS", "WARN", "DONE")) say("  -> ", r$detail)
  }
  qc <- st$status[st$step == "qc"]
  released <- !is.null(results) && utils::tail(results$step, 1) == "release" &&
    utils::tail(results$status, 1) == "DONE" && all(results$status %in% c("PASS", "WARN", "DONE"))
  rule <- strrep("-", 72)
  say(rule)
  if (released) {
    say("FINISHED -- QC ", qc, ". The blinded datasets are in ", paths$release_dir)
    say("Read qc_summary.md there before sharing. Keep ", file.path(folder, "private"), " -- it is the key.")
  } else {
    say("NOTHING WAS RELEASED -- see the table above and ", file.path(paths$qc_dir, "qc_report.md"))
    say("The step logs are in ", paths$log_dir)
  }
  say(rule)
  list(released = released, qc = qc, results = results, output_dir = paths$release_dir)
}

#' Original records behind the findings of a review of FOLDER/output
#'
#' Local, PRIVATE: the original rows are written to FOLDER/private/retrieved/<dataset>.csv.
#' @param folder the study folder (with input/, output/, private/)
#' @param findings a findings CSV path or a list as returned by [ub_read_findings()]
#' @param options the options used for [ub_blind_folder()] (dataset_aliases, config, ...)
#' @param quiet suppress messages
#' @return named list of data.frames, invisibly
#' @export
ub_retrieve_folder <- function(folder, findings, options = list(), quiet = FALSE) {
  say <- function(...) if (!quiet) message(...)
  folder <- normalizePath(folder, mustWork = TRUE)
  paths <- lapply(.FOLDER_PATHS, function(p) file.path(folder, p))
  raw <- options[intersect(names(options), .OPTION_KEYS)]
  raw$paths <- paths; raw$datasets <- .discover_datasets(paths$source_dir); raw$output_format <- "xpt"
  study <- ub_study_from_list(raw, folder)
  fs <- if (is.character(findings)) ub_read_findings(findings) else findings
  out_dir <- file.path(folder, "private", "retrieved")
  # Workflow 3: findings on the released lab file go back to the lab file's own rows
  root <- .ub_crosswalk_root(study)
  last <- .ub_releases(root)
  lab_file <- if (length(last)) .read_json(file.path(root, last[1], "crosswalk.json"))$datasets$LABPC$lab_file else NULL
  is_lab <- vapply(fs, function(f) { names(f) <- toupper(names(f))
    !is.null(lab_file) && "DATASET" %in% names(f) && toupper(trimws(f[["DATASET"]])) %in% .ub_lab_names(lab_file) }, logical(1))
  res <- if (any(!is_lab)) ub_retrieve(study, fs[!is_lab], out_dir) else list()
  if (any(!is_lab)) for (nm in names(res)) res[[nm]]$FINDING_NO <- which(!is_lab)[res[[nm]]$FINDING_NO]
  if (any(is_lab)) {
    lr <- .ub_lab_retrieve(folder, root, fs[is_lab])
    if (!is.null(lr)) {
      lr$FINDING_NO <- which(is_lab)[lr$FINDING_NO]
      res$LABPC <- lr
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      utils::write.csv(lr, file.path(out_dir, "labpc.csv"), row.names = FALSE, na = "")
    }
  }
  for (nm in sort(names(res)))
    say(nm, ": ", nrow(res[[nm]]), " original record(s) for finding(s) ", paste(sort(unique(res[[nm]]$FINDING_NO)), collapse = ", "),
        " -> ", file.path(out_dir, paste0(tolower(nm), ".csv")))
  hit <- unique(unlist(lapply(res, function(d) d$FINDING_NO)))
  for (i in setdiff(seq_along(fs), hit)) say("finding ", i, ": no matching record -- check the identifiers it quotes")
  invisible(res)
}
