# Workflow 2: put bioanalytical results that came in a SEPARATE lab file into PC, before blinding.
#
# PC (input/pc.xpt) was delivered with the sample records (subjects, visits, time points, dates) but WITHOUT
# results; the concentrations came from the bioanalytical lab as a CSV/Excel/text file with the lab's own
# column names. Every result is matched to ONE PC record on subject + visit + nominal time (+ analyte when the
# file has several) and PCTESTCD, PCTEST, PCORRES, PCORRESU, PCSTRESC, PCSTRESN, PCSTRESU and PCLLOQ are
# filled in. Values PC already has are never overwritten (differences are reported). The PC as received is
# kept unchanged as private/source_original/pc.xpt and every run starts again from that copy, so the settings
# can be corrected and the step simply run again. When anything does not match, nothing is written.

.UB_PPC_COLUMNS <- c("subject", "visit", "nominal_time", "result", "unit", "lloq", "timepoint_name", "analyte", "specimen")
.UB_PPC_REQUIRED <- c("subject", "visit", "nominal_time", "result")
.UB_PPC_BQL <- "^\\s*(BQL|BLQ)\\s*<\\s*\\(\\s*[0-9.]+\\s*\\)\\s*$"
.UB_PPC_NUMBER <- "^\\s*[-+]?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([eE][-+]?[0-9]+)?\\s*$"
.UB_PPC_LAB_EXT <- c("csv", "xlsx", "xls", "txt")

# private helpers (independent of numeric.R)
.ub_ppc_is_bql <- function(x) grepl(.UB_PPC_BQL, x, ignore.case = TRUE, perl = TRUE)
.ub_ppc_is_num <- function(x) grepl(.UB_PPC_NUMBER, x, perl = TRUE)

.ub_ppc_stop <- function(...) {
  stop(structure(class = c("ub_ppc_stop", "error", "condition"), list(message = paste0(...), call = NULL)))
}

.ub_ppc_show <- function(df, n = 15) {
  if (!nrow(df)) return("")
  paste(utils::capture.output(print(utils::head(df, n), row.names = FALSE)), collapse = "\n")
}

.ub_ppc_list <- function(x) paste(x, collapse = ", ")

# a named character vector from a named list / vector (JSON or R)
.ub_ppc_named_chr <- function(x) {
  if (is.null(x) || !length(x)) return(stats::setNames(character(), character()))
  stats::setNames(vapply(x, function(v) as.character(v)[1], ""), trimws(names(x)))
}

.ub_ppc_settings <- function(settings) {
  s <- list(results_file = "", sheet = NULL, columns = list(), visit_map = list(), time_map = list(),
            pctestcd = "", pctest = "", analytes = list(), check_dataset = "adnca.xpt")
  for (k in names(settings)) s[k] <- list(settings[[k]])
  str1 <- function(v) if (is.null(v) || !length(v) || is.na(v[1])) "" else trimws(as.character(v[1]))
  for (k in c("results_file", "pctestcd", "pctest", "check_dataset")) s[[k]] <- str1(s[[k]])
  col <- s$columns
  s$columns <- stats::setNames(vapply(.UB_PPC_COLUMNS, function(k) str1(col[[k]]), ""), .UB_PPC_COLUMNS)
  s$visit_map <- .ub_ppc_named_chr(s$visit_map)
  tm <- .ub_ppc_named_chr(s$time_map)
  s$time_map <- stats::setNames(suppressWarnings(as.numeric(tm)), names(tm))
  if (anyNA(s$time_map)) .ub_ppc_stop("time_map: the hours must be numbers (check ",
                                      .ub_ppc_list(names(s$time_map)[is.na(s$time_map)]), ").")
  s$analytes <- .ub_ppc_named_chr(s$analytes)
  if (!is.null(s$sheet) && (!length(s$sheet) || is.na(s$sheet[1]) || identical(trimws(as.character(s$sheet[1])), "")))
    s$sheet <- NULL
  s
}

# file in DIR named STEM.xpt / STEM.sas7bdat (any case); "" when there is none
.ub_ppc_find_pc <- function(dir, what) {
  if (!dir.exists(dir)) return("")
  fs <- list.files(dir)
  fs <- fs[tolower(fs) %in% c("pc.xpt", "pc.sas7bdat")]
  if (length(fs) > 1) .ub_ppc_stop(what, " has both ", paste(fs, collapse = " and "), "; keep only one PC file.")
  if (length(fs)) fs else ""
}

#' Results files (CSV, Excel, text) in a study folder's input folder
#'
#' @param folder the study folder (with `input/`)
#' @return the file names (not paths) of `.csv`, `.xlsx`, `.xls` and `.txt` files in `input/` (README files excluded)
#' @export
ub_lab_files <- function(folder) {
  d <- file.path(folder, "input")
  if (!dir.exists(d)) return(character())
  fs <- list.files(d)
  fs <- fs[!dir.exists(file.path(d, fs))]
  sort(fs[tolower(tools::file_ext(fs)) %in% .UB_PPC_LAB_EXT & !grepl("^readme", fs, ignore.case = TRUE)])
}

# the results file as an all-character data.frame (blank = "")
.ub_ppc_read_lab <- function(path, sheet = NULL) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE))
      .ub_ppc_stop("Reading Excel files needs the R package readxl: run  install.packages(\"readxl\")  ",
                   "(or save the file as CSV and give the .csv name in results_file).")
    sh <- if (is.null(sheet)) 1L else sheet
    if (is.character(sh) && grepl("^[0-9]+$", sh) && !sh %in% readxl::excel_sheets(path)) sh <- as.integer(sh)
    if (is.character(sh) && !sh %in% readxl::excel_sheets(path))
      .ub_ppc_stop("The Excel file has no sheet '", sh, "' (setting sheet). Its sheets are: ",
                   .ub_ppc_list(readxl::excel_sheets(path)), ".")
    df <- as.data.frame(readxl::read_excel(path, sheet = sh, col_types = "text", .name_repair = "minimal",
                                           trim_ws = FALSE), stringsAsFactors = FALSE)
    for (c in seq_along(df)) df[[c]][is.na(df[[c]])] <- ""
    return(df)
  }
  if (ext %in% c("csv", "txt", "tsv")) {
    first <- readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8")
    first <- if (length(first)) first else ""
    counts <- c("\t" = lengths(regmatches(first, gregexpr("\t", first, fixed = TRUE))),
                "," = lengths(regmatches(first, gregexpr(",", first, fixed = TRUE))),
                ";" = lengths(regmatches(first, gregexpr(";", first, fixed = TRUE))))
    sep <- if (ext == "tsv") "\t" else if (max(counts) == 0) "," else names(counts)[which.max(counts)]
    rd <- function(enc) utils::read.csv(path, sep = sep, colClasses = "character", na.strings = character(),
                                        check.names = FALSE, fileEncoding = enc, quote = "\"", comment.char = "")
    df <- tryCatch(rd("UTF-8-BOM"), warning = function(w) rd("latin1"), error = function(e) rd("latin1"))
    return(df)
  }
  if (ext %in% c("xpt", "sas7bdat")) {
    df <- ub_read_dataset(path)$data
    for (c in names(df)) df[[c]] <- ifelse(is.na(df[[c]]), "", as.character(df[[c]]))
    return(df)
  }
  .ub_ppc_stop("results_file must be a .csv, .xlsx, .xls or .txt file (", basename(path), " is not).")
}

#' Column names of a results file (for choosing the column settings)
#'
#' @param folder the study folder (with `input/`)
#' @param results_file name of the results file in `input/`
#' @param sheet Excel sheet name or number (default: the first sheet)
#' @return character vector of the header names (spaces around them removed)
#' @export
ub_prepare_pc_columns <- function(folder, results_file, sheet = NULL) {
  p <- file.path(folder, "input", results_file)
  if (!file.exists(p)) ub_error(p, " not found. Copy the results file into the input folder.")
  df <- tryCatch(.ub_ppc_read_lab(p, sheet), ub_ppc_stop = function(e) ub_error(conditionMessage(e)))
  trimws(names(df))
}

#' Fill PC with results from a separate lab file (Workflow 2), before blinding
#'
#' Use it when `input/pc.xpt` has the sample records but no results and the bioanalytical lab sent the
#' concentrations as a separate file (`.csv`, `.xlsx`, `.xls` or `.txt`) in `input/`. Each result is matched to
#' one PC record on subject + visit + nominal time (+ analyte); PCTESTCD, PCTEST, PCORRES, PCORRESU, PCSTRESC,
#' PCSTRESN, PCSTRESU and PCLLOQ are filled in. Values already in PC are never overwritten. The PC as received
#' is kept as `private/source_original/pc.xpt` (or `.sas7bdat`) and every run starts again from it. If any
#' result does not match, nothing is written. A report is saved as `private/logs/prepare_pc.log`.
#'
#' @param folder the study folder (with `input/` and `private/`)
#' @param settings named list: `results_file`; `sheet` (Excel sheet name or number, default the first);
#'   `columns` = list(subject, visit, nominal_time, result, unit, lloq, timepoint_name, analyte, specimen)
#'   (the first four required); `visit_map` (lab visit -> PC.VISIT); `time_map` (lab time point name -> hours in
#'   PC.PCTPTNUM); `pctestcd` + `pctest` (one analyte) or `analytes` (several: lab analyte name -> PC.PCTESTCD);
#'   `check_dataset` (dataset in `input/` whose AVAL the results are compared with; default `"adnca.xpt"`,
#'   `""` = no check)
#' @param write `FALSE` = check and report only, write no file at all
#' @param quiet `TRUE` = print nothing
#' @return a list: `ok`, `written`, `n_lab`, `n_matched`, `unmatched` (results rows that could not be used,
#'   with `reason` and `setting_to_change`; `lab_row` is the row number as seen in Excel, header = row 1),
#'   `no_result` (PC records without a result; `status` "amber" unless PCSTAT is NOT DONE), `conflicts`
#'   (values already in PC that differ from the lab file; PC kept), `aval_check` (filled results vs AVAL of
#'   `check_dataset`; `status` "ok"/"red"), `unmatched_values` = list(visits, subjects, timepoints),
#'   `pc_values` = list(visits, timepoints) (for choosing visit_map / time_map), `messages`, `log_file`.
#'   All of it is PRIVATE (real subject IDs and values).
#' @export
ub_prepare_pc <- function(folder, settings, write = TRUE, quiet = FALSE) {
  folder <- normalizePath(folder, mustWork = FALSE)
  st <- new.env()
  st$lines <- character()
  say <- function(...) {
    msg <- paste0(...)
    st$lines <- c(st$lines, msg)
    if (!quiet) message(msg)
  }
  st$res <- list(ok = FALSE, written = FALSE, n_lab = 0L, n_matched = 0L,
                 unmatched = data.frame(lab_row = integer(), subject = character(), visit = character(),
                                        nominal_time = character(), timepoint_name = character(), analyte = character(),
                                        result = character(), reason = character(), setting_to_change = character(),
                                        stringsAsFactors = FALSE),
                 no_result = data.frame(USUBJID = character(), VISIT = character(), PCTPT = character(),
                                        PCTPTNUM = numeric(), PCSTAT = character(), status = character(),
                                        stringsAsFactors = FALSE),
                 conflicts = data.frame(USUBJID = character(), VISIT = character(), PCTPTNUM = numeric(),
                                        variable = character(), pc_value = character(), lab_value = character(),
                                        stringsAsFactors = FALSE),
                 aval_check = data.frame(USUBJID = character(), VISIT = character(), PCTPT = character(),
                                         pc_value = numeric(), aval = numeric(), status = character(),
                                         stringsAsFactors = FALSE),
                 unmatched_values = list(visits = character(), subjects = character(), timepoints = character()),
                 pc_values = list(visits = character(), timepoints = character()),
                 messages = character(), log_file = NA_character_)
  ok <- tryCatch({
    .ub_ppc_run(folder, settings, write, say, st)
    TRUE
  }, ub_ppc_stop = function(e) {
    say("")
    say("STOPPED -- nothing was written. ", conditionMessage(e))
    FALSE
  }, ub_error = function(e) {
    say("")
    say("STOPPED -- nothing was written. ", conditionMessage(e))
    FALSE
  })
  res <- st$res
  res$ok <- ok
  res$messages <- st$lines
  if (write) {
    d <- file.path(folder, "private", "logs")
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    res$log_file <- file.path(d, "prepare_pc.log")
    con <- file(res$log_file, open = "w", encoding = "UTF-8")
    writeLines(st$lines, con)
    close(con)
  }
  res
}

.ub_ppc_key <- function(u, v, t, a = NULL) {
  t <- round(t, 6); t[!is.na(t) & t == 0] <- 0
  tt <- ifelse(is.na(t), "NA", sprintf("%.6f", t))
  k <- paste(u, v, tt, sep = "\r")
  if (!is.null(a)) k <- paste(k, a, sep = "\r")
  k
}

.ub_ppc_run <- function(folder, settings, write, say, st) {
  s <- .ub_ppc_settings(settings)
  col <- s$columns
  if (!nzchar(s$results_file)) .ub_ppc_stop("Fill in the settings first: results_file is empty.")
  missing <- .UB_PPC_REQUIRED[!nzchar(col[.UB_PPC_REQUIRED])]
  if (length(missing)) .ub_ppc_stop("Fill in these column names in the setting columns: ", paste(missing, collapse = ", "), ".")
  multi <- length(s$analytes) > 0
  if (multi && !nzchar(col[["analyte"]]))
    .ub_ppc_stop("The setting analytes is filled in: also give the analyte column in the setting columns.")
  if (!multi && (!nzchar(s$pctestcd) || !nzchar(s$pctest)))
    .ub_ppc_stop("Fill in pctestcd and pctest in the settings (the analyte code used by the other datasets).",
                 .ub_ppc_analyte_hint(folder, s))

  in_dir <- file.path(folder, "input")
  orig_dir <- file.path(folder, "private", "source_original")
  in_name <- .ub_ppc_find_pc(in_dir, "The input folder")
  orig_name <- .ub_ppc_find_pc(orig_dir, "private/source_original")
  if (!nzchar(orig_name)) {
    if (!nzchar(in_name)) .ub_ppc_stop(file.path(in_dir, "pc.xpt"), " not found.")
    if (write) {
      dir.create(orig_dir, recursive = TRUE, showWarnings = FALSE)
      file.copy(file.path(in_dir, in_name), file.path(orig_dir, in_name), copy.date = TRUE)
      say("The PC you received is kept unchanged as ", file.path(orig_dir, in_name))
      orig <- file.path(orig_dir, in_name)
    } else {
      orig <- file.path(in_dir, in_name)
    }
  } else {
    orig <- file.path(orig_dir, orig_name)
  }
  # the file written: the input file's name, as .xpt (a .sas7bdat cannot be written)
  base <- if (nzchar(in_name)) in_name else basename(orig)
  pc_path <- file.path(in_dir, sub("\\.sas7bdat$", ".xpt", base, ignore.case = TRUE))

  rd <- ub_read_dataset(orig)
  pc <- rd$data; meta <- rd$meta
  for (v in c("USUBJID", "VISIT", "PCTPTNUM"))
    if (!v %in% names(pc)) .ub_ppc_stop("PC has no ", v, "; it is needed to match the results.")
  pc$USUBJID <- as.character(pc$USUBJID); pc$VISIT <- as.character(pc$VISIT)
  pcT <- round(suppressWarnings(as.numeric(pc$PCTPTNUM)), 6)
  pc_visits <- sort(unique(pc$VISIT))
  tp <- unique(data.frame(PCTPT = if ("PCTPT" %in% names(pc)) as.character(pc$PCTPT) else "", T = pcT,
                          stringsAsFactors = FALSE))
  tp <- tp[order(tp$T, tp$PCTPT), , drop = FALSE]
  st$res$pc_values <- list(visits = pc_visits,
                           timepoints = unique(ifelse(nzchar(tp$PCTPT), paste0(tp$PCTPT, " (", format(tp$T, trim = TRUE), " h)"),
                                                      paste0(format(tp$T, trim = TRUE), " h"))))

  res_path <- file.path(in_dir, s$results_file)
  if (!file.exists(res_path)) .ub_ppc_stop(res_path, " not found. Copy the results file into the input folder.")
  lab <- .ub_ppc_read_lab(res_path, s$sheet)
  names(lab) <- trimws(names(lab))
  used <- col[nzchar(col)]
  bad <- unname(used[!used %in% names(lab)])
  if (length(bad))
    .ub_ppc_stop("These columns are not in ", s$results_file, ": ", .ub_ppc_list(bad), " (setting columns). Its columns are: ",
                 .ub_ppc_list(names(lab)))
  r <- as.data.frame(lapply(used, function(v) trimws(as.character(lab[[v]]))), stringsAsFactors = FALSE,
                     col.names = names(used))
  r$lab_row <- seq_len(nrow(r)) + 1L
  keep <- if (length(used)) Reduce(`|`, lapply(names(used), function(k) nzchar(r[[k]]))) else logical(nrow(r))
  r <- r[keep, , drop = FALSE]
  rownames(r) <- NULL
  for (k in setdiff(.UB_PPC_COLUMNS, names(r))) r[[k]] <- rep("", nrow(r))
  st$res$n_lab <- nrow(r)
  say("Results file: ", nrow(r), " rows.  PC: ", nrow(pc), " records.")

  # ---- row-level checks (first reason wins, in the order of the stops below) ----
  reason <- rep(NA_character_, nrow(r)); setting <- reason; cat_ <- reason
  flag <- function(mask, category, why, what) {
    m <- is.na(reason) & !is.na(mask) & mask
    reason[m] <<- why; setting[m] <<- what; cat_[m] <<- category
  }
  pc_codes <- if ("PCTESTCD" %in% names(pc)) sort(unique(as.character(pc$PCTESTCD))) else character()
  if (multi) {
    if (!"PCTESTCD" %in% names(pc))
      .ub_ppc_stop("PC has no PCTESTCD, so results of several analytes cannot be matched to it.")
    r$PCTESTCD <- unname(s$analytes[r$analyte])
    flag(!r$analyte %in% names(s$analytes), "noa", "analyte not listed in the setting analytes", "analytes")
    flag(!r$PCTESTCD %in% pc_codes, "nop", "the analyte code given for it in the setting analytes is not in PC.PCTESTCD", "analytes")
  } else if (nzchar(col[["analyte"]]) && length(unique(r$analyte)) > 1) {
    .ub_ppc_stop("The results file has several analytes (", .ub_ppc_list(sort(unique(r$analyte))), "). Fill in ",
                 "the setting analytes (and leave pctestcd/pctest empty).")
  }

  ids <- unique(pc$USUBJID)
  us <- unique(r$subject)
  um <- vapply(us, function(v) {
    if (v %in% ids) return(v)
    hit <- ids[endsWith(ids, v)]
    if (length(hit) == 1) hit else NA_character_
  }, "", USE.NAMES = FALSE)
  r$USUBJID <- um[match(r$subject, us)]
  flag(is.na(r$USUBJID), "subject", "subject not in PC.USUBJID (or it matches several subjects)",
       "columns: subject (or correct the subject ID in the results file)")

  r$VISIT <- ifelse(r$visit %in% names(s$visit_map), unname(s$visit_map[r$visit]), r$visit)
  flag(!r$VISIT %in% pc_visits, "visit", "visit not in PC.VISIT", "visit_map")

  r$T <- round(ifelse(r$timepoint_name %in% names(s$time_map), unname(s$time_map[r$timepoint_name]),
                      suppressWarnings(as.numeric(r$nominal_time))), 6)
  flag(is.na(r$T), "time", "nominal time is not a number", "columns: nominal_time, or time_map")

  r$key <- .ub_ppc_key(r$USUBJID, r$VISIT, r$T, if (multi) r$PCTESTCD)
  pc_key <- .ub_ppc_key(pc$USUBJID, pc$VISIT, pcT, if (multi) as.character(pc$PCTESTCD))
  ok_so_far <- is.na(reason)
  dk <- r$key[ok_so_far]
  dupk <- unique(dk[duplicated(dk)])
  flag(r$key %in% dupk, "dup", "another result has the same subject + visit + nominal time", "time_map")
  pc_dup <- anyDuplicated(pc_key) > 0
  flag(!r$key %in% pc_key, "nopc", "no PC record with this subject, visit and nominal time", "visit_map or time_map")
  matched <- is.na(reason)
  st$res$n_matched <- sum(matched)
  r$pc_row <- ifelse(matched, match(r$key, pc_key), NA_integer_)
  flag(matched & !(.ub_ppc_is_bql(r$result) | .ub_ppc_is_num(r$result)), "result",
       "result is neither a number nor BQL<(x) / BLQ<(x)", "correct the value in the results file")

  bad_r <- !is.na(reason)
  st$res$unmatched <- data.frame(lab_row = r$lab_row[bad_r], subject = r$subject[bad_r], visit = r$visit[bad_r],
                                 nominal_time = r$nominal_time[bad_r], timepoint_name = r$timepoint_name[bad_r],
                                 analyte = r$analyte[bad_r], result = r$result[bad_r], reason = reason[bad_r],
                                 setting_to_change = setting[bad_r], stringsAsFactors = FALSE)
  tcat <- cat_ %in% c("time", "dup", "nopc")
  st$res$unmatched_values <- list(
    visits = sort(unique(r$VISIT[cat_ %in% "visit"])),
    subjects = sort(unique(r$subject[cat_ %in% "subject"])),
    timepoints = sort(unique(if (nzchar(col[["timepoint_name"]])) r$timepoint_name[tcat] else r$nominal_time[tcat])))

  # PC records without a result (meaningful only when PC has one record per key)
  if (!pc_dup) {
    nr <- which(!pc_key %in% r$key[matched])
    pcstat <- if ("PCSTAT" %in% names(pc)) trimws(as.character(pc$PCSTAT[nr])) else rep("", length(nr))
    gv <- function(v) if (v %in% names(pc)) as.character(pc[[v]][nr]) else rep("", length(nr))
    st$res$no_result <- data.frame(USUBJID = pc$USUBJID[nr], VISIT = pc$VISIT[nr], PCTPT = gv("PCTPT"),
                                   PCTPTNUM = pcT[nr], PCSTAT = pcstat,
                                   status = ifelse(pcstat == "NOT DONE", "ok", "amber"), stringsAsFactors = FALSE)
  }

  # ---- stops, in the order of the original procedure ----
  if (any(cat_ %in% "noa"))
    .ub_ppc_stop("Analytes of the results file missing from the setting analytes: ", .ub_ppc_list(sort(unique(r$analyte[cat_ %in% "noa"]))))
  if (any(cat_ %in% "nop"))
    .ub_ppc_stop("PCTESTCD ", .ub_ppc_list(sort(unique(r$PCTESTCD[cat_ %in% "nop"]))), " from the setting analytes is not in PC ",
                 "(PC has ", .ub_ppc_list(pc_codes), ").")
  if (any(cat_ %in% "subject")) {
    nos <- st$res$unmatched_values$subjects
    .ub_ppc_stop(length(nos), " subject ID(s) of the results file are not in PC.USUBJID: ", .ub_ppc_list(utils::head(nos, 10)),
                 ". PC has e.g. ", .ub_ppc_list(utils::head(sort(ids), 5)), ". Check the setting columns: subject.")
  }
  if (any(cat_ %in% "visit"))
    .ub_ppc_stop("These visits of the results file are not in PC.VISIT: ", .ub_ppc_list(st$res$unmatched_values$visits),
                 ". Add them to visit_map. PC visits are: ", .ub_ppc_list(pc_visits))
  if (any(cat_ %in% "time"))
    .ub_ppc_stop("Nominal times that are not numbers: ", .ub_ppc_list(utils::head(sort(unique(r$nominal_time[cat_ %in% "time"])), 10)),
                 ". Check the setting columns: nominal_time, or add the time points to time_map.")
  if (any(cat_ %in% "dup")) {
    d <- r[cat_ %in% "dup", , drop = FALSE]
    show <- data.frame(USUBJID = d$USUBJID, VISIT = d$VISIT, hours = d$T, stringsAsFactors = FALSE)
    if (multi) show$PCTESTCD <- d$PCTESTCD
    if (nzchar(col[["timepoint_name"]])) show$timepoint_name <- d$timepoint_name
    .ub_ppc_stop(nrow(d), " results rows share the same subject + visit + nominal time, e.g.:\n", .ub_ppc_show(show, 6),
                 "\nUsually a predose sample that belongs to the previous dose: add it to time_map.")
  }
  if (pc_dup)
    .ub_ppc_stop("PC has several records with the same USUBJID + VISIT + PCTPTNUM", if (multi) " + PCTESTCD",
                 "; they cannot be matched.",
                 if (multi) "" else " If PC has a record per analyte, fill in the setting analytes instead of pctestcd/pctest.")
  say("Matched: ", sum(matched), " of ", nrow(r), " results.")
  if (any(cat_ %in% "nopc")) {
    d <- r[cat_ %in% "nopc", , drop = FALSE]
    show <- data.frame(USUBJID = d$USUBJID, VISIT = d$VISIT, hours = d$T, stringsAsFactors = FALSE)
    if (multi) show$PCTESTCD <- d$PCTESTCD
    if (nzchar(col[["timepoint_name"]])) show$timepoint_name <- d$timepoint_name
    .ub_ppc_stop(nrow(d), " result(s) have no PC record with the same subject, visit and nominal time:\n", .ub_ppc_show(show, 15),
                 "\nCheck visit_map and time_map (PC.PCTPT / PCTPTNUM show what PC expects).")
  }
  nr <- st$res$no_result
  say("PC records without a result: ", nrow(nr), " (", sum(nr$PCSTAT == "NOT DONE"), " marked NOT DONE).")
  odd <- nr[nr$status == "amber", , drop = FALSE]
  if (nrow(odd)) {
    say("WARNING: ", nrow(odd), " PC record(s) have no result and are not marked NOT DONE (ask the data manager):")
    say(.ub_ppc_show(odd[, c("USUBJID", "VISIT", "PCTPT")], 1e6))
  }
  if (any(cat_ %in% "result"))
    .ub_ppc_stop(sum(cat_ %in% "result"), " result(s) are neither a number nor BQL<(x) / BLQ<(x): ",
                 .ub_ppc_list(utils::head(sort(unique(r$result[cat_ %in% "result"])), 10)), ". Correct them in the results file.")

  # ---- fill ----
  b <- r[matched, , drop = FALSE]
  idx <- b$pc_row
  is_bql <- .ub_ppc_is_bql(b$result); is_num <- .ub_ppc_is_num(b$result)
  fill <- list(PCORRES = b$result, PCORRESU = b$unit, PCSTRESC = b$result, PCSTRESU = b$unit,
               PCSTRESN = ifelse(is_num, suppressWarnings(as.numeric(b$result)), NA_real_))
  if (nzchar(col[["lloq"]])) fill$PCLLOQ <- suppressWarnings(as.numeric(b$lloq))
  if (!nzchar(col[["unit"]])) fill$PCORRESU <- fill$PCSTRESU <- NULL     # no unit column: PC keeps its units
  out <- pc
  conflicts <- st$res$conflicts
  for (var in names(fill)) {
    new <- fill[[var]]
    numeric <- var %in% c("PCSTRESN", "PCLLOQ")
    if (!var %in% names(out)) {
      out[[var]] <- if (numeric) rep(NA_real_, nrow(out)) else rep("", nrow(out))
      meta$labels[var] <- ""; meta$formats[var] <- ""
    }
    if (numeric && is.character(out[[var]]) && all(!nzchar(trimws(out[[var]]))))
      out[[var]] <- rep(NA_real_, nrow(out))
    cur <- out[[var]][idx]
    curc <- trimws(as.character(cur))
    empty <- is.na(cur) | is.na(curc) | curc == ""
    differ <- if (numeric) !empty & abs(suppressWarnings(as.numeric(cur)) - new) > 1e-9
              else !empty & nzchar(trimws(as.character(new))) & curc != trimws(as.character(new))
    differ[is.na(differ)] <- FALSE
    if (any(differ)) {
      say("WARNING: ", var, ": ", sum(differ), " value(s) already in PC differ from the results file; PC kept.")
      conflicts <- rbind(conflicts, data.frame(USUBJID = b$USUBJID[differ], VISIT = b$VISIT[differ], PCTPTNUM = b$T[differ],
                                               variable = var, pc_value = curc[differ],
                                               lab_value = trimws(as.character(new[differ])), stringsAsFactors = FALSE))
    }
    out[[var]][idx[empty]] <- new[empty]
  }
  st$res$conflicts <- conflicts
  old <- if ("PCTESTCD" %in% names(out)) sort(unique(as.character(out$PCTESTCD))) else character()
  say("Filled: ", sum(is_num), " numeric results and ", sum(is_bql), " below the LLOQ.")
  if (multi) {
    old <- character()
    a <- s$analytes[order(names(s$analytes))]
    say("Analytes: ", paste0(names(a), " = ", a, collapse = ", "), " (PCTESTCD/PCTEST as in PC).")
  } else {
    if (!"PCTESTCD" %in% names(out)) { meta$labels["PCTESTCD"] <- ""; meta$formats["PCTESTCD"] <- "" }
    if (!"PCTEST" %in% names(out)) { meta$labels["PCTEST"] <- ""; meta$formats["PCTEST"] <- "" }
    out$PCTESTCD <- rep(s$pctestcd, nrow(out))
    out$PCTEST <- rep(s$pctest, nrow(out))
    say("PCTESTCD/PCTEST set to ", s$pctestcd, "/", s$pctest, " on all ", nrow(out), " PC records",
        if (length(old) && !identical(old, s$pctestcd)) paste0(" (was ", .ub_ppc_list(old), ").") else ".")
  }
  if (nzchar(col[["specimen"]]) && "PCSPEC" %in% names(out)) {
    pairs <- unique(data.frame(PCSPEC = as.character(out$PCSPEC[idx]), results_file = b$specimen, stringsAsFactors = FALSE))
    pairs <- pairs[order(pairs$PCSPEC, pairs$results_file), , drop = FALSE]
    say("Specimen, PC.PCSPEC vs results file (PC kept): ", paste0("(", pairs$PCSPEC, ", ", pairs$results_file, ")", collapse = ", "))
  }

  if (nzchar(s$check_dataset)) .ub_ppc_check(folder, s$check_dataset, out, idx, say, multi, st)

  if (write) {
    ub_write_dataset(out, pc_path, meta, name = "PC")
    say("Written: ", pc_path)
    if (nzchar(in_name) && grepl("\\.sas7bdat$", in_name, ignore.case = TRUE)) {
      file.remove(file.path(in_dir, in_name))
      say("The input ", in_name, " was replaced by ", basename(pc_path), " (the original is kept in ", orig_dir, ").")
    }
    st$res$written <- TRUE
    say("")
    say("NEXT: blind the study folder.")
  } else {
    say("Check only: nothing was written.")
  }
  if (!multi && length(old) && !identical(old, s$pctestcd))
    say("If the blinding then stops on the codebook because of the old analyte code ", .ub_ppc_list(old),
        ", delete its rows from private/codebook.csv and run it again.")
  invisible(TRUE)
}

.ub_ppc_analyte_hint <- function(folder, s) {
  if (!nzchar(s$check_dataset)) return("")
  p <- file.path(folder, "input", s$check_dataset)
  if (!file.exists(p)) return("")
  a <- tryCatch(ub_read_dataset(p)$data, error = function(e) NULL)
  if (is.null(a) || !"PARAMCD" %in% names(a)) return("")
  codes <- setdiff(sort(unique(as.character(a$PARAMCD))), "")
  if (length(codes)) paste0(" ", s$check_dataset, " uses PARAMCD ", .ub_ppc_list(codes), ".") else ""
}

# Filled results against AVAL of an analysis dataset (same subject, visit and time point; BQL = AVAL 0)
.ub_ppc_check <- function(folder, name, out, idx, say, multi, st) {
  p <- file.path(folder, "input", name)
  if (!file.exists(p)) { say("Cross-check skipped: ", name, " is not in the input folder."); return(invisible()) }
  a <- ub_read_dataset(p)$data
  need <- c("USUBJID", "VISIT", "PCTPT", "AVAL")
  if (!all(need %in% names(a)) || !"PCTPT" %in% names(out)) {
    miss <- setdiff(need, names(a))
    say("Cross-check skipped: ", name, " lacks ", .ub_ppc_list(if (length(miss)) miss else "PCTPT in PC"), ".")
    return(invisible())
  }
  on <- c("USUBJID", "VISIT", "PCTPT")
  if (multi) {
    acode <- if ("PCTESTCD" %in% names(a)) "PCTESTCD" else "PARAMCD"
    if (!acode %in% names(a) || !length(intersect(as.character(a[[acode]]), as.character(out$PCTESTCD)))) {
      say("Cross-check skipped: ", name, " has no analyte codes equal to PC.PCTESTCD.")
      return(invisible())
    }
    if (acode != "PCTESTCD") a$PCTESTCD <- a[[acode]]
    on <- c(on, "PCTESTCD")
  }
  ak <- do.call(paste, c(lapply(on, function(v) as.character(a[[v]])), sep = "\r"))
  if (anyDuplicated(ak)) {
    say("Cross-check skipped: ", name, " has several records per ", paste(on, collapse = " + "), ".")
    return(invisible())
  }
  x <- out[idx, on, drop = FALSE]
  for (v in on) x[[v]] <- as.character(x[[v]])
  xk <- do.call(paste, c(lapply(on, function(v) x[[v]]), sep = "\r"))
  x$pc_value <- as.numeric(out$PCSTRESN[idx])
  x$aval <- suppressWarnings(as.numeric(a$AVAL))[match(xk, ak)]
  expv <- ifelse(is.na(x$pc_value), 0, x$pc_value)
  ok <- !is.na(x$aval) & abs(x$aval - expv) < 1e-6
  x$status <- ifelse(ok, "ok", "red")
  rownames(x) <- NULL
  st$res$aval_check <- x
  if (all(ok)) {
    say("Cross-check: all ", nrow(x), " results equal ", name, " AVAL.")
  } else {
    say("WARNING: cross-check: ", sum(!ok), " of ", nrow(x), " results differ from ", name, " AVAL (or have no ", name,
        " record), e.g.:\n", .ub_ppc_show(x[!ok, setdiff(names(x), "status"), drop = FALSE], 10))
  }
  invisible()
}
