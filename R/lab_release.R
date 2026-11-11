# Workflow 3: the bioanalytical lab results file cannot be merged into PC (times don't line up, rows missing on one
# side), so it is released blinded next to PC instead. Before the run, the lab file becomes LABPC: a PC-shaped table
# (USUBJID, PCTESTCD, VISIT, PCTPTNUM, PCORRES/PCSTRESN, PCLLOQ, ...) with the lab row number, the PC record it matches
# and the lab's own key texts. The run blinds LABPC like PC (same key, same factors); the released file is turned back
# into the lab's own layout (see .ub_lab_layout).

.UB_MATCH_STATUS <- c(matched = "matched", no_pc = "no PC record at this visit/time", several = "several PC records")

# lab role -> LABPC column carrying what is released for it
.UB_LAB_ROLE_COL <- c(subject = "USUBJID", visit = "LVISIT", nominal_time = "LTIME", result = "PCORRES", unit = "PCSTRESU",
                      lloq = "PCLLOQ", timepoint_name = "LTPT", analyte = "PCTESTCD", specimen = "LSPEC")

.ub_lab_wrap <- function(expr) tryCatch(expr, ub_ppc_stop = function(e) ub_error(conditionMessage(e)))

# A kept lab column that holds dates: every non-empty value parses with one format (English month abbreviations are
# understood whatever the locale). Returns list(is_date, iso) with ISO 8601 text (a date, or a date and time).
.ub_lab_dates <- function(x, name = "") {
  v <- trimws(as.character(x)); v[is.na(v)] <- ""
  has <- nzchar(v)
  no <- list(is_date = FALSE, iso = v)
  if (!any(has)) return(no)
  w <- v[has]
  for (i in 1:12) w <- gsub(paste0("(?i)(?<![A-Za-z])", month.abb[i], "(?![A-Za-z])"), sprintf("%02d", i), w, perl = TRUE)
  forms <- list(
    list(rx = "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}$", fmt = "%Y-%m-%dT%H:%M:%S", out = "%Y-%m-%dT%H:%M:%S"),
    list(rx = "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}$", fmt = "%Y-%m-%dT%H:%M", out = "%Y-%m-%dT%H:%M"),
    list(rx = "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}(:\\d{2})?$", fmt = "%Y-%m-%d %H:%M", out = "%Y-%m-%dT%H:%M"),
    list(rx = "^\\d{4}-\\d{2}-\\d{2}$", fmt = "%Y-%m-%d", out = "%Y-%m-%d"),
    list(rx = "^\\d{1,2}-\\d{2}-\\d{4}$", fmt = "%d-%m-%Y", out = "%Y-%m-%d"),
    list(rx = "^\\d{2}\\d{2}\\d{4}$", fmt = "%d%m%Y", out = "%Y-%m-%d"),
    list(rx = "^\\d{1,2}/\\d{1,2}/\\d{4}$", fmt = "%d/%m/%Y", out = "%Y-%m-%d"),
    list(rx = "^\\d{1,2}/\\d{1,2}/\\d{4}$", fmt = "%m/%d/%Y", out = "%Y-%m-%d"))
  for (f in forms) {
    if (!all(grepl(f$rx, w))) next
    p <- as.POSIXct(strptime(w, f$fmt, tz = "UTC"))
    if (anyNA(p)) next
    iso <- v; iso[has] <- format(p, f$out, tz = "UTC")
    return(list(is_date = TRUE, iso = iso))
  }
  no
}

#' Prepare the lab results file for release (Workflow 3), or check how it matches PC
#'
#' Reads the lab results file named in the Prepare PC settings (`prepare_pc`: results file, sheet, column roles,
#' analytes, visit and time translations), resolves each row to PC (subject, analyte, visit, nominal time, specimen)
#' and builds LABPC, the PC-shaped table the run blinds. Rows whose subject is not in PC (or whose analyte is unknown)
#' are left out and reported. `lab_release$keep_columns` names extra lab columns to release (dates are converted to
#' ISO 8601 so that they are shifted like PC's dates).
#' @param folder the UB study folder
#' @param options study settings (default: `blind_options.json`)
#' @param write `TRUE`: write `private/lab/labpc.xpt`, `private/lab/labpc_layout.json` and
#'   `private/reports/lab_release_report.csv/.html`; `FALSE` (Check matching): write nothing
#' @return list(labpc, layout, counts = c(rows, matched, no_pc, several, left_out), left_out, lab_file, lab_md5)
#' @export
ub_lab_release_prepare <- function(folder, options = ub_read_options(folder), write = TRUE) {
  folder <- normalizePath(folder, mustWork = FALSE)
  s <- .ub_lab_wrap(.ub_ppc_settings(options$prepare_pc %||% list()))
  col <- s$columns
  if (!nzchar(s$results_file))
    ub_error("lab_release is on but prepare_pc.results_file is empty: choose the lab results file in the Prepare PC tab.")
  miss <- .UB_PPC_REQUIRED[!nzchar(col[.UB_PPC_REQUIRED])]
  if (length(miss)) ub_error("lab_release: give the lab file's column for ", paste(miss, collapse = ", "),
                             " (Prepare PC tab, column roles).")
  multi <- length(s$analytes) > 0
  if (multi && !nzchar(col[["analyte"]]))
    ub_error("lab_release: the setting analytes is filled in, so give the analyte column too (Prepare PC tab).")
  if (!multi && !nzchar(s$pctestcd))
    ub_error("lab_release: give the analyte code (pctestcd) or, for several analytes, the setting analytes (Prepare PC tab).")
  path <- file.path(folder, "input", s$results_file)
  if (!file.exists(path)) ub_error(path, " not found. Copy the lab results file into the input folder.")
  lab <- .ub_lab_wrap(.ub_ppc_read_lab(path, s$sheet))
  names(lab) <- trimws(names(lab))
  roles <- col[nzchar(col)]
  keep <- trimws(as.character(unlist(options$lab_release$keep_columns)))
  keep <- unique(keep[nzchar(keep)])
  for (k in keep) {
    if (!k %in% names(lab)) ub_error("keep_columns: ", k, " is not a column of ", s$results_file, " (its columns are: ",
                                     paste(names(lab), collapse = ", "), ").")
    if (k %in% roles) ub_error("keep_columns: ", k, " is already a role column (", names(roles)[roles == k][1],
                               "); it is released anyway.")
  }
  pcn <- .ub_lab_wrap(.ub_ppc_find_pc(file.path(folder, "input"), "The input folder"))
  if (!nzchar(pcn)) ub_error("There is no PC (pc.xpt or pc.sas7bdat) in the input folder: Workflow 3 matches the lab file to it.")
  pc <- ub_read_dataset(file.path(folder, "input", pcn))$data
  for (v in c("USUBJID", "VISIT", "PCTPTNUM", "PCTESTCD"))
    if (!v %in% names(pc)) ub_error("PC has no ", v, "; it is needed to match the lab results.")
  r <- .ub_lab_wrap(.ub_ppc_resolve(lab, pc, s, use_specimen = TRUE))
  pc_codes <- unique(as.character(pc$PCTESTCD))
  if (!multi) r$PCTESTCD <- rep(s$pctestcd, nrow(r))

  # rows that cannot be released: the subject or the analyte is not PC's
  out_reason <- ifelse(r$category %in% c("subject", "noa", "nop"), r$reason, NA_character_)
  out_reason[is.na(out_reason) & !r$PCTESTCD %in% pc_codes] <- "the analyte code is not in PC.PCTESTCD"
  out <- !is.na(out_reason)
  left_out <- data.frame(lab_row = r$lab_row[out], subject = r$subject[out], analyte = r$analyte[out],
                         reason = out_reason[out], stringsAsFactors = FALSE)
  rr <- r[!out, , drop = FALSE]

  # PC's name of each analyte code
  pc_test <- if ("PCTEST" %in% names(pc)) as.character(pc$PCTEST) else rep("", nrow(pc))
  first_name <- vapply(pc_codes, function(cd) { x <- pc_test[pc$PCTESTCD == cd & nzchar(pc_test)]; if (length(x)) x[1] else "" }, "")
  analyte_is_name <- nzchar(col[["analyte"]]) && any(!toupper(trimws(rr$analyte)) %in% toupper(pc_codes))
  status <- ifelse(rr$n_pc == 1L, .UB_MATCH_STATUS[["matched"]],
                   ifelse(rr$n_pc == 0L, .UB_MATCH_STATUS[["no_pc"]], .UB_MATCH_STATUS[["several"]]))
  num <- .ub_ppc_is_num(rr$result) & !.ub_ppc_is_bql(rr$result)
  L <- data.frame(STUDYID = if ("STUDYID" %in% names(pc)) as.character(pc$STUDYID[1]) else "",
                  USUBJID = rr$USUBJID, PCTESTCD = rr$PCTESTCD, PCTEST = unname(first_name[rr$PCTESTCD]),
                  VISIT = rr$VISIT, PCTPTNUM = rr$T, PCTPT = rr$timepoint_name, PCSPEC = rr$specimen,
                  PCORRES = rr$result, PCSTRESC = rr$result,
                  PCSTRESN = ifelse(num, suppressWarnings(as.numeric(rr$result)), NA_real_), PCSTRESU = rr$unit,
                  PCLLOQ = suppressWarnings(as.numeric(rr$lloq)),
                  LABROW = as.integer(rr$lab_row), PCSRCROW = ifelse(rr$n_pc == 1L, as.integer(rr$pc_row), NA_integer_),
                  MATCHST = status, LVISIT = rr$visit, LTIME = rr$nominal_time, stringsAsFactors = FALSE)
  if (nzchar(col[["timepoint_name"]])) L$LTPT <- rr$timepoint_name
  if (nzchar(col[["specimen"]])) L$LSPEC <- rr$specimen

  # kept columns: K01, K02, ... (dates: K01DTC, ... in ISO 8601, so that they are shifted like PC's dates)
  kept <- data.frame(lab_name = character(), labpc_name = character(), kind = character(), stringsAsFactors = FALSE)
  for (i in seq_along(keep)) {
    vals <- as.character(lab[[keep[i]]][rr$lab_row - 1L])
    d <- .ub_lab_dates(vals, keep[i])
    nm <- sprintf("K%02d%s", i, if (d$is_date) "DTC" else "")
    L[[nm]] <- d$iso
    kept <- rbind(kept, data.frame(lab_name = keep[i], labpc_name = nm, kind = if (d$is_date) "kept_date" else "kept_text",
                                   stringsAsFactors = FALSE))
  }
  role_cols <- data.frame(lab_name = unname(roles), role = names(roles), stringsAsFactors = FALSE)
  role_cols$labpc_name <- unname(.UB_LAB_ROLE_COL[role_cols$role])
  if (analyte_is_name) role_cols$labpc_name[role_cols$role == "analyte"] <- "PCTEST"
  role_cols$kind <- "role"
  kept$role <- rep("", nrow(kept))
  cols <- rbind(role_cols, kept[, names(role_cols)])
  cols <- cols[order(match(cols$lab_name, names(lab))), , drop = FALSE]
  rownames(cols) <- NULL
  lab_md5 <- .ub_md5(path)
  layout <- list(lab_file = s$results_file, sheet = s$sheet, lab_md5 = lab_md5, analyte_is_name = analyte_is_name,
                 columns = cols)
  counts <- c(rows = nrow(L), matched = sum(status == .UB_MATCH_STATUS[["matched"]]),
              no_pc = sum(status == .UB_MATCH_STATUS[["no_pc"]]), several = sum(status == .UB_MATCH_STATUS[["several"]]),
              left_out = nrow(left_out))

  if (write) {
    d <- file.path(folder, "private", "lab")
    ub_write_dataset(L, file.path(d, "labpc.xpt"), name = "LABPC")
    jsonlite::write_json(layout, file.path(d, "labpc_layout.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
    rp <- file.path(folder, "private", "reports", "lab_release_report.csv")
    dir.create(dirname(rp), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(left_out, rp, row.names = FALSE, fileEncoding = "UTF-8")
    ub_report_html(left_out, rep("red", nrow(left_out)), sub("csv$", "html", rp),
                   title = "Lab file release: rows left out",
                   intro = sprintf("%s: %d row(s) released (%d matched to PC, %d without a PC record, %d with several), %d left out.",
                                   s$results_file, counts[["rows"]], counts[["matched"]], counts[["no_pc"]],
                                   counts[["several"]], counts[["left_out"]]))
  }
  list(labpc = L, layout = layout, counts = counts, left_out = left_out, lab_file = s$results_file, lab_md5 = lab_md5)
}

# ---- the released file: the lab's own layout ----------------------------------------------------------------------
.ub_lab_layout_read <- function(study) {
  lay <- jsonlite::fromJSON(file.path(dirname(ub_source_path(study, "LABPC")), "labpc_layout.json"))
  lay$columns <- as.data.frame(lay$columns, stringsAsFactors = FALSE)
  lay
}

# private/blinded/<lab file stem>_blinded.csv
.ub_lab_layout_path <- function(study, lay = .ub_lab_layout_read(study))
  file.path(study$paths$blinded_dir, paste0(tools::file_path_sans_ext(basename(lay$lab_file)), "_blinded.csv"))

# The blinded LABPC in the lab's layout: UBRECID, LAB_ROW, the lab's role and kept columns (lab names, lab order),
# PC_UBRECID (the matched record of the PC released with it; "" when none) and MATCH_STATUS. All columns are text.
.ub_lab_layout <- function(bl, pc_xw, lay) {
  chr <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }
  out <- data.frame(UBRECID = chr(bl[[UB_RECID_VAR]]), LAB_ROW = chr(as.integer(bl$LABROW)), stringsAsFactors = FALSE)
  cols <- lay$columns
  for (i in seq_len(nrow(cols))) out[[cols$lab_name[i]]] <- chr(bl[[cols$labpc_name[i]]])
  pr <- suppressWarnings(as.integer(bl$PCSRCROW))
  out$PC_UBRECID <- chr(if (is.null(pc_xw)) rep(NA_character_, nrow(bl)) else pc_xw$UBRECID[match(pr, pc_xw$SRC_ROW)])
  out$MATCH_STATUS <- chr(bl$MATCHST)
  out
}

.ub_lab_read_out <- function(path) utils::read.csv(path, colClasses = "character", check.names = FALSE,
                                                   na.strings = character(), fileEncoding = "UTF-8")

# blind step: write the lab-layout file next to the blinded datasets; returns what crosswalk.json records about it
.ub_lab_write_layout <- function(study, pc_xw) {
  lay <- .ub_lab_layout_read(study)
  bl <- ub_read_dataset(ub_blinded_path(study, "LABPC"))$data
  out <- .ub_lab_layout(bl, pc_xw, lay)
  p <- .ub_lab_layout_path(study, lay)
  utils::write.csv(out, p, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  list(layout_file = basename(p), layout_md5 = .ub_md5(p), lab_file = lay$lab_file, lab_md5 = lay$lab_md5)
}

# qc step: the lab's column names are released too; and warn when most lab rows matched no PC record
.ub_lab_qc <- function(study, df, name_terms, alnum, add) {
  lay <- .ub_lab_layout_read(study)
  for (c in lay$columns$lab_name)
    if (any(vapply(name_terms, function(x) grepl(x, alnum(c), fixed = TRUE), logical(1))))
      add("FAIL", "identity_in_varname", "LABPC", c, paste0("lab column name ", c, " contains an identity fragment"),
          "a variable name contains an identity fragment")
  n <- nrow(df); m <- sum(df$MATCHST == .UB_MATCH_STATUS[["matched"]])
  if (n && m / n < 0.5) {
    msg <- sprintf("%d of %d lab rows matched a PC record", m, n)
    add("WARN", "lab_match_rate", "LABPC", "MATCHST", paste0(msg, ": check the visit and time translations (Prepare PC)"), msg)
  }
}

# verify step: the released lab file is exactly the blinded LABPC in the lab's layout, linked to the released PC
.ub_lab_verify <- function(study, bl, cw) {
  lay <- .ub_lab_layout_read(study)
  p <- .ub_lab_layout_path(study, lay)
  if (!file.exists(p)) return("the lab file in its own layout is missing")
  notes <- character()
  if (!identical(.ub_md5(p), cw$info$LABPC$layout_md5)) notes <- c(notes, "the lab file changed after the blind step")
  re <- .ub_lab_layout(bl, cw$xw$PC, lay)
  got <- .ub_lab_read_out(p)
  if (!identical(names(re), names(got))) notes <- c(notes, "the lab file's columns differ from a re-run of its layout")
  else {
    bad <- names(re)[!vapply(names(re), function(c) identical(re[[c]], got[[c]]), logical(1))]
    if (length(bad)) notes <- c(notes, paste("the lab file differs from a re-run of its layout in", paste(bad, collapse = ", ")))
  }
  ids <- got$PC_UBRECID[nzchar(got$PC_UBRECID)]
  if (length(ids) && !all(ids %in% cw$xw$PC$UBRECID)) notes <- c(notes, "the lab file links to records that are not in the released PC")
  notes
}

# release step: the manifest line about the lab file
.ub_lab_manifest_line <- function(study) {
  m <- ub_read_dataset(ub_blinded_path(study, "LABPC"))$data$MATCHST
  k <- sum(m == .UB_MATCH_STATUS[["matched"]])
  sprintf("Lab file: %d rows released, %d matched to PC, %d not matched (MATCH_STATUS; PC_UBRECID links a row to its PC record).",
          length(m), k, length(m) - k)
}
