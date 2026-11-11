# Codebook handling for the app (read with a status per row, save with backup, highlighted HTML/xlsx copies,
# import of decisions made in Excel) and a generic HTML report with coloured rows.
# All files written here go to <UB folder>/private: they hold real names.

.UB_CB_COLUMNS <- c("TYPE", "ACTUAL_VALUE", "BLINDED_VALUE", "NOTE", "CHECK")
.UB_CB_EDITABLE <- c("BLINDED_VALUE", "NOTE", "CHECK")
.UB_COLOURS <- c(red = "#f8d7da", amber = "#fff3cd", ok = "#ffffff", info = "#e7f1ff")
.UB_COLOUR_TEXT <- c(red = "must fix", amber = "needs your decision", ok = "OK", info = "for information")

.ub_codebook_path <- function(ub_dir) file.path(ub_dir, "private", "codebook.csv")

#' Read the codebook with a review status per row
#'
#' Status: `red` = the run stops on it (BLINDED_VALUE `?`, or a blank TYPE/ACTUAL_VALUE), `amber` = the CHECK column
#' asks for a decision, `ok` otherwise.
#' @param ub_dir the UB folder
#' @return data.frame with TYPE, ACTUAL_VALUE, BLINDED_VALUE, NOTE, CHECK, .status, .row
#' @export
ub_codebook_read <- function(ub_dir) {
  p <- .ub_codebook_path(ub_dir)
  if (!file.exists(p)) return(NULL)
  t <- utils::read.csv(p, colClasses = "character", na.strings = character(), check.names = FALSE,
                       fileEncoding = "UTF-8-BOM")
  names(t) <- toupper(trimws(names(t)))
  for (c in .UB_CB_COLUMNS) if (is.null(t[[c]])) t[[c]] <- rep("", nrow(t))
  t <- t[, .UB_CB_COLUMNS, drop = FALSE]
  for (c in .UB_CB_COLUMNS) t[[c]] <- trimws(t[[c]])
  t$.status <- ub_codebook_status(t)
  t$.row <- seq_len(nrow(t))
  t
}

#' Review status of codebook rows (red / amber / ok)
#' @param t codebook data.frame
#' @export
ub_codebook_status <- function(t) {
  red <- t$BLINDED_VALUE == "?" | (!nzchar(t$TYPE) & nzchar(t$ACTUAL_VALUE))
  amber <- nzchar(t$CHECK)
  ifelse(red, "red", ifelse(amber, "amber", "ok"))
}

#' Save the codebook, backing up the previous version, and refresh its HTML/xlsx copies
#' @param ub_dir the UB folder
#' @param t codebook data.frame (extra columns starting with "." are dropped)
#' @export
ub_codebook_write <- function(ub_dir, t) {
  p <- .ub_codebook_path(ub_dir)
  if (file.exists(p)) {
    b <- file.path(ub_dir, "private", "codebook_backup")
    dir.create(b, recursive = TRUE, showWarnings = FALSE)
    file.copy(p, file.path(b, paste0("codebook_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")))
  }
  utils::write.csv(t[, .UB_CB_COLUMNS, drop = FALSE], p, row.names = FALSE, fileEncoding = "UTF-8")
  ub_codebook_reports(ub_dir)
  invisible(p)
}

# ---- confirmation that a person reviewed the codebook -------------------------------------------------------------
# private/codebook_review.json records who confirmed, when, and a fingerprint of the codes (TYPE, ACTUAL_VALUE,
# BLINDED_VALUE). Editing NOTE or CHECK keeps the confirmation; a changed or added code needs a new one.
.ub_cb_review_path <- function(ub_dir) file.path(ub_dir, "private", "codebook_review.json")

.ub_cb_fingerprint <- function(t) {
  f <- tempfile(fileext = ".txt"); on.exit(unlink(f))
  writeLines(enc2utf8(paste(t$TYPE, t$ACTUAL_VALUE, t$BLINDED_VALUE, sep = "\t")), f, useBytes = TRUE)
  unname(tools::md5sum(f))
}

#' Has a person confirmed the codebook review?
#'
#' @param ub_dir the UB folder
#' @return list(status, reviewer, time, open): status is `none` (no codebook yet), `open` (rows still need an answer),
#'   `unconfirmed` (all rows answered, review not confirmed), `changed` (codes changed since the confirmation) or
#'   `confirmed`.
#' @export
ub_codebook_review_status <- function(ub_dir) {
  t <- ub_codebook_read(ub_dir)
  if (is.null(t)) return(list(status = "none", reviewer = NA_character_, time = NA_character_, open = 0L))
  open <- sum(t$.status != "ok")
  p <- .ub_cb_review_path(ub_dir)
  r <- if (file.exists(p)) tryCatch(jsonlite::fromJSON(p), error = function(e) NULL)
  st <- if (open) "open" else if (is.null(r)) "unconfirmed" else if (!identical(r$fingerprint, .ub_cb_fingerprint(t))) "changed" else "confirmed"
  list(status = st, reviewer = r$reviewer %||% NA_character_, time = r$time %||% NA_character_, open = open)
}

#' Confirm that the codebook was reviewed
#'
#' Allowed only when no row needs an answer (no red or amber rows). The app runs the blinding only after this
#' confirmation; a new or changed code needs a new one.
#' @param ub_dir the UB folder
#' @param reviewer the name of the person who reviewed it
#' @export
ub_codebook_confirm_review <- function(ub_dir, reviewer) {
  reviewer <- trimws(paste(reviewer, collapse = " "))
  if (!nzchar(reviewer)) ub_error("Type the name of the person who reviewed the codebook.")
  t <- ub_codebook_read(ub_dir)
  if (is.null(t)) ub_error("There is no codebook yet: run the blinding once.")
  n <- sum(t$.status != "ok")
  if (n) ub_error(n, " codebook row(s) still need an answer: answer them first.")
  rec <- list(reviewer = reviewer, time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), rows = nrow(t),
              fingerprint = .ub_cb_fingerprint(t))
  jsonlite::write_json(rec, .ub_cb_review_path(ub_dir), auto_unbox = TRUE, pretty = TRUE)
  invisible(rec)
}

.ub_html_escape <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}

#' Write a self-contained HTML table with coloured rows (attention rows first)
#' @param df data.frame to show
#' @param status character vector per row: red / amber / ok / info
#' @param file output path
#' @param title page title
#' @param intro optional paragraph(s) of plain text shown above the table
#' @param sections optional named list of extra data.frames (each with a `.status` column) shown below
#' @export
ub_report_html <- function(df, status, file, title, intro = NULL, sections = NULL) {
  tab <- function(d, st) {
    if (is.null(d) || !nrow(d)) return("<p><em>None.</em></p>")
    d <- d[, !startsWith(names(d), "."), drop = FALSE]
    o <- order(match(st, c("red", "amber", "info", "ok")))
    d <- d[o, , drop = FALSE]; st <- st[o]
    head <- paste0("<tr><th>Status</th>", paste0("<th>", .ub_html_escape(names(d)), "</th>", collapse = ""), "</tr>")
    rows <- vapply(seq_len(nrow(d)), function(i) {
      cells <- paste0("<td>", .ub_html_escape(vapply(d[i, ], function(v) if (is.na(v)) "" else as.character(v), "")),
                      "</td>", collapse = "")
      paste0("<tr class=\"", st[i], "\"><td>", .UB_COLOUR_TEXT[[st[i]]], "</td>", cells, "</tr>")
    }, "")
    paste0("<table>", head, paste(rows, collapse = "\n"), "</table>")
  }
  counts <- function(st) paste0(vapply(c("red", "amber", "ok"), function(s)
    sprintf("<span class=\"badge %s\">%s: %d</span>", s, .UB_COLOUR_TEXT[[s]], sum(st == s)), ""), collapse = " ")
  css <- paste0("body{font-family:Segoe UI,Arial,sans-serif;margin:1.5em;color:#222}",
                "table{border-collapse:collapse;margin:.5em 0 1.5em 0}th,td{border:1px solid #bbb;padding:3px 7px;",
                "text-align:left;vertical-align:top;font-size:13px}th{background:#e9ecef;position:sticky;top:0}",
                sprintf("tr.red td{background:%s}tr.amber td{background:%s}tr.info td{background:%s}",
                        .UB_COLOURS[["red"]], .UB_COLOURS[["amber"]], .UB_COLOURS[["info"]]),
                ".badge{padding:2px 8px;border:1px solid #999;border-radius:8px;margin-right:6px}",
                sprintf(".badge.red{background:%s}.badge.amber{background:%s}", .UB_COLOURS[["red"]], .UB_COLOURS[["amber"]]),
                ".private{color:#842029;font-weight:bold}")
  body <- c(sprintf("<h1>%s</h1>", .ub_html_escape(title)),
            sprintf("<p>Written %s. <span class=\"private\">PRIVATE: contains real values. Never share.</span></p>",
                    format(Sys.time(), "%Y-%m-%d %H:%M")),
            if (length(intro)) paste0("<p>", .ub_html_escape(intro), "</p>"),
            sprintf("<p>%s</p>", counts(status)),
            "<p>Legend: red = must fix; amber = needs your decision; no colour = OK. Rows needing attention are listed first.</p>",
            tab(df, status))
  for (n in names(sections)) {
    s <- sections[[n]]
    st <- if (!is.null(s) && nrow(s) && ".status" %in% names(s)) s$.status else rep("ok", if (is.null(s)) 0 else nrow(s))
    body <- c(body, sprintf("<h2>%s</h2>", .ub_html_escape(n)), if (length(st)) sprintf("<p>%s</p>", counts(st)), tab(s, st))
  }
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  writeLines(c("<!DOCTYPE html><html><head><meta charset=\"utf-8\">",
               sprintf("<title>%s</title><style>%s</style></head><body>", .ub_html_escape(title), css),
               body, "</body></html>"), file, useBytes = TRUE)
  invisible(file)
}

#' Write the codebook review copies (HTML and Excel) to private/reports
#' @param ub_dir the UB folder
#' @return list(html, xlsx) paths (NULL when there is no codebook)
#' @export
ub_codebook_reports <- function(ub_dir) {
  t <- ub_codebook_read(ub_dir)
  if (is.null(t)) return(NULL)
  rep_dir <- file.path(ub_dir, "private", "reports")
  html <- ub_report_html(t, t$.status, file.path(rep_dir, "codebook_review.html"), "Codebook review",
                         intro = paste("Every drug, analyte, arm and other private text the blinding replaces, with its code.",
                                       "Answer the rows with a question in CHECK (amber) and every '?' (red),",
                                       "in the app or in codebook_review.xlsx, then import the decisions."))
  xlsx <- ub_codebook_xlsx(t, file.path(rep_dir, "codebook_review.xlsx"))
  list(html = html, xlsx = xlsx)
}

#' Write the codebook as an Excel file with highlighted rows
#'
#' Columns BLINDED_VALUE, NOTE and CHECK (clear it when reviewed) are editable; the others are locked (the sheet is protected without a password,
#' filtering and sorting stay allowed).
#' @param t codebook data.frame (from [ub_codebook_read()])
#' @param file output .xlsx path
#' @export
ub_codebook_xlsx <- function(t, file) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "codebook")
  st <- if (".status" %in% names(t)) t$.status else ub_codebook_status(t)
  d <- t[, .UB_CB_COLUMNS, drop = FALSE]
  d <- cbind(STATUS = unname(.UB_COLOUR_TEXT[st]), d, stringsAsFactors = FALSE)
  openxlsx::writeData(wb, "codebook", d, withFilter = TRUE,
                      headerStyle = openxlsx::createStyle(textDecoration = "bold", fgFill = "#e9ecef", border = "Bottom"))
  n <- ncol(d)
  locked <- openxlsx::createStyle(locked = TRUE)
  for (s in c("red", "amber")) {
    rows <- which(st == s) + 1
    if (length(rows)) openxlsx::addStyle(wb, "codebook", openxlsx::createStyle(fgFill = .UB_COLOURS[[s]], locked = TRUE),
                                         rows = rows, cols = seq_len(n), gridExpand = TRUE)
  }
  ed <- match(.UB_CB_EDITABLE, names(d))
  for (s in c("red", "amber", "ok")) {
    rows <- which(st == s) + 1
    if (length(rows)) openxlsx::addStyle(wb, "codebook",
      openxlsx::createStyle(fgFill = if (s == "ok") NULL else .UB_COLOURS[[s]], locked = FALSE),
      rows = rows, cols = ed, gridExpand = TRUE)
  }
  openxlsx::setColWidths(wb, "codebook", cols = seq_len(n), widths = c(18, 12, 34, 16, 40, 50))
  openxlsx::freezePane(wb, "codebook", firstRow = TRUE)
  openxlsx::protectWorksheet(wb, "codebook", protect = TRUE, lockAutoFilter = FALSE, lockSorting = FALSE,
                             lockFormattingColumns = FALSE)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  invisible(file)
}

#' Compare decisions made in codebook_review.xlsx with the codebook
#'
#' Rows are matched on TYPE + ACTUAL_VALUE, so a re-sorted or filtered workbook imports correctly.
#' @param ub_dir the UB folder
#' @param file the edited workbook (default private/reports/codebook_review.xlsx)
#' @return list(changes = data.frame(TYPE, ACTUAL_VALUE, column, old, new), updated = codebook data.frame,
#'   unknown = rows in the workbook not in the codebook)
#' @export
ub_codebook_import_xlsx <- function(ub_dir, file = file.path(ub_dir, "private", "reports", "codebook_review.xlsx")) {
  t <- ub_codebook_read(ub_dir)
  if (is.null(t)) ub_error("There is no codebook yet: run the blinding once first.")
  x <- openxlsx::read.xlsx(file, sheet = 1, colNames = TRUE, skipEmptyRows = TRUE, na.strings = character())
  names(x) <- toupper(trimws(names(x)))
  miss <- setdiff(c("TYPE", "ACTUAL_VALUE", .UB_CB_EDITABLE), names(x))
  if (length(miss)) ub_error("The workbook lacks column(s) ", paste(miss, collapse = ", "))
  for (c in names(x)) x[[c]] <- trimws(ifelse(is.na(x[[c]]), "", as.character(x[[c]])))
  key_t <- paste(toupper(t$TYPE), t$ACTUAL_VALUE, sep = "\r")
  key_x <- paste(toupper(x$TYPE), x$ACTUAL_VALUE, sep = "\r")
  ch <- list()
  for (i in seq_len(nrow(x))) {
    j <- match(key_x[i], key_t)
    if (is.na(j)) next
    for (c in .UB_CB_EDITABLE) if (!identical(t[[c]][j], x[[c]][i])) {
      ch[[length(ch) + 1]] <- data.frame(TYPE = t$TYPE[j], ACTUAL_VALUE = t$ACTUAL_VALUE[j], column = c,
                                         old = t[[c]][j], new = x[[c]][i], stringsAsFactors = FALSE)
      t[[c]][j] <- x[[c]][i]
    }
  }
  t$.status <- ub_codebook_status(t)
  changes <- if (length(ch)) do.call(rbind, ch) else
    data.frame(TYPE = character(), ACTUAL_VALUE = character(), column = character(), old = character(), new = character())
  list(changes = changes, updated = t, unknown = x[is.na(match(key_x, key_t)), c("TYPE", "ACTUAL_VALUE"), drop = FALSE])
}

#' HTML report of a Prepare PC run
#' @param res the result of [ub_prepare_pc()]
#' @param file output path
#' @export
ub_prepare_pc_report <- function(res, file) {
  add <- function(d, st) { if (is.null(d) || !NROW(d)) return(d); d$.status <- st; d }
  un <- add(res$unmatched, "red")
  nr <- res$no_result
  if (!is.null(nr) && NROW(nr)) nr$.status <- ifelse(!is.null(nr$status) & nr$status == "amber", "amber", "ok")
  cf <- add(res$conflicts, "red")
  av <- res$aval_check
  if (!is.null(av) && NROW(av)) av$.status <- ifelse(av$status == "red", "red", "ok")
  summ <- data.frame(item = c("Lab rows", "Matched to a PC record", "Results written to PC"),
                     value = c(res$n_lab %||% NA, res$n_matched %||% NA, if (isTRUE(res$written)) "yes" else "no"),
                     stringsAsFactors = FALSE)
  st <- if (isTRUE(res$ok)) rep("ok", 3) else c("ok", "red", "red")
  ub_report_html(summ, st, file, "Prepare PC report",
                 intro = paste(res$messages %||% character(), collapse = " "),
                 sections = list("Lab rows not matched to PC (change the named setting)" = un,
                                 "Values that differ from those already in PC (not overwritten)" = cf,
                                 "PC records without a result" = nr,
                                 "Cross-check with ADNCA/ADPC AVAL" = av))
}
