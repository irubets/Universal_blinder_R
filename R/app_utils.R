# Small helpers shared by the app modules.

# DT table with rows coloured by a .status column (red / amber / info / ok); .status itself is hidden
.ub_dt <- function(df, editable = FALSE, selection = "none", page_length = 25, status_col = ".status",
                   hide = character(), ...) {
  if (is.null(df)) df <- data.frame()
  hidden <- which(names(df) %in% c(status_col, ".row", hide)) - 1
  dt <- DT::datatable(df, rownames = FALSE, selection = selection, editable = editable, filter = "top",
                      options = list(pageLength = page_length, scrollX = TRUE, autoWidth = FALSE,
                                     columnDefs = list(list(visible = FALSE, targets = hidden))), ...)
  if (status_col %in% names(df))
    dt <- DT::formatStyle(dt, status_col, target = "row",
                          backgroundColor = DT::styleEqual(c("red", "amber", "info"),
                                                           unname(.UB_COLOURS[c("red", "amber", "info")])))
  dt
}

# coloured notice box; type = red / amber / ok / info
.ub_notice <- function(type, ...) {
  cls <- c(red = "danger", amber = "warning", ok = "success", info = "info")[[type]]
  shiny::div(class = paste0("alert alert-", cls), role = "alert", ...)
}

.ub_counts_badges <- function(status) {
  shiny::tagList(
    shiny::span(class = "badge bg-danger me-1", paste0("must fix: ", sum(status == "red"))),
    shiny::span(class = "badge bg-warning text-dark me-1", paste0("needs decision: ", sum(status == "amber"))),
    shiny::span(class = "badge bg-success", paste0("OK: ", sum(status %in% c("ok", "info")))))
}

.ub_open_path <- function(path) {
  if (!file.exists(path)) return(invisible(FALSE))
  if (.Platform$OS.type == "windows") shell.exec(normalizePath(path)) else utils::browseURL(path)
  invisible(TRUE)
}

.ub_need_study <- function(ub_dir) {
  shiny::validate(shiny::need(!is.null(ub_dir) && nzchar(ub_dir),
                              "Choose or create the study's Universal Blinder folder in the 'Study' tab first."))
}

# apply DT cell edits (info from input$<id>_cell_edit) to a data.frame; cols = names of editable columns
.ub_apply_edit <- function(df, info, visible_cols = names(df)) {
  for (k in seq_len(nrow(info))) {
    col <- visible_cols[info$col[k] + 1]
    df[info$row[k], col] <- as.character(info$value[k])
  }
  df
}

.ub_split_list <- function(x) {
  x <- trimws(unlist(strsplit(as.character(x), "[,;]")))
  x[nzchar(x)]
}

.ub_read_md <- function(path) {
  if (!file.exists(path)) return(NULL)
  h <- commonmark::markdown_html(paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), extensions = TRUE)
  # images of the user guide sit next to the installed copy (inst/app/www/docs), served by the app as ubwww/docs
  h <- gsub('src="inst/app/www/docs/', 'src="ubwww/docs/', h, fixed = TRUE)
  shiny::HTML(gsub('<img ', '<img style="max-width:100%;height:auto" ', h, fixed = TRUE))
}

.ub_docs_path <- function(name) system.file("app", "www", "docs", name, package = "universalblinder")
