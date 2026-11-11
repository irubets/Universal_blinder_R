#' Retrieve tab
#' @keywords internal
mod_retrieve_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(5, 7),
    bslib::card(
      bslib::card_header("Findings from the review of the blinded data"),
      shiny::p("One row per finding. Each row needs at least one identifier: UBRECID (the reviewer quotes it), or the",
               "blinded SUBJID/USUBJID, APROFILE, ATEST/PCTEST, PARAMCD/PCTESTCD, AVISIT/VISIT. Several values for one",
               "variable: separate them with ';'. FINDING is the reviewer's text."),
      shiny::fileInput(ns("upload"), "Upload findings.csv", accept = c(".csv", ".txt")),
      shiny::textAreaInput(ns("pasted"), "...or paste from Excel (with the header row)", rows = 5, width = "100%"),
      shiny::actionButton(ns("use_pasted"), "Use pasted rows", class = "btn-outline-primary"),
      shiny::actionButton(ns("add_row"), "Add an empty row", class = "btn-outline-secondary"),
      shiny::actionButton(ns("clear"), "Clear", class = "btn-outline-secondary")),
    bslib::card(
      bslib::card_header("Findings (double-click a cell to edit)"),
      DT::DTOutput(ns("findings")),
      shiny::actionButton(ns("retrieve"), "Retrieve the original records", class = "btn-success mt-2"),
      shiny::uiOutput(ns("msg")),
      shiny::uiOutput(ns("results")))
  )
}

.UB_FINDING_COLS <- c("DATASET", "UBRECID", "SUBJID", "USUBJID", "APROFILE", "ATEST", "PCTESTCD", "PARAMCD",
                      "AVISIT", "VISIT", "FINDING")

#' @keywords internal
mod_retrieve_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    empty <- as.data.frame(setNames(replicate(length(.UB_FINDING_COLS), character(), simplify = FALSE), .UB_FINDING_COLS),
                           stringsAsFactors = FALSE)
    fd <- shiny::reactiveVal(empty)
    res <- shiny::reactiveVal(NULL)
    msg <- shiny::reactiveVal(NULL)
    norm <- function(d) {
      names(d) <- toupper(trimws(names(d)))
      for (c in names(d)) d[[c]] <- ifelse(is.na(d[[c]]), "", as.character(d[[c]]))
      for (c in setdiff(.UB_FINDING_COLS, names(d))) d[[c]] <- rep("", nrow(d))
      d[, c(.UB_FINDING_COLS, setdiff(names(d), .UB_FINDING_COLS)), drop = FALSE]
    }
    shiny::observeEvent(input$upload, {
      d <- tryCatch(utils::read.csv(input$upload$datapath, colClasses = "character", check.names = FALSE,
                                    fileEncoding = "UTF-8-BOM"), error = function(e) NULL)
      if (is.null(d)) return(msg(.ub_notice("red", "Could not read this file as CSV.")))
      fd(norm(d))
    })
    shiny::observeEvent(input$use_pasted, {
      txt <- input$pasted
      if (!nzchar(trimws(txt))) return()
      sep <- if (grepl("\t", txt)) "\t" else ","
      d <- tryCatch(utils::read.table(text = txt, sep = sep, header = TRUE, colClasses = "character", quote = "\"",
                                      check.names = FALSE, fill = TRUE, comment.char = ""), error = function(e) NULL)
      if (is.null(d)) return(msg(.ub_notice("red", "Could not read the pasted rows; include the header row.")))
      fd(norm(d))
    })
    shiny::observeEvent(input$add_row, fd(rbind(fd(), as.data.frame(as.list(setNames(rep("", ncol(fd())), names(fd()))),
                                                                   stringsAsFactors = FALSE))))
    shiny::observeEvent(input$clear, { fd(empty); res(NULL) })
    shiny::observeEvent(state$ai_findings, if (!is.null(state$ai_findings)) fd(norm(as.data.frame(state$ai_findings))))
    output$findings <- DT::renderDT(.ub_dt(fd(), editable = "cell", page_length = 15))
    shiny::observeEvent(input$findings_cell_edit, fd(.ub_apply_edit(fd(), input$findings_cell_edit)))

    shiny::observeEvent(input$retrieve, {
      .ub_need_study(state$ub_dir)
      d <- fd()
      d <- d[apply(d[, setdiff(names(d), c("FINDING", "DATASET")), drop = FALSE], 1, function(r) any(nzchar(r))), , drop = FALSE]
      if (!nrow(d)) return(msg(.ub_notice("amber", "Add at least one finding with an identifier.")))
      p <- file.path(state$ub_dir, "private", "retrieved", "findings_used.csv")
      dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
      utils::write.csv(d[, colSums(d != "") > 0, drop = FALSE], p, row.names = FALSE, fileEncoding = "UTF-8")
      r <- tryCatch(ub_retrieve_folder(state$ub_dir, p, ub_options_for_engine(ub_read_options(state$ub_dir)), quiet = TRUE),
                    error = function(e) e)
      if (inherits(r, "error")) return(msg(.ub_notice("red", conditionMessage(r))))
      res(r)
      hit <- unique(unlist(lapply(r, function(x) x$FINDING_NO)))
      miss <- setdiff(seq_len(nrow(d)), hit)
      msg(shiny::tagList(
        .ub_notice("ok", sum(vapply(r, nrow, 1L)), " original record(s) written to ", shiny::code("private\\retrieved"),
                   " (PRIVATE; never sent to the Claude API)."),
        if (length(miss)) .ub_notice("amber", "No matching record for finding(s) ", paste(miss, collapse = ", "),
                                     ": check the identifiers they quote.")))
    })
    output$msg <- shiny::renderUI(msg())
    output$results <- shiny::renderUI({
      r <- res()
      if (is.null(r) || !length(r)) return(NULL)
      do.call(bslib::navset_tab, lapply(names(r), function(nm) bslib::nav_panel(
        paste0(nm, " (", nrow(r[[nm]]), ")"), DT::renderDT(.ub_dt(r[[nm]], page_length = 10)))))
    })
  })
}
