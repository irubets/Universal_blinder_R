#' Codebook review tab
#' @keywords internal
mod_codebook_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Codebook: every replaced name and its code (private\\codebook.csv)"),
    shiny::p("The first run lists every drug, analyte, arm and cohort name with the code that replaces it (TRT01, ANL01, ARM01, ...).",
             "A code that is both a treatment and an analyte rightly gets a TRT code and an ANL code."),
    shiny::p(shiny::strong("Everything is done here, in the app: you do not need to open codebook.csv."),
             " The rows that need you:"),
    shiny::tags$ul(class = "small",
      shiny::tags$li(shiny::span(class = "badge bg-warning text-dark", "amber"), " = the CHECK column asks a question. Read it and ",
                     "check ACTUAL_VALUE and BLINDED_VALUE. If the row is right (or once you have corrected it), ",
                     shiny::strong("click the row to highlight it"), " (several rows are fine) and click ",
                     shiny::em("Mark the highlighted rows as decided"), ". This empties CHECK and saves the codebook."),
      shiny::tags$li(shiny::span(class = "badge bg-danger", "red"), " = a code is missing ('?'; the run stops on it). ",
                     shiny::strong("Double-click"), " the '?' in BLINDED_VALUE, type a code (for example the next free TRT number) or '-' to leave the text as it is, ",
                     "press Enter, then click ", shiny::em("Save codebook"), "."),
      shiny::tags$li("To change a code or add a NOTE, double-click the cell in BLINDED_VALUE, NOTE or CHECK, press Enter, then click ",
                     shiny::em("Save codebook"), ". Every save keeps the previous version in private\\codebook_backup."),
      shiny::tags$li(shiny::strong("Confirm the review."), " When no row needs an answer, type your name and click ",
                     shiny::em("I have reviewed the codebook"), ". The run starts only after this confirmation; a new or ",
                     "changed code asks for it again."),
      shiny::tags$li("Then go back to ", shiny::em("Blind & QC \u2192 2. Run blinding"), " and click ", shiny::em("Run blinding"), " again.",
                     " Working in Excel instead is optional: ", shiny::em("Open in Excel"), ", edit, save, then ",
                     shiny::em("Import decisions from Excel"), ".")),
    shiny::div(class = "d-flex gap-3 align-items-center",
               shiny::checkboxInput(ns("only_attention"), "Rows needing attention only", value = TRUE),
               shiny::uiOutput(ns("counts"))),
    shiny::uiOutput(ns("decide_ui")),
    DT::DTOutput(ns("table")),
    shiny::uiOutput(ns("done_ui")),
    shiny::div(class = "mt-2 d-flex flex-wrap gap-2",
               shiny::actionButton(ns("save"), "Save codebook", class = "btn-primary"),
               shiny::actionButton(ns("reload"), "Discard changes", class = "btn-outline-secondary"),
               shiny::actionButton(ns("open_html"), "Open highlighted HTML", class = "btn-outline-secondary"),
               shiny::actionButton(ns("open_xlsx"), "Open in Excel", class = "btn-outline-secondary"),
               shiny::actionButton(ns("import_xlsx"), "Import decisions from Excel", class = "btn-outline-primary"),
               shiny::actionButton(ns("ask_ai"), "Ask the local AI model", class = "btn-outline-dark")),
    shiny::uiOutput(ns("msg"))
  )
}

#' @keywords internal
mod_codebook_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    cb <- shiny::reactiveVal(NULL)
    dirty <- shiny::reactiveVal(FALSE)
    msg <- shiny::reactiveVal(NULL)
    load <- function() { cb(if (is.null(state$ub_dir)) NULL else ub_codebook_read(state$ub_dir)); dirty(FALSE) }
    shiny::observeEvent(list(state$ub_dir, state$tick), if (!dirty()) load())
    shiny::observeEvent(input$reload, { load(); msg(NULL) })

    shown <- shiny::reactive({
      t <- cb()
      if (is.null(t)) return(NULL)
      if (isTRUE(input$only_attention)) t[t$.status != "ok", , drop = FALSE] else t
    })
    output$counts <- shiny::renderUI({ t <- cb(); if (!is.null(t)) .ub_counts_badges(t$.status) })
    output$table <- DT::renderDT({
      .ub_need_study(state$ub_dir)
      shiny::validate(shiny::need(!is.null(cb()), "There is no codebook yet: run the blinding once (tab 'Blind & QC')."))
      t <- shown()
      ed <- which(names(t) %in% .UB_CB_EDITABLE) - 1
      .ub_dt(t, editable = list(target = "cell", disable = list(columns = setdiff(seq_along(t) - 1, ed))), page_length = 50,
             selection = "multiple")
    })

    # one click: the highlighted rows are decided -> CHECK emptied and the codebook saved (with a backup)
    output$decide_ui <- shiny::renderUI({
      t <- cb(); if (is.null(t) || !any(t$.status == "amber")) return(NULL)
      shiny::div(class = "mb-2", shiny::actionButton(ns("decide"), "Mark the highlighted rows as decided (highlight at least one row first)",
                                                     icon = shiny::icon("check"), class = "btn-success fw-bold"))
    })
    shiny::observe({
      t <- cb(); shiny::req(t, any(t$.status == "amber"))
      n <- length(input$table_rows_selected)
      shiny::updateActionButton(session, "decide", label = if (!n)
        "Mark the highlighted rows as decided (highlight at least one row first)" else
        sprintf("Mark the %d highlighted row%s as decided and save", n, if (n == 1) "" else "s"))
    })
    shiny::observeEvent(input$decide, {
      i <- input$table_rows_selected; s <- shown()
      if (is.null(s) || !length(i)) return(msg(.ub_notice("amber", "Click a row first to highlight it, then click the button.")))
      rows <- s$.row[i]
      t <- cb()
      hit <- t$.row %in% rows
      had_check <- sum(hit & nzchar(t$CHECK))
      t$CHECK[hit] <- ""
      t$.status <- ub_codebook_status(t)
      still_red <- sum(hit & t$.status == "red")
      ub_codebook_write(state$ub_dir, t)
      dirty(FALSE); load()
      msg(.ub_notice(if (still_red) "amber" else "ok",
        sprintf("%d row(s) marked as decided and the codebook saved (previous version in private\\codebook_backup).", had_check),
        if (still_red) sprintf(" %d of them still need a code: double-click the '?' in BLINDED_VALUE.", still_red)))
      state$refresh()
    })
    # last step of the review: a person confirms it (name + time + fingerprint of the codes, private\codebook_review.json)
    output$done_ui <- shiny::renderUI({
      state$tick
      t <- cb(); if (is.null(t) || !nrow(t) || dirty() || any(t$.status %in% c("red", "amber"))) return(NULL)
      rv <- ub_codebook_review_status(state$ub_dir)
      to_run <- shiny::actionLink(ns("to_run"), "Blind & QC \u2192 2. Run blinding", icon = shiny::icon("arrow-right"),
                                  class = "fw-bold text-decoration-underline")
      if (identical(rv$status, "confirmed"))
        return(.ub_notice("ok", shiny::strong("Codebook review confirmed"), " by ", rv$reviewer, " on ", rv$time, ". ",
                          "Go back and run the blinding again: ", to_run))
      .ub_notice("amber",
        shiny::strong("Last step: confirm the review. "),
        if (identical(rv$status, "changed"))
          shiny::span("Codes changed after the review confirmed by ", rv$reviewer, " on ", rv$time, ": confirm again. ")
        else "No rows need an answer any more. ",
        "Confirm that you have read every row (all rows: untick ", shiny::em("Rows needing attention only"), ") and that each ",
        "real name has the right code. ", shiny::em("Run blinding"), " starts only after this confirmation.",
        shiny::div(class = "d-flex flex-wrap gap-2 align-items-end mt-2",
          shiny::textInput(ns("reviewer"), "Your name", value = Sys.info()[["user"]] %||% "", width = "240px"),
          shiny::div(class = "mb-3", shiny::actionButton(ns("confirm_review"), "I have reviewed the codebook",
                                                         icon = shiny::icon("user-check"), class = "btn-success fw-bold"))))
    })
    shiny::observeEvent(input$confirm_review, {
      r <- tryCatch(ub_codebook_confirm_review(state$ub_dir, input$reviewer %||% ""), error = function(e) e)
      if (inherits(r, "error")) return(msg(.ub_notice("red", conditionMessage(r))))
      msg(NULL); load(); state$refresh()
    })
    shiny::observeEvent(input$to_run, state$goto("blind", "2. Run blinding"))
    shiny::observeEvent(input$table_cell_edit, {
      info <- input$table_cell_edit
      t <- cb(); s <- shown()
      for (k in seq_len(nrow(info))) {
        r <- s$.row[info$row[k]]
        col <- names(s)[info$col[k] + 1]
        if (col %in% .UB_CB_EDITABLE) t[t$.row == r, col] <- trimws(as.character(info$value[k]))
      }
      t$.status <- ub_codebook_status(t)
      cb(t); dirty(TRUE)
      msg(.ub_notice("info", "Changes not saved yet: click 'Save codebook'."))
    })
    shiny::observeEvent(input$save, {
      shiny::req(cb())
      ub_codebook_write(state$ub_dir, cb())
      dirty(FALSE); load()
      msg(.ub_notice("ok", "Saved. The previous version is in private\\codebook_backup. Run the blinding again (tab 'Blind & QC')."))
      state$refresh()
    })
    open_report <- function(which) {
      shiny::req(state$ub_dir)
      r <- ub_codebook_reports(state$ub_dir)
      if (is.null(r)) return(msg(.ub_notice("amber", "There is no codebook yet.")))
      .ub_open_path(r[[which]])
      if (which == "xlsx") msg(.ub_notice("info", "Edit BLINDED_VALUE, NOTE or CHECK in Excel, save the file, then click ",
                                         "'Import decisions from Excel'. File: ", shiny::code(r$xlsx)))
    }
    shiny::observeEvent(input$open_html, open_report("html"))
    shiny::observeEvent(input$open_xlsx, open_report("xlsx"))

    pending <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$import_xlsx, {
      shiny::req(state$ub_dir)
      r <- tryCatch(ub_codebook_import_xlsx(state$ub_dir), error = function(e) e)
      if (inherits(r, "error")) return(msg(.ub_notice("red", conditionMessage(r))))
      if (!nrow(r$changes)) return(msg(.ub_notice("info", "The workbook has no changes compared with the codebook.")))
      pending(r)
      shiny::showModal(shiny::modalDialog(
        title = "Import these decisions from Excel?", size = "l",
        shiny::p(nrow(r$changes), " change(s):"), DT::renderDT(.ub_dt(r$changes, page_length = 20)),
        if (nrow(r$unknown)) .ub_notice("amber", nrow(r$unknown), " workbook row(s) are not in the codebook and are ignored."),
        footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton(ns("confirm_import"), "Import and save", class = "btn-primary"))))
    })
    shiny::observeEvent(input$confirm_import, {
      shiny::removeModal()
      ub_codebook_write(state$ub_dir, pending()$updated)
      load()
      msg(.ub_notice("ok", "Imported ", nrow(pending()$changes), " change(s) and saved the codebook."))
      state$refresh()
    })
    shiny::observeEvent(input$ask_ai, {
      state$ai_context <- list(task = "codebook")
      state$goto("ai")
    })
    output$msg <- shiny::renderUI(msg())
  })
}
