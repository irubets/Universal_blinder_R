#' Blind & QC tab
#' @keywords internal
mod_blind_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_card_tab(id = ns("steps"),
    bslib::nav_panel("1. Pre-flight",
      shiny::p("Checks the datasets in input before blinding: identifying columns the rules don't cover, analyte names",
               "equal to their code, free text with dates, a missing exposure dataset, and inputs that differ from what",
               "earlier studies looked like. Each finding has a suggested setting (tab 'Study settings')."),
      shiny::tags$ol(class = "small",
        shiny::tags$li("Click ", shiny::em("Run pre-flight"), ". If the table below lists findings, review them."),
        shiny::tags$li(shiny::strong("To accept a suggestion"), " (for example, to leave a variable such as RACE out of the ",
                       "blinded datasets), click its row to highlight it. Click it again to remove the highlight."),
        shiny::tags$li("Click the green ", shiny::em("Apply"), " button. The suggestions are saved in the study settings ",
                       "(a backup is kept) and the pre-flight runs again. They are used at the next ",
                       shiny::em("Run blinding"), "."),
        shiny::tags$li("The datasets in input are not changed: a variable left out is missing only from output, and it ",
                       "comes back when you retrieve original records. Suggestions that offer a choice (\"... or ...\") ",
                       "or a condition (\"If ...\") are not applied: decide them in 'Study settings'.")),
      shiny::actionButton(ns("preflight"), "Run pre-flight", class = "btn-primary"),
      shiny::uiOutput(ns("pf_counts")),
      shiny::uiOutput(ns("pf_apply_ui")),
      DT::DTOutput(ns("pf")),
      shiny::actionButton(ns("pf_ai"), "Explain these with the AI assistant", class = "btn-outline-dark mt-2"),
      shiny::div(class = "mt-3", shiny::uiOutput(ns("pf_saved")))),
    bslib::nav_panel("2. Run blinding",
      shiny::uiOutput(ns("pc_check")),
      shiny::actionButton(ns("run"), "Run blinding", class = "btn-success btn-lg"),
      shiny::uiOutput(ns("first_run_hint")),
      shiny::uiOutput(ns("run_result")),
      shiny::h6("Log (private\\logs\\blind.log)"),
      shiny::verbatimTextOutput(ns("log")) |> shiny::tagAppendAttributes(class = "ub-log")),
    bslib::nav_panel("3. QC result",
      shiny::uiOutput(ns("qc_badge")),
      shiny::uiOutput(ns("qc_counts")),
      DT::DTOutput(ns("qc_table")),
      shiny::div(class = "mt-2 d-flex gap-2",
                 shiny::actionButton(ns("qc_ai"), "Ask the AI about the selected finding", class = "btn-outline-dark"),
                 shiny::actionButton(ns("open_output"), "Open output folder", class = "btn-outline-secondary")),
      shiny::hr(),
      bslib::layout_columns(col_widths = c(6, 6),
        shiny::div(shiny::h5("output\\qc_summary.md"), shiny::uiOutput(ns("qc_summary"))),
        shiny::div(shiny::h5("output\\RELEASE_MANIFEST.md"), shiny::uiOutput(ns("manifest")))))
  )
}

.ub_qc_findings <- function(ub_dir) {
  p <- file.path(ub_dir, "private", "qc", "qc_report.json")
  if (!file.exists(p)) return(NULL)
  j <- tryCatch(jsonlite::fromJSON(p), error = function(e) NULL)
  f <- j$findings
  if (is.null(f) || !NROW(f)) return(NULL)
  f <- as.data.frame(f, stringsAsFactors = FALSE)
  f$.status <- c(FAIL = "red", WARN = "amber", WAIVED = "info", INFO = "ok")[f$status]
  f$.status[is.na(f$.status)] <- "ok"
  f[order(match(f$status, c("FAIL", "WARN", "WAIVED", "INFO"))), , drop = FALSE]
}

#' @keywords internal
mod_blind_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pf <- shiny::reactiveVal(NULL)
    result <- shiny::reactiveVal(NULL)

    run_preflight <- function() {
      r <- tryCatch(ub_preflight(state$ub_dir, ub_read_options(state$ub_dir)), error = function(e) e)
      if (inherits(r, "error")) { shiny::showNotification(conditionMessage(r), type = "error"); return() }
      r$.status <- ifelse(r$severity %in% c("red", "amber"), r$severity, "info")
      pf(r)
    }
    shiny::observeEvent(input$preflight, { .ub_need_study(state$ub_dir); run_preflight() })
    # the last pre-flight before Run blinding: an empty table, or rows the user leaves as they are on purpose
    output$pf_counts <- shiny::renderUI({
      r <- pf(); if (is.null(r)) return(NULL)
      if (!nrow(r))
        return(.ub_notice("ok", shiny::strong("Nothing left to decide. "), "Go on to ", shiny::em("2. Run blinding"), "."))
      shiny::tagList(shiny::div(class = "my-2", .ub_counts_badges(r$.status)),
        shiny::p(class = "small text-muted", "Rows you leave in this table are left as they are, by your choice: ",
                 "the run uses the settings as they are now. Apply what you accept, run the pre-flight again, ",
                 "and go on to ", shiny::em("2. Run blinding"), " when only rows you have decided to leave remain."))
    })
    output$pf <- DT::renderDT({ shiny::req(pf()); .ub_dt(pf()[, setdiff(names(pf()), "severity")], selection = "multiple") })

    # highlighted rows -> study settings (never the input datasets)
    output$pf_apply_ui <- shiny::renderUI({
      r <- pf(); if (is.null(r) || !nrow(r)) return(NULL)
      shiny::div(class = "mb-2", shiny::actionButton(ns("pf_apply"), "Apply suggestions (highlight at least one row first)",
                                                     icon = shiny::icon("check"), class = "btn-success fw-bold"))
    })
    shiny::observe({
      r <- pf(); shiny::req(r, nrow(r))
      n <- length(input$pf_rows_selected)
      shiny::updateActionButton(session, "pf_apply", label = if (!n) "Apply suggestions (highlight at least one row first)" else
        sprintf("Apply the %d highlighted suggestion%s to the study settings", n, if (n == 1) "" else "s"))
    })
    shiny::observeEvent(input$pf_apply, {
      r <- pf(); i <- input$pf_rows_selected
      if (is.null(r) || !length(i)) return(shiny::showNotification("Highlight at least one row first.", type = "warning"))
      a <- ub_preflight_apply(state$ub_dir, r$suggestion[i])
      if (length(a$problems))
        return(shiny::showNotification(paste("Not applied:", paste(a$problems, collapse = "; ")), type = "error", duration = 15))
      left <- if (length(a$choose))
        paste0(" Not applied (a choice, a condition or advice; decide in 'Study settings'): ",
               paste(unique(paste(r$dataset[i][a$choose], r$variable[i][a$choose])), collapse = ", "), ".") else ""
      if (a$applied) { state$settings_reload <- Sys.time(); state$refresh(); run_preflight() }
      shiny::showNotification(paste0(a$applied, " suggestion(s) applied to the study settings (backup kept).", left),
                              type = if (a$applied) "message" else "warning", duration = 12)
    })
    # choices already saved (same box as the Study tab): the pre-flight does not suggest these again
    output$pf_saved <- shiny::renderUI({ state$tick; state$settings_reload; shiny::req(state$ub_dir); .ub_settings_box(state$ub_dir, ns) })
    .ub_settings_reset_server(input, ns, state, after = function() if (!is.null(shiny::isolate(pf()))) run_preflight())
    shiny::observeEvent(input$pf_ai, { state$ai_context <- list(task = "triage"); state$goto("ai") })

    output$pc_check <- shiny::renderUI({
      state$tick
      shiny::req(state$ub_dir)
      pc <- tryCatch(ub_check_pc_results(state$ub_dir), error = function(e) NULL)
      .ub_pc_notice(pc, state, ns)
    })
    shiny::observeEvent(input$go_prepare, state$goto("prepare_pc"))
    shiny::observeEvent(input$recheck_pc, state$refresh())

    run <- function(continue_anyway = FALSE) {
      opts <- ub_options_for_engine(ub_read_options(state$ub_dir))
      res <- shiny::withProgress(message = "Blinding", value = 0, {
        steps <- c("build_maps", "blind", "qc", "unblind", "verify", "release")
        prog <- function(step, message) {
          i <- match(step, steps)
          shiny::setProgress(value = if (is.na(i)) NULL else i / length(steps), detail = paste(step, message))
        }
        tryCatch(ub_blind_folder(state$ub_dir, opts, quiet = TRUE, continue_without_results = continue_anyway,
                                 on_progress = prog, require_codebook_review = TRUE),
                 error = function(e) list(released = FALSE, error = conditionMessage(e)))
      })
      result(res)
      state$refresh()
    }
    shiny::observeEvent(input$run, {
      .ub_need_study(state$ub_dir)
      # after the first run, the blinding runs only on a codebook that a person confirmed as reviewed
      rv <- ub_codebook_review_status(state$ub_dir)
      if (rv$status %in% c("open", "unconfirmed", "changed")) {
        shiny::showModal(shiny::modalDialog(
          title = "Confirm the codebook review first",
          if (identical(rv$status, "open")) sprintf("%d codebook row(s) still need an answer.", rv$open)
          else if (identical(rv$status, "changed")) "Codes changed after the codebook review was confirmed."
          else "The codebook review has not been confirmed yet.",
          " Open Codebook review, answer the rows, then type your name and click 'I have reviewed the codebook'.",
          footer = shiny::tagList(shiny::actionButton(ns("to_codebook_gate"), "Open Codebook review", class = "btn-primary"),
                                  shiny::modalButton("Cancel"))))
        return()
      }
      pc <- tryCatch(ub_check_pc_results(state$ub_dir), error = function(e) NULL)
      wf3 <- .ub_lab_release_on(ub_read_options(state$ub_dir))
      if (!is.null(pc) && identical(pc$status, "lab_file_not_merged") && !wf3) {
        shiny::showModal(shiny::modalDialog(
          title = "PC has no results", "PC has no concentrations and a lab results file in input was not merged into PC.",
          " Put the results into PC first (tab 'Prepare PC'), or, if they can't be matched to PC, release the lab file ",
          "blinded next to PC (Workflow 3), or blind PC without results on purpose.",
          footer = shiny::tagList(shiny::actionButton(ns("to_prepare"), "Go to Prepare PC", class = "btn-primary"),
                                  shiny::actionButton(ns("to_workflow3_modal"), "Release the lab file blinded (Workflow 3)",
                                                      class = "btn-outline-primary"),
                                  shiny::actionButton(ns("continue_anyway"), "Continue anyway", class = "btn-outline-danger"),
                                  shiny::modalButton("Cancel"))))
        return()
      }
      run(FALSE)
    })
    shiny::observeEvent(input$to_codebook_gate, { shiny::removeModal(); state$goto("codebook") })
    # Workflow 3: Prepare PC turns the switch on and shows how the lab rows match PC
    shiny::observeEvent(input$to_workflow3_modal, { shiny::removeModal(); state$wf3_request <- Sys.time(); state$goto("prepare_pc") })
    shiny::observeEvent(input$go_wf3, state$goto("prepare_pc"))
    shiny::observeEvent(input$to_prepare, { shiny::removeModal(); state$goto("prepare_pc") })
    shiny::observeEvent(input$continue_anyway, { shiny::removeModal(); run(TRUE) })

    output$run_result <- shiny::renderUI({
      r <- result()
      if (is.null(r)) return(NULL)
      if (isTRUE(r$released)) {
        lf <- list.files(file.path(state$ub_dir, "output"), pattern = "_blinded\\.csv$", full.names = TRUE)
        lab <- if (length(lf)) tryCatch({ m <- .ub_lab_read_out(lf[1])$MATCH_STATUS
          sprintf("Lab file released: %d rows, %d matched to PC (%s). ", length(m), sum(m == "matched"), basename(lf[1])) },
          error = function(e) NULL)
        return(.ub_notice(if (identical(r$qc, "WARN")) "amber" else "ok", shiny::strong("FINISHED -- QC ", r$qc, ". "),
                          "The blinded datasets are in output. ", lab,
                          "Read qc_summary.md first (tab '3. QC result'), then share only output."))
      }
      cb <- grepl("codebook", r$error %||% "", ignore.case = TRUE)
      if (cb)
        return(.ub_notice("amber", shiny::strong("NOTHING WAS RELEASED. "), r$error,
                          shiny::div(shiny::actionButton(ns("to_codebook"), "Review the codebook", class = "btn-sm btn-dark mt-1"))))
      fails <- if (is.null(r$error) && identical(r$qc, "FAIL")) {
        f <- .ub_qc_findings(state$ub_dir); if (!is.null(f)) f[f$status == "FAIL", , drop = FALSE]
      }
      ai_btn <- shiny::actionButton(ns("why_ai"), "Ask the AI why", icon = shiny::icon("comments"), class = "btn-sm btn-outline-dark")
      if (!is.null(fails) && nrow(fails))
        return(.ub_notice("red", shiny::strong("NOTHING WAS RELEASED: QC failed. "),
          sprintf("%d check%s failed, so nothing was copied to output:", nrow(fails), if (nrow(fails) == 1) "" else "s"),
          shiny::tags$ul(class = "mb-1", lapply(seq_len(nrow(fails)), function(i) shiny::tags$li(
            shiny::strong(paste(c(if (!is.null(fails$dataset) && !is.na(fails$dataset[i])) fails$dataset[i], fails$check[i]),
                                collapse = ": ")),
            if (!is.null(fails$variable) && !is.na(fails$variable[i]) && nzchar(fails$variable[i])) paste0(" (", fails$variable[i], ")"),
            if (!is.null(fails$detail) && !is.na(fails$detail[i])) paste0(": ", fails$detail[i])))),
          shiny::p(class = "mb-1", "Run the ", shiny::em("Pre-flight"), " again: it often lists a row that fixes this, which you can ",
                   "apply with one click. Or change the setting in 'Study settings', then click ", shiny::em("Run blinding"), " again."),
          shiny::div(class = "d-flex gap-2 flex-wrap",
            shiny::actionButton(ns("to_preflight"), "Open 1. Pre-flight", class = "btn-sm btn-dark"),
            shiny::actionButton(ns("to_qc"), "Open 3. QC result", class = "btn-sm btn-outline-dark"),
            ai_btn)))
      .ub_notice("red", shiny::strong("NOTHING WAS RELEASED. "), r$error %||% "See the log below.",
                 shiny::div(class = "mt-1", ai_btn))
    })
    shiny::observeEvent(input$to_codebook, state$goto("codebook"))
    shiny::observeEvent(input$to_preflight, bslib::nav_select("steps", "1. Pre-flight", session = session))
    shiny::observeEvent(input$to_qc, bslib::nav_select("steps", "3. QC result", session = session))
    # the AI gets the QC findings through the usual triage task (masked for Claude), not the log
    shiny::observeEvent(input$why_ai, {
      r <- result()
      state$ai_context <- list(task = "triage", release_failed = TRUE, question = paste0(
        "The last 'Run blinding' released nothing ", if (identical(r$qc, "FAIL")) "because QC failed" else "because the run stopped with an error",
        ". Explain in plain words why, which failed check matters, and the setting that fixes it."))
      state$goto("ai")
    })

    # shown only until the codebook review is done: before the first run, and while codebook rows are open
    output$first_run_hint <- shiny::renderUI({
      state$tick
      shiny::req(state$ub_dir)
      s <- tryCatch(ub_study_status(state$ub_dir), error = function(e) NULL)
      if (is.null(s)) return(NULL)
      link <- shiny::actionLink(ns("hint_codebook"), "Codebook review", icon = shiny::icon("arrow-right"),
                                class = "fw-bold text-decoration-underline")
      if (!s$codebook_rows && !isTRUE(s$key_present))
        return(.ub_notice("info", "The first run builds the key and the codebook and ", shiny::strong("stops once"),
                          " for the review. Then open ", link, ", answer the red and amber rows, confirm the review, and click ",
                          shiny::em("Run blinding"), " again."))
      if (s$codebook_open > 0)
        return(.ub_notice("amber", shiny::strong(s$codebook_open, " codebook row(s) to review. "), "Open ", link,
                          ", answer them there (in the app; no need to open codebook.csv), confirm the review, then click ",
                          shiny::em("Run blinding"), " again."))
      rv <- ub_codebook_review_status(state$ub_dir)
      if (rv$status %in% c("unconfirmed", "changed"))
        return(.ub_notice("amber", shiny::strong("The codebook review is not confirmed yet. "), "Open ", link,
                          ", type your name and click ", shiny::em("I have reviewed the codebook"), ". ",
                          shiny::em("Run blinding"), " starts only after that."))
      if (identical(rv$status, "confirmed"))
        return(shiny::p(class = "small text-success mt-2", shiny::icon("user-check"),
                        sprintf(" Codebook review confirmed by %s on %s.", rv$reviewer, rv$time)))
      NULL
    })
    shiny::observeEvent(input$hint_codebook, state$goto("codebook"))
    output$log <- shiny::renderText({
      state$tick
      shiny::req(state$ub_dir)
      p <- file.path(state$ub_dir, "private", "logs", "blind.log")
      if (file.exists(p)) paste(readLines(p, warn = FALSE), collapse = "\n") else "No run yet."
    })

    qc <- shiny::reactive({ state$tick; shiny::req(state$ub_dir); .ub_qc_findings(state$ub_dir) })
    output$qc_badge <- shiny::renderUI({
      st <- ub_study_status(shiny::req(state$ub_dir))$qc
      if (is.na(st)) return(shiny::p("No QC result yet."))
      cls <- c(PASS = "bg-success", WARN = "bg-warning text-dark", FAIL = "bg-danger")[[st]]
      shiny::h4(shiny::span(class = paste("badge", cls), paste("QC", st)))
    })
    output$qc_counts <- shiny::renderUI({ f <- qc(); if (!is.null(f)) .ub_counts_badges(f$.status) })
    output$qc_table <- DT::renderDT({
      f <- qc()
      shiny::validate(shiny::need(!is.null(f), "No QC findings yet."))
      .ub_dt(f[, intersect(c("status", "check", "dataset", "variable", "detail", ".status"), names(f))],
             selection = "single")
    })
    shiny::observeEvent(input$qc_ai, {
      f <- qc(); i <- input$qc_table_rows_selected
      if (is.null(f) || !length(i)) return(shiny::showNotification("Select a finding first.", type = "warning"))
      state$ai_context <- list(task = "triage", finding = as.list(f[i, setdiff(names(f), ".status")]))
      state$goto("ai")
    })
    shiny::observeEvent(input$open_output, .ub_open_path(file.path(shiny::req(state$ub_dir), "output")))
    output$qc_summary <- shiny::renderUI({ state$tick; .ub_read_md(file.path(shiny::req(state$ub_dir), "output", "qc_summary.md")) %||% "Not written yet." })
    output$manifest <- shiny::renderUI({ state$tick; .ub_read_md(file.path(shiny::req(state$ub_dir), "output", "RELEASE_MANIFEST.md")) %||% "Not written yet." })
  })
}
