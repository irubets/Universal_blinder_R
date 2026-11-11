#' Workflow tab: the two workflows as steps, with the status of the selected study
#' @keywords internal
mod_workflow_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(7, 5),
    bslib::card(
      bslib::card_header("How to blind a study"),
      shiny::p("The Universal Blinder makes blinded copies of PK datasets (PC, ADNCA/ADPC, EX, ADSL, ...) that you can",
               "send to a CRO, a vendor or a review tool. Subjects, sites, treatment and analyte names, dates and",
               "concentrations are disguised consistently across the study. The private key never leaves your computer",
               "and maps every finding back to the original records."),
      shiny::h5("Which workflow?"),
      shiny::tags$table(class = "table table-sm",
        shiny::tags$tr(shiny::tags$th("Your PC dataset"), shiny::tags$th("Workflow")),
        shiny::tags$tr(shiny::tags$td("has the concentrations (PCORRES, PCSTRESC, PCSTRESN filled in)"),
                       shiny::tags$td(shiny::strong("1"), ": blind as it is")),
        shiny::tags$tr(shiny::tags$td("has the samples but no concentrations, and the lab sent the results in a separate CSV/Excel file"),
                       shiny::tags$td(shiny::strong("2"), ": first put the results into PC (Prepare PC), then blind"))),
      shiny::p("The app checks this for you once PC is in input."),
      shiny::h5("Steps for the selected study"),
      shiny::uiOutput(ns("steps"))
    ),
    bslib::card(
      bslib::card_header("Good to know"),
      shiny::tags$ul(
        shiny::tags$li("Share only the files in ", shiny::code("output"), ". Never share ", shiny::code("input"), " or ",
                       shiny::code("private"), "."),
        shiny::tags$li("Each subject's concentrations are multiplied by a random factor between 0.8 and 1.2, and each",
                       " subject's dates are shifted by a random number of days. Profile shapes, timing, ratios and study",
                       " days within a subject stay exact."),
        shiny::tags$li("Blinded data is pseudonymised, not anonymised: send it only where your data-protection rules allow."),
        shiny::tags$li("Keep the whole ", shiny::code("private"), " folder with the study for as long as you may need to map",
                       " results back. Use one folder per study; never copy a private folder to another study."),
        shiny::tags$li("Both spellings BQL<(0.500) and BLQ<(0.500) are understood; the LLOQ inside them is blinded too.")),
      shiny::actionLink(ns("show_guide"), "Open the full user guide")
    )
  )
}

#' @keywords internal
mod_workflow_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    step <- function(n, title, status, text, tab = NULL, label = NULL) {
      cls <- c(done = "done", todo = "todo", fail = "fail", na = "")[[status]]
      icon <- intToUtf8(c(done = 0x2714, todo = 0x25cb, fail = 0x2716, na = 0x2013)[[status]])
      shiny::div(class = paste("ub-step", cls),
                 shiny::strong(icon, " ", n, ". ", title), shiny::br(), text,
                 if (!is.null(tab)) shiny::actionLink(ns(paste0("go_", tab)), label %||% "Open"))
    }
    output$steps <- shiny::renderUI({
      state$tick
      if (is.null(state$ub_dir))
        return(shiny::tagList(step(1, "Choose the study", "todo", "Pick or paste the study folder; the app creates UB_<StudyNumber>. ",
                                   "study", "Go to Study")))
      s <- ub_study_status(state$ub_dir)
      wf2 <- !is.null(s$pc) && s$pc$status %in% c("no_results", "lab_file_not_merged")
      shiny::tagList(
        step(1, "Study folder", "done", shiny::code(state$ub_dir)),
        step(2, "Copy datasets into input", if (nrow(s$datasets)) "done" else "todo",
             if (nrow(s$datasets)) paste(nrow(s$datasets), "dataset(s) in input. ") else "Nothing in input yet. ", "study", "Go to Study"),
        step(3, "Workflow 2 only: put the lab results into PC",
             if (!wf2) "na" else if (identical(s$pc$status, "lab_file_not_merged")) "todo" else "todo",
             if (!wf2) "Not needed (PC has results, or no PC). "
             else "PC has no concentrations. If the results can't be matched to PC, release the lab file blinded instead (Workflow 3, same tab). ",
             if (wf2) "prepare_pc", "Go to Prepare PC"),
        step(4, "First run: builds the key and the codebook, then stops once", if (s$codebook_rows) "done" else "todo",
             "Run the blinding once. ", "blind", "Go to Blind & QC"),
        step(5, "Review the codebook and confirm the review",
             if (!s$codebook_rows) "todo" else if (s$codebook_answer_needed) "fail"
             else if (identical(s$codebook_review$status, "confirmed")) "done" else "todo",
             if (!s$codebook_rows) ""
             else if (s$codebook_open) sprintf("%d row(s) still need attention. ", s$codebook_open)
             else if (identical(s$codebook_review$status, "confirmed"))
               sprintf("Confirmed by %s on %s. ", s$codebook_review$reviewer, s$codebook_review$time)
             else "All rows answered; the review is not confirmed yet. ",
             "codebook", "Go to Codebook review"),
        step(6, "Run again: QC and release", if (is.na(s$qc)) "todo" else if (s$qc == "FAIL") "fail" else "done",
             if (is.na(s$qc)) "" else paste0("Last QC: ", s$qc, ". "), "blind", "Go to Blind & QC"),
        step(7, "Share only output; after the review, retrieve the originals", "na", "", "retrieve", "Go to Retrieve"))
    })
    for (tab in c("study", "prepare_pc", "blind", "codebook", "retrieve")) local({
      t <- tab
      shiny::observeEvent(input[[paste0("go_", t)]], state$goto(t), ignoreInit = TRUE)
    })
    shiny::observeEvent(input$show_guide, {
      g <- .ub_read_md(.ub_docs_path("USERGUIDE.md"))
      shiny::showModal(shiny::modalDialog(if (is.null(g)) "The user guide is not installed." else g,
                                          title = "User guide", size = "xl", easyClose = TRUE))
    })
  })
}
