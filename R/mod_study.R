#' Study tab
#' @keywords internal
mod_study_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(5, 7),
    bslib::card(
      bslib::card_header("1. Choose the study folder"),
      shiny::p("Pick the study's existing folder (any layout), or its UB_<StudyNumber> folder if you made one already."),
      shinyFiles::shinyDirButton(ns("tree"), "Browse folders...", "Choose the study folder", class = "btn-primary mb-2"),
      shiny::textInput(ns("pasted"), "...or paste the full path (with or without quotes)", width = "100%",
                       placeholder = "\"C:\\projects\\ABC-123\""),
      shiny::actionButton(ns("open_pasted"), "Use this path", class = "btn-outline-primary mb-2"),
      shiny::selectInput(ns("recent"), "...or a recent study", choices = character(), width = "100%"),
      shiny::uiOutput(ns("chosen")),
      shiny::uiOutput(ns("setup_box"))
    ),
    shiny::div(
      bslib::card(bslib::card_header("2. Copy the datasets to blind into input"),
                  shiny::uiOutput(ns("copy_help")),
                  shiny::div(class = "d-flex flex-wrap gap-2 align-items-center mb-2",
                             shiny::actionButton(ns("copy"), "Copy selected files to input",
                                                 icon = shiny::icon("copy"),
                                                 class = "btn-success btn-lg fw-bold"),
                             shiny::actionButton(ns("rescan"), "Rescan", class = "btn-outline-secondary"),
                             shiny::actionButton(ns("open_input"), "Open input folder", class = "btn-outline-secondary")),
                  shiny::uiOutput(ns("copy_result")),
                  DT::DTOutput(ns("scan"))),
      bslib::card(bslib::card_header("3. Status"),
                  shiny::uiOutput(ns("status")))
    )
  )
}

#' @keywords internal
mod_study_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    roots <- c(Home = normalizePath("~", mustWork = FALSE), shinyFiles::getVolumes()())
    shinyFiles::shinyDirChoose(input, "tree", roots = roots, session = session)
    shiny::updateSelectInput(session, "recent", choices = c("(choose)" = "", ub_recent_studies()))
    msg <- shiny::reactiveVal(NULL)

    choose <- function(path) {
      path <- ub_clean_path(path)
      if (!nzchar(path)) return(msg(.ub_notice("amber", "Type or paste a folder path.")))
      if (!dir.exists(path)) return(msg(.ub_notice("red", "This folder does not exist: ", shiny::code(path))))
      path <- normalizePath(path)
      msg(NULL)
      if (ub_is_ub_folder(path)) {
        state$ub_dir <- path
        state$study_dir <- dirname(path)
        ub_add_recent_study(path)
      } else {
        state$study_dir <- path
        state$ub_dir <- NULL
        ubs <- list.dirs(path, recursive = FALSE)
        ubs <- ubs[grepl("^UB_", basename(ubs)) & vapply(ubs, ub_is_ub_folder, TRUE)]
        if (length(ubs) == 1) {
          state$ub_dir <- normalizePath(ubs)
          ub_add_recent_study(state$ub_dir)
          msg(.ub_notice("info", "Opened the existing Universal Blinder folder ", shiny::code(basename(ubs)), "."))
        }
      }
      state$refresh()
    }
    shiny::observeEvent(input$tree, {
      p <- shinyFiles::parseDirPath(roots, input$tree)
      if (length(p)) choose(p)
    })
    shiny::observeEvent(input$open_pasted, choose(input$pasted))
    shiny::observeEvent(input$recent, if (nzchar(input$recent)) choose(input$recent))

    output$chosen <- shiny::renderUI({
      shiny::tagList(msg(),
        if (!is.null(state$study_dir)) shiny::p(shiny::strong("Study folder: "), shiny::code(state$study_dir)))
    })

    output$setup_box <- shiny::renderUI({
      if (is.null(state$study_dir) || !is.null(state$ub_dir)) return(NULL)
      sn <- tryCatch(ub_guess_study_number(state$study_dir), error = function(e) "")
      shiny::tagList(
        shiny::hr(),
        shiny::h5("Create the Universal Blinder folder"),
        shiny::textInput(ns("study_number"), "Study number (from STUDYID; you can change it)", value = sn),
        shiny::uiOutput(ns("proposed")),
        shiny::actionButton(ns("create"), "Create the folder", class = "btn-success"))
    })
    output$proposed <- shiny::renderUI({
      shiny::req(state$study_dir)
      ub <- file.path(state$study_dir, paste0("UB_", .ub_sanitise_name(input$study_number %||% "")))
      shiny::tags$pre(paste0(ub, "\\\n",
                             "    blind_options.json  blind.R  prepare_pc.R  retrieve.R\n",
                             "    input\\     <- the datasets to blind (+ lab results file, Workflow 2)\n",
                             "    output\\    <- what you may share (after a run)\n",
                             "    private\\   <- THE KEY, reports, logs: never share"))
    })
    shiny::observeEvent(input$create, {
      r <- tryCatch(ub_setup_study(state$study_dir, input$study_number), ub_error = function(e) e, error = function(e) e)
      if (inherits(r, "error")) return(msg(.ub_notice("red", conditionMessage(r))))
      state$ub_dir <- r$path
      ub_add_recent_study(r$path)
      msg(.ub_notice("ok", if (r$created) "Created " else "Opened ", shiny::code(r$path),
                     ". Now copy the datasets to blind into input (right)."))
      state$refresh()
    })

    scan <- shiny::reactive({
      state$tick; input$rescan
      shiny::req(state$study_dir)
      ub_scan_study(state$study_dir, state$ub_dir)
    })
    output$copy_help <- shiny::renderUI({
      if (is.null(state$ub_dir)) return(shiny::p("Create or open the Universal Blinder folder first."))
      shiny::tagList(
        shiny::p("These files were found under the study folder. ",
                 shiny::strong("Rows highlighted in blue are selected."),
                 " These are selected for you: the targets (PC, ADNCA/ADPC), the exposure datasets (EX, EC, ADEX) and the ",
                 shiny::em("linked"), " datasets (PP, ADPP, ADPD). Linked datasets are blinded with the same key, so blinded ",
                 "subjects, analytes (PPCAT = PCTEST), reference dates (PPRFTDTC = PCRFTDTC), specimen and visits still match the concentrations. ",
                 shiny::em("Subject-level"), " datasets (ADSL, DM) are optional: add them only if the analysis needs ",
                 "variables that ADNCA/ADPP do not already carry (see the user guide, 'ADSL and DM')."),
        shiny::tags$ol(
          shiny::tags$li("Click anywhere on a row to highlight it (select) or to remove the highlight (deselect). ",
                         "There are no checkboxes: the blue highlight is the selection."),
          shiny::tags$li("If PC came without results, also click the lab's results file (CSV/Excel) to highlight it."),
          shiny::tags$li("Click the green ", shiny::strong("Copy selected files to input"),
                         " button just below. It shows how many files are selected.")),
        shiny::p(class = "text-muted small",
                 "You can also copy files into input by hand and click Rescan."))
    })
    shiny::observe({
      n <- length(input$scan_rows_selected)
      shiny::updateActionButton(session, "copy",
        label = if (n) sprintf("Copy the %d highlighted file%s to input", n, if (n == 1) "" else "s")
                else "Copy selected files to input (highlight rows first)")
    })
    output$scan <- DT::renderDT({
      s <- scan()
      d <- s[, c("relative", "profile", "role", "size_kb", "modified", "already_copied")]
      d$.status <- ifelse(s$role == "no profile", "amber", ifelse(s$already_copied, "info", "ok"))
      .ub_dt(d, selection = list(mode = "multiple", selected = which(s$preselect & !s$already_copied)), page_length = 50)
    })
    output$copy_result <- shiny::renderUI(NULL)
    shiny::observeEvent(input$copy, {
      shiny::req(state$ub_dir)
      sel <- input$scan_rows_selected
      if (!length(sel)) return(output$copy_result <- shiny::renderUI(.ub_notice("amber", "No file is highlighted. Click at least one row in the table to highlight it.")))
      r <- ub_copy_to_input(state$ub_dir, scan()$path[sel])
      output$copy_result <- shiny::renderUI(.ub_notice(if (any(r$status %in% c("failed", "exists (not overwritten)"))) "amber" else "ok",
        shiny::tags$ul(lapply(seq_len(nrow(r)), function(i) shiny::tags$li(r$file[i], ": ", r$status[i])))))
      state$refresh()
    })
    shiny::observeEvent(input$open_input, if (!is.null(state$ub_dir)) .ub_open_path(file.path(state$ub_dir, "input")))

    output$status <- shiny::renderUI({
      state$tick
      shiny::req(state$ub_dir)
      s <- ub_study_status(state$ub_dir)
      pc <- s$pc
      shiny::tagList(
        shiny::p(shiny::strong("Datasets in input: "),
                 if (nrow(s$datasets)) paste0(s$datasets$file, " (", ifelse(is.na(s$datasets$profile), "no profile: set an alias in Study settings", s$datasets$profile), ")", collapse = ", ")
                 else "none yet"),
        if (length(s$lab_files)) shiny::p(shiny::strong("Lab results files in input: "), paste(s$lab_files, collapse = ", ")),
        .ub_pc_notice(pc, state, ns),
        shiny::p(shiny::strong("Key: "), if (s$key_present) "present (private\\maps)" else "not yet (made by the first run)"),
        shiny::p(shiny::strong("Codebook: "), if (s$codebook_rows) sprintf("%d rows, %d need attention", s$codebook_rows, s$codebook_open) else "not yet",
                 if (identical(s$codebook_review$status, "confirmed"))
                   sprintf("; review confirmed by %s on %s", s$codebook_review$reviewer, s$codebook_review$time)
                 else if (s$codebook_rows) "; review not confirmed"),
        shiny::p(shiny::strong("Last QC: "), if (is.na(s$qc)) "not run" else s$qc),
        .ub_settings_box(state$ub_dir, ns))
    })
    .ub_settings_reset_server(input, ns, state)
    shiny::observeEvent(input$recheck_pc, state$refresh())
    shiny::observeEvent(input$go_prepare, state$goto("prepare_pc"))
    shiny::observeEvent(input$go_wf3, state$goto("prepare_pc"))
  })
}

# the reset button of .ub_settings_box: confirm, then write the defaults (blind_options.json, with a backup).
# Used by the Study tab and the Pre-flight tab; after() runs once the reset is done.
.ub_settings_reset_server <- function(input, ns, state, after = NULL) {
  shiny::observeEvent(input$reset_settings, {
    shiny::showModal(shiny::modalDialog(
      title = "Reset settings to defaults?",
      "All choices saved for this study in blind_options.json (variables left out, blanked, aliases, waivers, ",
      "configuration changes) are replaced by the defaults of a new study. The previous version is kept in ",
      "private\\settings_backup. The datasets in input, the key and the codebook are not changed.",
      footer = shiny::tagList(shiny::actionButton(ns("confirm_reset"), "Reset to defaults", class = "btn-danger"),
                              shiny::modalButton("Cancel"))))
  })
  shiny::observeEvent(input$confirm_reset, {
    shiny::removeModal()
    ub_reset_options(state$ub_dir)
    state$settings_reload <- Sys.time(); state$refresh()
    if (is.function(after)) after()
    shiny::showNotification("Settings reset to the defaults (backup in private\\settings_backup). Run the pre-flight again.",
                            type = "message", duration = 10)
  })
}

# saved settings of the study: what differs from the defaults, and a reset button
.ub_settings_box <- function(ub_dir, ns) {
  ch <- tryCatch(ub_settings_changes(ub_dir), error = function(e) NULL)
  if (is.null(ch)) return(NULL)
  if (!nrow(ch))
    return(shiny::p(shiny::strong("Saved settings: "), "the defaults (no choices saved for this study yet)."))
  show <- utils::head(ch, 10)
  .ub_notice("info",
    shiny::strong(sprintf("Saved settings: %d saved choice%s ", nrow(ch), if (nrow(ch) == 1) "" else "s")),
    "(blind_options.json, next to input; not in private). The pre-flight does not suggest again what is already here.",
    shiny::tags$ul(class = "small mb-1", lapply(seq_len(nrow(show)), function(i)
      shiny::tags$li(if (show$change[i] == "removed") "default removed: " else "", shiny::code(show$item[i])))),
    if (nrow(ch) > 10) shiny::p(class = "small mb-1", sprintf("... and %d more (tab 'Study settings').", nrow(ch) - 10)),
    shiny::actionButton(ns("reset_settings"), "Reset settings to defaults", icon = shiny::icon("rotate-left"),
                        class = "btn-sm btn-danger fw-semibold"))
}

# notice about PC results, shared by several tabs
.ub_pc_notice <- function(pc, state, ns) {
  if (is.null(pc) || identical(pc$status, "no_pc")) return(NULL)
  switch(pc$status,
    ok = .ub_notice("ok", "PC has its concentrations: Workflow 1 (blind as it is)."),
    no_results = .ub_notice("amber", shiny::strong("PC has no concentrations (PCORRES is empty). "),
                            "Did the bioanalytical lab send a CSV or Excel file with the results? If so, copy it to input ",
                            "(Study tab, card 2: click its row to highlight it, then click 'Copy selected files to input') and click Recheck. ",
                            "You can also carry on without it.",
                            shiny::div(shiny::actionButton(ns("recheck_pc"), "Recheck", class = "btn-sm btn-outline-dark mt-1"))),
    lab_file_not_merged = if (isTRUE(tryCatch(.ub_lab_release_on(ub_read_options(state$ub_dir)), error = function(e) FALSE)))
      .ub_notice("info", shiny::strong("Workflow 3: the lab file will be released blinded next to PC"),
                 " (PC itself has no concentrations). ",
                 shiny::actionLink(ns("go_prepare"), "Change this in Prepare PC"))
    else .ub_notice("amber", shiny::strong("PC has no concentrations and a lab results file is in input. "),
                    "Put the results into PC first (Workflow 2).",
                    shiny::div(class = "d-flex flex-wrap gap-2 align-items-center mt-1",
                      shiny::actionButton(ns("go_prepare"), "Go to Prepare PC", class = "btn-sm btn-dark"),
                      shiny::actionLink(ns("go_wf3"), "or, if the results can't be matched to PC, release the lab file blinded (Workflow 3)"))),
    .ub_notice("info", pc$message %||% ""))
}
