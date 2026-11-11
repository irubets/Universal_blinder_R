#' Prepare PC tab (Workflow 2)
#' @keywords internal
.UB_LAB_ROLES <- list(required = c(subject = "Subject ID (full USUBJID or its last part)", visit = "Visit",
                                   nominal_time = "Planned time in hours (matched to PC.PCTPTNUM)",
                                   result = "Concentration as reported (numbers and BLQ<(x) / BQL<(x))"),
                      optional = c(unit = "Unit", lloq = "LLOQ", timepoint_name = "Time-point name (needed for time translations)",
                                   analyte = "Analyte (needed when there are several)", specimen = "Specimen"))

.ub_input_pc <- function(ub_dir) {
  f <- list.files(file.path(ub_dir, "input"), pattern = "^pc\\.(xpt|sas7bdat)$", ignore.case = TRUE, full.names = TRUE)
  if (!length(f)) return(NULL)
  tryCatch(if (grepl("xpt$", f[1], ignore.case = TRUE)) haven::read_xpt(f[1]) else haven::read_sas(f[1]), error = function(e) NULL)
}

.ub_map_table <- function(x, cols) {
  if (!length(x)) return(.ub_df(cols))
  .ub_df(cols, lapply(names(x), function(k) c(k, as.character(x[[k]]))))
}

#' @keywords internal
mod_prepare_pc_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("pc_state")),
    bslib::layout_columns(
      col_widths = c(5, 7),
      bslib::card(
        bslib::card_header("1. Results file and its columns"),
        shiny::selectInput(ns("results_file"), "Results file in input", choices = character(), width = "100%"),
        shiny::textInput(ns("sheet"), "Excel sheet (name or number; blank = first)", value = ""),
        shiny::actionButton(ns("read_cols"), "Read the column names", class = "btn-outline-primary mb-2"),
        shiny::uiOutput(ns("col_selects"))),
      bslib::card(
        bslib::card_header("2. Analytes and translations"),
        shiny::radioButtons(ns("analyte_mode"), NULL, inline = TRUE,
                            choices = c("One analyte" = "one", "Several analytes (parent + metabolite)" = "several")),
        shiny::conditionalPanel(sprintf("input['%s'] == 'one'", ns("analyte_mode")),
          shiny::p(class = "text-muted", "Use the code the other datasets use (ADNCA/ADPC PARAMCD), not the lab's name."),
          bslib::layout_columns(col_widths = c(6, 6),
            shiny::textInput(ns("pctestcd"), "PCTESTCD to put into PC"),
            shiny::textInput(ns("pctest"), "PCTEST to put into PC"))),
        shiny::conditionalPanel(sprintf("input['%s'] == 'several'", ns("analyte_mode")),
          shiny::p(class = "text-muted", "For each analyte name in the lab file, the PCTESTCD that PC already has."),
          DT::DTOutput(ns("analytes")),
          shiny::actionButton(ns("add_analyte"), "Add row", class = "btn-sm btn-outline-primary")),
        shiny::h6(class = "mt-3", "Visit names that differ from PC.VISIT"),
        DT::DTOutput(ns("visit_map")),
        shiny::actionButton(ns("add_visit"), "Add row", class = "btn-sm btn-outline-primary"),
        shiny::h6(class = "mt-3", "Time points whose hours differ from PC.PCTPTNUM (e.g. a Day 2 predose recorded as 24 h)"),
        DT::DTOutput(ns("time_map")),
        shiny::actionButton(ns("add_time"), "Add row", class = "btn-sm btn-outline-primary"),
        shiny::selectInput(ns("check_dataset"), "Cross-check the results against (its AVAL)", choices = character()))
    ),
    bslib::card(
      bslib::card_header("3. Run"),
      shiny::div(class = "d-flex gap-2",
                 shiny::actionButton(ns("save"), "Save settings", class = "btn-outline-primary"),
                 shiny::actionButton(ns("run"), "Put the results into PC", class = "btn-success"),
                 shiny::actionButton(ns("open_report"), "Open highlighted HTML report", class = "btn-outline-secondary")),
      shiny::uiOutput(ns("run_msg")),
      shiny::uiOutput(ns("run_tables"))),
    bslib::card(
      bslib::card_header("4. Workflow 3: the results can't be put into PC"),
      shiny::p(class = "text-muted",
               "When times don't line up or rows are missing on one side, the lab file can be released blinded next to PC ",
               "instead: same subject codes, analyte codes and concentration factors as PC, in the lab's own columns, plus ",
               "PC_UBRECID (the PC record a row matches) and MATCH_STATUS. The recipient merges what matches. It uses the ",
               "file, columns, analytes and translations above."),
      shiny::checkboxInput(ns("lab_release_on"),
                           "The results can't be merged into PC: release the lab file blinded instead (Workflow 3)", width = "100%"),
      shiny::selectizeInput(ns("keep_cols"), "Other lab columns to release (optional; dates are shifted, drug names coded)",
                            choices = character(), multiple = TRUE, width = "100%"),
      shiny::actionButton(ns("check_matching"), "Check matching", class = "btn-outline-primary"),
      shiny::uiOutput(ns("match_ui")))
  )
}

#' @keywords internal
mod_prepare_pc_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    cols <- shiny::reactiveVal(character())
    maps <- shiny::reactiveValues(analytes = .ub_df(c("LAB_NAME", "PCTESTCD")), visit = .ub_df(c("LAB_VISIT", "PC_VISIT")),
                                  time = .ub_df(c("LAB_TIMEPOINT", "HOURS")))
    result <- shiny::reactiveVal(NULL)
    msg <- shiny::reactiveVal(NULL)
    match_res <- shiny::reactiveVal(NULL)

    pc_status <- shiny::reactive({ state$tick; shiny::req(state$ub_dir); tryCatch(ub_check_pc_results(state$ub_dir), error = function(e) NULL) })
    output$pc_state <- shiny::renderUI({
      .ub_need_study(state$ub_dir)
      pc <- pc_status()
      if (is.null(pc) || pc$status == "no_pc") return(.ub_notice("info", "There is no PC dataset in input yet."))
      if (pc$status == "ok" && !isTRUE(pc$merged)) return(.ub_notice("ok", "PC has its concentrations: this step is not needed (Workflow 1)."))
      if (pc$status == "ok") return(.ub_notice("ok", "The lab results are in PC. You can rerun this step: it always starts again from the PC you received (private\\source_original\\pc.xpt)."))
      if (pc$status == "no_results") return(.ub_notice("amber", "PC has no concentrations (PCORRES is empty) and there is no results file in input. ",
        "Did the bioanalytical lab send a CSV or Excel file? If so, copy it to input (tab 'Study') and come back. You can also blind without results."))
      .ub_notice("amber", "PC has no concentrations. Map the results file below and click 'Put the results into PC'.")
    })

    # results files in input may change (copied in the Study tab): refresh the list, keep the choice
    shiny::observeEvent(state$tick, {
      shiny::req(state$ub_dir)
      lf <- tryCatch(ub_lab_files(state$ub_dir), error = function(e) character())
      sel <- if (isTRUE(input$results_file %in% lf)) input$results_file else lf[1]
      shiny::updateSelectInput(session, "results_file", choices = lf, selected = sel)
    }, ignoreInit = TRUE)
    # load the saved settings only when another study is opened (not after each run: that would drop unsaved rows)
    shiny::observeEvent(state$ub_dir, {
      shiny::req(state$ub_dir)
      lf <- tryCatch(ub_lab_files(state$ub_dir), error = function(e) character())
      s <- tryCatch(ub_read_options(state$ub_dir)$prepare_pc, error = function(e) NULL) %||% list()
      shiny::updateSelectInput(session, "results_file", choices = lf, selected = s$results_file %||% lf[1])
      ds <- list.files(file.path(state$ub_dir, "input"), pattern = "^(adnca|adpc)\\.(xpt|sas7bdat)$", ignore.case = TRUE)
      shiny::updateSelectInput(session, "check_dataset", choices = c("(none)" = "", ds), selected = s$check_dataset %||% ds[1])
      if (length(s)) {
        shiny::updateTextInput(session, "sheet", value = as.character(s$sheet %||% ""))
        shiny::updateTextInput(session, "pctestcd", value = s$pctestcd %||% "")
        shiny::updateTextInput(session, "pctest", value = s$pctest %||% "")
        shiny::updateRadioButtons(session, "analyte_mode", selected = if (length(s$analytes)) "several" else "one")
        maps$analytes <- .ub_map_table(s$analytes, c("LAB_NAME", "PCTESTCD"))
        maps$visit <- .ub_map_table(s$visit_map, c("LAB_VISIT", "PC_VISIT"))
        maps$time <- .ub_map_table(s$time_map, c("LAB_TIMEPOINT", "HOURS"))
      } else {
        pc <- .ub_input_pc(state$ub_dir)
        ad <- tryCatch({
          f <- file.path(state$ub_dir, "input", ds[1]); d <- if (grepl("xpt$", f, ignore.case = TRUE)) haven::read_xpt(f) else haven::read_sas(f)
          unique(as.character(d$PARAMCD))
        }, error = function(e) character())
        if (length(ad) == 1) { shiny::updateTextInput(session, "pctestcd", value = ad); shiny::updateTextInput(session, "pctest", value = ad) }
        else if (!is.null(pc) && "PCTESTCD" %in% names(pc)) shiny::updateTextInput(session, "pctestcd", value = unique(pc$PCTESTCD)[1])
      }
      if (length(lf) && !is.null(s$columns)) read_cols(s$results_file %||% lf[1], s$sheet, s$columns)
      lr <- tryCatch(ub_read_options(state$ub_dir)$lab_release, error = function(e) NULL)
      shiny::updateCheckboxInput(session, "lab_release_on", value = isTRUE(as.logical(unlist(lr$enabled))[1]))
      match_res(NULL)
    })

    read_cols <- function(file, sheet = NULL, preset = NULL) {
      h <- tryCatch(ub_prepare_pc_columns(state$ub_dir, file, if (is.null(sheet) || !nzchar(sheet)) NULL else sheet),
                    error = function(e) { msg(.ub_notice("red", conditionMessage(e))); character() })
      cols(h)
      kept <- tryCatch(unlist(ub_read_options(state$ub_dir)$lab_release$keep_columns), error = function(e) NULL)
      hint <- ifelse(grepl("ID|ACCESSION|BARCODE", h, ignore.case = TRUE), paste0(h, " (an ID? keep only if needed)"), h)
      shiny::updateSelectizeInput(session, "keep_cols", choices = stats::setNames(h, hint), selected = intersect(kept, h))
      guess <- function(role) {
        if (!is.null(preset[[role]])) return(preset[[role]])
        pat <- c(subject = "SUBJ|USUBJID|PATIENT", visit = "VISIT", nominal_time = "NOMINAL|HOUR|NOM",
                 result = "CONC|RESULT", unit = "UNIT", lloq = "LLOQ", timepoint_name = "TIME.?POINT|TPT",
                 analyte = "ANALYTE", specimen = "SPEC|MATRIX")[[role]]
        m <- grep(pat, h, ignore.case = TRUE, value = TRUE)
        if (length(m)) m[1] else ""
      }
      output$col_selects <- shiny::renderUI(shiny::tagList(
        shiny::h6("Required"),
        lapply(names(.UB_LAB_ROLES$required), function(r) shiny::selectInput(ns(paste0("col_", r)), .UB_LAB_ROLES$required[[r]],
                                                                            choices = c("(choose)" = "", h), selected = guess(r))),
        shiny::h6("Optional"),
        lapply(names(.UB_LAB_ROLES$optional), function(r) shiny::selectInput(ns(paste0("col_", r)), .UB_LAB_ROLES$optional[[r]],
                                                                            choices = c("(none)" = "", h), selected = guess(r)))))
    }
    shiny::observeEvent(input$read_cols, { shiny::req(input$results_file); read_cols(input$results_file, input$sheet) })

    edit_tbl <- function(key, out_id, add_id) {
      output[[out_id]] <- DT::renderDT(.ub_dt(maps[[key]], editable = "cell", page_length = 10))
      shiny::observeEvent(input[[paste0(out_id, "_cell_edit")]], maps[[key]] <- .ub_apply_edit(maps[[key]], input[[paste0(out_id, "_cell_edit")]]))
      shiny::observeEvent(input[[add_id]], {
        d <- maps[[key]]
        maps[[key]] <- rbind(d[, setdiff(names(d), ".status"), drop = FALSE],
                             as.data.frame(as.list(setNames(c("", ""), setdiff(names(d), ".status"))), stringsAsFactors = FALSE))
      })
    }
    edit_tbl("analytes", "analytes", "add_analyte")
    edit_tbl("visit", "visit_map", "add_visit")
    edit_tbl("time", "time_map", "add_time")

    settings <- function() {
      roles <- c(names(.UB_LAB_ROLES$required), names(.UB_LAB_ROLES$optional))
      cl <- lapply(roles, function(r) input[[paste0("col_", r)]] %||% "")
      names(cl) <- roles
      cl <- cl[nzchar(unlist(cl))]
      tomap <- function(d, num = FALSE) {
        d <- d[nzchar(trimws(d[[1]])), , drop = FALSE]
        if (!nrow(d)) return(setNames(list(), character()))
        setNames(lapply(d[[2]], function(v) if (num) suppressWarnings(as.numeric(v)) else trimws(v)), trimws(d[[1]]))
      }
      s <- list(results_file = input$results_file, sheet = if (nzchar(input$sheet)) input$sheet else NULL, columns = cl,
                visit_map = tomap(maps$visit), time_map = tomap(maps$time, num = TRUE),
                check_dataset = if (nzchar(input$check_dataset %||% "")) input$check_dataset else NULL)
      if (input$analyte_mode == "one") { s$pctestcd <- trimws(input$pctestcd); s$pctest <- trimws(input$pctest) }
      else s$analytes <- tomap(maps$analytes)
      s[!vapply(s, is.null, TRUE)]
    }
    save <- function() {
      o <- ub_read_options(state$ub_dir); o$prepare_pc <- settings()
      if (!is.null(input$lab_release_on))
        o$lab_release <- list(enabled = isTRUE(input$lab_release_on), keep_columns = as.list(as.character(input$keep_cols)))
      ub_write_options(state$ub_dir, o)
    }
    # Workflow 3 switch: saved as soon as it changes (the rest of the settings with 'Save settings' or a run)
    shiny::observeEvent(list(input$lab_release_on, input$keep_cols), {
      shiny::req(state$ub_dir, !is.null(input$lab_release_on))
      o <- ub_read_options(state$ub_dir)
      keep <- if (is.null(input$keep_cols)) unlist(o$lab_release$keep_columns) else input$keep_cols
      new <- list(enabled = isTRUE(input$lab_release_on), keep_columns = as.list(as.character(keep)))
      if (identical(.ub_lab_release_on(o), new$enabled) && setequal(unlist(o$lab_release$keep_columns), unlist(new$keep_columns))) return()
      o$lab_release <- new; ub_write_options(state$ub_dir, o)
      state$settings_reload <- Sys.time()
    }, ignoreInit = TRUE)
    # Check matching: how the lab rows match PC, from the saved settings; writes nothing
    run_check <- function() match_res(tryCatch(ub_lab_release_prepare(state$ub_dir, write = FALSE), error = function(e) e))
    shiny::observeEvent(input$check_matching, { .ub_need_study(state$ub_dir); save(); run_check() })
    output$match_ui <- shiny::renderUI({
      p <- match_res(); if (is.null(p)) return(NULL)
      if (inherits(p, "error")) return(.ub_notice("red", conditionMessage(p)))
      k <- p$counts
      lo <- p$left_out
      shiny::tagList(
        .ub_notice(if (k[["matched"]] >= k[["rows"]] / 2) "ok" else "amber",
          shiny::strong(sprintf("%d lab row(s) would be released: %d matched to PC, %d with no PC record at this visit/time, %d with several PC records. ",
                                k[["rows"]], k[["matched"]], k[["no_pc"]], k[["several"]])),
          sprintf("%d row(s) left out (subject or analyte not in PC).", k[["left_out"]]),
          if (!.ub_lab_release_on(ub_read_options(state$ub_dir))) shiny::div("Workflow 3 is off: turn the switch on to release the lab file at the next run.")),
        if (nrow(lo)) shiny::tags$table(class = "table table-sm small",
          shiny::tags$tr(lapply(c("Lab row", "Subject", "Analyte", "Why left out"), shiny::tags$th)),
          lapply(seq_len(nrow(lo)), function(i) shiny::tags$tr(lapply(c(lo$lab_row[i], lo$subject[i], lo$analyte[i], lo$reason[i]),
                                                                    function(v) shiny::tags$td(as.character(v)))))))
    })
    # turn Workflow 3 on (from the prompt after an unmatched run, or from another tab) and show the matching
    enable_wf3 <- function() {
      o <- ub_read_options(state$ub_dir); o$lab_release$enabled <- TRUE
      if (is.null(o$lab_release$keep_columns)) o$lab_release$keep_columns <- list()
      ub_write_options(state$ub_dir, o)
      shiny::updateCheckboxInput(session, "lab_release_on", value = TRUE)
      state$settings_reload <- Sys.time(); state$refresh()
      run_check()
    }
    shiny::observeEvent(input$to_workflow3, { .ub_need_study(state$ub_dir); enable_wf3() })
    shiny::observeEvent(state$wf3_request, { shiny::req(state$ub_dir); enable_wf3() }, ignoreInit = TRUE)
    shiny::observeEvent(input$save, { .ub_need_study(state$ub_dir); save(); msg(.ub_notice("ok", "Settings saved in blind_options.json.")) })

    shiny::observeEvent(input$run, {
      .ub_need_study(state$ub_dir)
      s <- settings()
      miss <- setdiff(names(.UB_LAB_ROLES$required), names(s$columns))
      if (length(miss)) return(msg(.ub_notice("red", "Choose the column for: ", paste(miss, collapse = ", "))))
      save()
      r <- shiny::withProgress(message = "Putting the results into PC",
                               tryCatch(ub_prepare_pc(state$ub_dir, s), error = function(e) list(ok = FALSE, written = FALSE, messages = conditionMessage(e))))
      rep <- file.path(state$ub_dir, "private", "reports", "prepare_pc_report.html")
      try(ub_prepare_pc_report(r, rep), silent = TRUE)
      result(r)
      # unmatched visits / time points become red rows to fill in
      uv <- setdiff(r$unmatched_values$visits %||% character(), maps$visit$LAB_VISIT)
      if (length(uv)) { v <- maps$visit[, c("LAB_VISIT", "PC_VISIT")]; v <- rbind(v, data.frame(LAB_VISIT = uv, PC_VISIT = "")); v$.status <- ifelse(v$PC_VISIT == "", "red", "ok"); maps$visit <- v }
      ut <- setdiff(r$unmatched_values$timepoints %||% character(), maps$time$LAB_TIMEPOINT)
      if (length(ut)) { t <- maps$time[, c("LAB_TIMEPOINT", "HOURS")]; t <- rbind(t, data.frame(LAB_TIMEPOINT = ut, HOURS = "")); t$.status <- ifelse(t$HOURS == "", "red", "ok"); maps$time <- t }
      nu <- NROW(r$unmatched)
      msg(shiny::tagList(
        .ub_notice(if (isTRUE(r$written)) "ok" else "red",
                   if (isTRUE(r$written)) shiny::strong("Results put into PC. ") else shiny::strong("Nothing was written. "),
                   shiny::tags$ul(lapply(r$messages %||% character(), shiny::tags$li)),
                   if (length(uv) || length(ut)) "Unmatched visits/time points were added in red to the tables above: fill them in and run again.",
                   if (isTRUE(r$written)) shiny::div("Next: tab 'Blind & QC'.")),
        # the results could not be matched: propose Workflow 3 (not after a settings error)
        if (!isTRUE(r$written) && nu > 0)
          .ub_notice("amber", sprintf("%d of %d lab rows could not be matched to PC. ", nu, r$n_lab %||% nu),
                     "If translations (visit names, time points) can't fix this, release the lab file blinded instead ",
                     "(Workflow 3): the recipient merges what matches.",
                     shiny::div(shiny::actionButton(ns("to_workflow3"), "Switch to Workflow 3", class = "btn-sm btn-dark mt-1")))))
      state$refresh()
    })
    shiny::observeEvent(input$open_report, .ub_open_path(file.path(shiny::req(state$ub_dir), "private", "reports", "prepare_pc_report.html")))
    output$run_msg <- shiny::renderUI(msg())
    output$run_tables <- shiny::renderUI({
      r <- result()
      if (is.null(r)) return(NULL)
      tab <- function(title, d, st) {
        if (is.null(d) || !NROW(d)) return(NULL)
        d <- as.data.frame(d); if (is.null(d$.status)) d$.status <- st
        bslib::nav_panel(paste0(title, " (", nrow(d), ")"), DT::renderDT(.ub_dt(d, page_length = 10)))
      }
      nr <- r$no_result
      if (!is.null(nr) && NROW(nr)) nr$.status <- ifelse(nr$status == "amber", "amber", "ok")
      av <- r$aval_check
      if (!is.null(av) && NROW(av)) av$.status <- ifelse(av$status == "red", "red", "ok")
      p <- Filter(Negate(is.null), list(tab("Not matched", r$unmatched, "red"), tab("Differs from PC", r$conflicts, "red"),
                                        tab("PC records without a result", nr, "ok"), tab("Check against AVAL", av, "ok")))
      if (!length(p)) return(NULL)
      do.call(bslib::navset_tab, p)
    })
  })
}
