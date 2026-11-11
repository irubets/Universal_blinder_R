#' Study settings tab
#'
#' Each settings key is edited as a small table; the conversion to and from blind_options.json is done by
#' `.ub_opts_to_tables()` / `.ub_tables_to_opts()` (pure functions, tested).
#' @keywords internal
.UB_SETTING_TABLES <- list(
  dataset_aliases = list(title = "Dataset aliases", cols = c("FILE", "PROFILE"),
    help = "A dataset file whose name has no profile but the structure of one that has, e.g. ADCONC -> ADNCA. Profiles: ADSL DM EX EC PC PP VS ADEX ADEC ADNCA ADPC ADPD ADPP (SUPPxx automatic)."),
  treatment_aliases = list(title = "Treatment abbreviations", cols = c("TREATMENT", "ABBREVIATIONS"),
    help = "Abbreviations of a drug used in free text: the EXTRT value, and its abbreviations separated by commas."),
  analyte_aliases = list(title = "Analyte abbreviations", cols = c("PCTESTCD", "ABBREVIATIONS"),
    help = "Abbreviations of an analyte (also short names used in PP.PPCAT / ADPP.ATEST), separated by commas."),
  analyte_code_aliases = list(title = "Other codes of an analyte", cols = c("OTHER_CODE", "PCTESTCD"),
    help = "Another code for an analyte, e.g. an ADNCA PARAMCD that differs from PC.PCTESTCD."),
  private_terms = list(title = "Other private text", cols = c("TERM"),
    help = "Any other study-identifying text to replace everywhere (one per row)."),
  rename_vars = list(title = "Rename variables", cols = c("DATASET", "OLD_NAME", "NEW_NAME"),
    help = "Variables whose NAME contains a drug fragment, e.g. ADEX DOSEABC -> DOSET01."),
  extra_suppress_vars = list(title = "Blank variables", cols = c("DATASET", "VARIABLES"),
    help = "Extra variables to blank; DATASET '*' = every dataset; wildcards * and ?; several separated by commas."),
  drop_vars = list(title = "Leave variables out of the release", cols = c("DATASET", "VARIABLES"),
    help = "Variables left out of the released datasets (they come back on retrieval), e.g. * RACE, ETHNIC."),
  blank_where = list(title = "Blank where a condition holds", cols = c("DATASETS", "VARIABLES", "WHEN_VARIABLE", "WHEN_VALUES"),
    help = "Blank variables on rows where a variable of the ORIGINAL data has one of the values (default: planned doses in mg/m2, which with the dose in mg give the body surface area)."),
  qc_waivers = list(title = "QC waivers", cols = c("WAIVER", "REASON", "REVIEWER"),
    help = "Only after a documented review of the QC finding: <check>:<DATASET>.<VARIABLE>, with the reason and who reviewed it.")
)

.ub_df <- function(cols, rows = list()) {
  d <- as.data.frame(setNames(replicate(length(cols), character(), simplify = FALSE), cols), stringsAsFactors = FALSE)
  if (length(rows)) d <- rbind(d, do.call(rbind, lapply(rows, function(r) as.data.frame(as.list(setNames(as.character(r), cols)), stringsAsFactors = FALSE))))
  d
}

.ub_opts_to_tables <- function(o) {
  idn <- o$identities %||% list()
  j <- function(x) paste(unlist(x), collapse = ", ")
  map2 <- function(x, cols, f = function(v) j(v)) .ub_df(cols, lapply(names(x), function(k) c(k, f(x[[k]]))))
  list(
    dataset_aliases = map2(o$dataset_aliases, .UB_SETTING_TABLES$dataset_aliases$cols),
    treatment_aliases = map2(idn$treatment_aliases, .UB_SETTING_TABLES$treatment_aliases$cols),
    analyte_aliases = map2(idn$analyte_aliases, .UB_SETTING_TABLES$analyte_aliases$cols),
    analyte_code_aliases = map2(idn$analyte_code_aliases, .UB_SETTING_TABLES$analyte_code_aliases$cols),
    private_terms = .ub_df("TERM", lapply(unlist(idn$private_terms), function(t) t)),
    rename_vars = .ub_df(.UB_SETTING_TABLES$rename_vars$cols, unlist(lapply(names(o$rename_vars), function(ds)
      lapply(names(o$rename_vars[[ds]]), function(v) c(ds, v, o$rename_vars[[ds]][[v]]))), recursive = FALSE)),
    extra_suppress_vars = map2(o$extra_suppress_vars, c("DATASET", "VARIABLES")),
    drop_vars = map2(o$drop_vars, c("DATASET", "VARIABLES")),
    blank_where = .ub_df(.UB_SETTING_TABLES$blank_where$cols, lapply(o$blank_where, function(b) {
      w <- names(b$when)[1] %||% ""
      c(j(b$datasets), j(b$vars), w, if (nzchar(w)) j(b$when[[w]]) else "")
    })),
    qc_waivers = .ub_df(.UB_SETTING_TABLES$qc_waivers$cols, lapply(unlist(o$qc_waivers), function(w) {
      n <- o$qc_waiver_notes[[w]] %||% list()
      c(w, n$reason %||% "", n$reviewer %||% "")
    })))
}

.ub_tables_to_opts <- function(tb, o, overrides_json = NULL) {
  keep <- function(d) d[apply(d, 1, function(r) nzchar(trimws(r[1]))), , drop = FALSE]
  named <- function(d, f = .ub_split_list) {
    d <- keep(d)
    x <- setNames(lapply(d[[2]], f), trimws(d[[1]]))
    if (!length(x)) setNames(list(), character()) else x
  }
  o$dataset_aliases <- named(tb$dataset_aliases, function(v) toupper(trimws(v)))
  o$identities <- list(
    treatment_aliases = named(tb$treatment_aliases, function(v) as.list(.ub_split_list(v))),
    analyte_aliases = named(tb$analyte_aliases, function(v) as.list(.ub_split_list(v))),
    analyte_code_aliases = named(tb$analyte_code_aliases, trimws),
    private_terms = as.list(trimws(keep(tb$private_terms)$TERM)))
  rv <- keep(tb$rename_vars)
  o$rename_vars <- if (!nrow(rv)) setNames(list(), character()) else
    lapply(split(rv, toupper(trimws(rv$DATASET))), function(d) setNames(as.list(toupper(trimws(d$NEW_NAME))), toupper(trimws(d$OLD_NAME))))
  o$extra_suppress_vars <- named(tb$extra_suppress_vars, function(v) as.list(.ub_split_list(v)))
  o$drop_vars <- named(tb$drop_vars, function(v) as.list(.ub_split_list(v)))
  bw <- keep(tb$blank_where)
  o$blank_where <- lapply(seq_len(nrow(bw)), function(i) {
    b <- list(datasets = as.list(.ub_split_list(bw$DATASETS[i])), vars = as.list(.ub_split_list(bw$VARIABLES[i])))
    if (nzchar(trimws(bw$WHEN_VARIABLE[i]))) b$when <- setNames(list(as.list(.ub_split_list(bw$WHEN_VALUES[i]))), trimws(bw$WHEN_VARIABLE[i]))
    b
  })
  qw <- keep(tb$qc_waivers)
  o$qc_waivers <- as.list(trimws(qw$WAIVER))
  o$qc_waiver_notes <- if (!nrow(qw)) setNames(list(), character()) else
    setNames(lapply(seq_len(nrow(qw)), function(i) list(reason = trimws(qw$REASON[i]), reviewer = trimws(qw$REVIEWER[i]),
                                                        date = format(Sys.Date()))), trimws(qw$WAIVER))
  if (!is.null(overrides_json)) {
    txt <- trimws(overrides_json)
    o$config_overrides <- if (!nzchar(txt)) setNames(list(), character()) else jsonlite::fromJSON(txt, simplifyVector = FALSE)
  }
  o
}

# guided switches for config_overrides: list(label, path (list of keys), value)
.UB_OVERRIDE_SWITCHES <- list(
  pctest_adnca = list(label = "ADNCA carries PCTESTCD/PCTEST: blind them like the analyte (recommended)",
                      path = c("datasets", "ADNCA", "exact_vars"), value = list(PCTESTCD = "analyte_code", PCTEST = "analyte_name")),
  pctest_adpc = list(label = "ADPC carries PCTESTCD/PCTEST: blind them like the analyte (recommended)",
                     path = c("datasets", "ADPC", "exact_vars"), value = list(PCTESTCD = "analyte_code", PCTEST = "analyte_name")),
  precision = list(label = "ADNCA has PCORRES but no ARESC: round AVAL like PCORRES",
                   path = c("datasets", "ADNCA", "analyte_numeric", "precision_from"), value = "PCORRES"),
  pc_subjid = list(label = "PC carries SUBJID: blind it as a subject ID",
                   path = c("datasets", "PC", "id_vars", "subjid"), value = "SUBJID"),
  adnca_text = list(label = "ADNCA labels name the drug or placebo (EXTRT, TRTP, TRTA, APROFILE, PARCAT1): treat them as text",
                    path = c("datasets", "ADNCA", "text_vars"),
                    value = list(EXTRT = "treatment", TRTP = "treatment", TRTA = "treatment", APROFILE = "any", PARCAT1 = "any")))

.ub_get_path <- function(x, path) { for (p in path) { if (is.null(x[[p]])) return(NULL); x <- x[[p]] }; x }
.ub_set_path <- function(x, path, value) {
  if (length(path) == 1) { x[[path]] <- value; return(x) }
  sub <- x[[path[1]]] %||% setNames(list(), character())
  x[[path[1]]] <- .ub_set_path(sub, path[-1], value)
  x
}
.ub_switch_on <- function(ov, sw) {
  cur <- .ub_get_path(ov, sw$path)
  if (is.null(cur)) return(FALSE)
  if (is.list(sw$value)) all(names(sw$value) %in% names(cur)) else identical(as.character(cur), as.character(sw$value))
}
.ub_switch_apply <- function(ov, sw, on) {
  if (on) {
    cur <- .ub_get_path(ov, sw$path)
    val <- if (is.list(sw$value) && is.list(cur)) utils::modifyList(cur, sw$value) else sw$value
    return(.ub_set_path(ov, sw$path, val))
  }
  if (is.list(sw$value)) {
    cur <- .ub_get_path(ov, sw$path)
    if (is.null(cur)) return(ov)
    cur[names(sw$value)] <- NULL
    return(.ub_set_path(ov, sw$path, if (length(cur)) cur else NULL))
  }
  .ub_set_path(ov, sw$path, NULL)
}

#' @keywords internal
mod_settings_ui <- function(id) {
  ns <- shiny::NS(id)
  panels <- lapply(names(.UB_SETTING_TABLES), function(k) {
    s <- .UB_SETTING_TABLES[[k]]
    bslib::accordion_panel(s$title, value = k,
      shiny::p(class = "text-muted", s$help),
      DT::DTOutput(ns(paste0("tb_", k))),
      shiny::div(class = "mt-1",
                 shiny::actionButton(ns(paste0("add_", k)), "Add row", class = "btn-sm btn-outline-primary"),
                 shiny::actionButton(ns(paste0("del_", k)), "Delete selected rows", class = "btn-sm btn-outline-danger")))
  })
  panels <- c(panels, list(bslib::accordion_panel("Configuration changes for this study", value = "config_overrides",
    shiny::p(class = "text-muted", "Common cases as switches; anything else in the JSON box (checked when you save)."),
    shiny::uiOutput(ns("switches")),
    shiny::textAreaInput(ns("overrides_json"), "config_overrides (JSON)", rows = 12, width = "100%"))))
  bslib::card(
    bslib::card_header("Study settings (blind_options.json)"),
    shiny::p("Most studies need few or none of these. The pre-flight (tab 'Blind & QC') and the AI assistant suggest",
             "settings when a study needs them. Double-click a cell to edit; lists are separated by commas."),
    do.call(bslib::accordion, c(list(id = ns("acc"), open = FALSE), panels)),
    shiny::div(class = "mt-2",
               shiny::actionButton(ns("save"), "Save settings", class = "btn-primary"),
               shiny::actionButton(ns("reload"), "Discard changes", class = "btn-outline-secondary")),
    shiny::uiOutput(ns("msg")))
}

#' @keywords internal
mod_settings_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tables <- shiny::reactiveValues()
    opts <- shiny::reactiveVal(NULL)
    msg <- shiny::reactiveVal(NULL)
    load <- function() {
      if (is.null(state$ub_dir)) return()
      o <- tryCatch(ub_read_options(state$ub_dir), error = function(e) { msg(.ub_notice("red", conditionMessage(e))); NULL })
      if (is.null(o)) return()
      opts(o)
      tb <- .ub_opts_to_tables(o)
      for (k in names(tb)) tables[[k]] <- tb[[k]]
      shiny::updateTextAreaInput(session, "overrides_json",
                                 value = jsonlite::toJSON(o$config_overrides %||% setNames(list(), character()), auto_unbox = TRUE, pretty = TRUE))
    }
    shiny::observeEvent(state$ub_dir, load())
    shiny::observeEvent(state$settings_reload, load(), ignoreInit = TRUE)
    shiny::observeEvent(input$reload, { load(); msg(NULL) })

    for (k in names(.UB_SETTING_TABLES)) local({
      key <- k
      output[[paste0("tb_", key)]] <- DT::renderDT({
        .ub_need_study(state$ub_dir)
        .ub_dt(tables[[key]], editable = "cell", selection = "multiple", page_length = 10)
      })
      shiny::observeEvent(input[[paste0("tb_", key, "_cell_edit")]],
                          tables[[key]] <- .ub_apply_edit(tables[[key]], input[[paste0("tb_", key, "_cell_edit")]]))
      shiny::observeEvent(input[[paste0("add_", key)]], {
        d <- tables[[key]]
        tables[[key]] <- rbind(d, as.data.frame(as.list(setNames(rep("", ncol(d)), names(d))), stringsAsFactors = FALSE))
      })
      shiny::observeEvent(input[[paste0("del_", key)]], {
        sel <- input[[paste0("tb_", key, "_rows_selected")]]
        if (length(sel)) tables[[key]] <- tables[[key]][-sel, , drop = FALSE]
      })
    })

    ov <- shiny::reactive(tryCatch(jsonlite::fromJSON(input$overrides_json %||% "{}", simplifyVector = FALSE), error = function(e) NULL))
    output$switches <- shiny::renderUI({
      o <- opts()$config_overrides %||% list()
      shiny::tagList(lapply(names(.UB_OVERRIDE_SWITCHES), function(k)
        shiny::checkboxInput(ns(paste0("sw_", k)), .UB_OVERRIDE_SWITCHES[[k]]$label, value = .ub_switch_on(o, .UB_OVERRIDE_SWITCHES[[k]]))))
    })
    for (k in names(.UB_OVERRIDE_SWITCHES)) local({
      key <- k
      shiny::observeEvent(input[[paste0("sw_", key)]], {
        o <- ov()
        if (is.null(o)) return(msg(.ub_notice("red", "The JSON box is not valid JSON; fix it before using the switches.")))
        if (.ub_switch_on(o, .UB_OVERRIDE_SWITCHES[[key]]) == isTRUE(input[[paste0("sw_", key)]])) return()
        o <- .ub_switch_apply(o, .UB_OVERRIDE_SWITCHES[[key]], isTRUE(input[[paste0("sw_", key)]]))
        shiny::updateTextAreaInput(session, "overrides_json",
                                   value = jsonlite::toJSON(if (length(o)) o else setNames(list(), character()), auto_unbox = TRUE, pretty = TRUE))
      }, ignoreInit = TRUE)
    })

    shiny::observeEvent(input$save, {
      .ub_need_study(state$ub_dir)
      if (is.null(ov())) return(msg(.ub_notice("red", "config_overrides is not valid JSON: nothing was saved.")))
      tb <- shiny::reactiveValuesToList(tables)
      o <- tryCatch(.ub_tables_to_opts(tb, ub_read_options(state$ub_dir), input$overrides_json), error = function(e) e)
      if (inherits(o, "error")) return(msg(.ub_notice("red", conditionMessage(o))))
      p <- ub_validate_options(o)
      if (length(p)) return(msg(.ub_notice("red", "Not saved: ", shiny::tags$ul(lapply(p, shiny::tags$li)))))
      ub_write_options(state$ub_dir, o)
      opts(o)
      msg(.ub_notice("ok", "Saved. The previous version is in private\\settings_backup."))
      state$refresh()
    })
    output$msg <- shiny::renderUI(msg())
  })
}
