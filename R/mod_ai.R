#' AI assistant tab
#' @keywords internal
mod_ai_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(4, 8),
    bslib::card(
      bslib::card_header("Connection"),
      shiny::radioButtons(ns("provider"), "Model", choices = c("Claude (Anthropic API, cloud)" = "claude",
                                                              "Local model: Ollama" = "ollama",
                                                              "Local model: OpenAI-compatible server (LM Studio, vLLM, llama.cpp)" = "openai")),
      shiny::conditionalPanel(sprintf("input['%s'] == 'claude'", ns("provider")),
        shiny::uiOutput(ns("key_state")),
        shiny::passwordInput(ns("api_key"), "API key (this session only; never saved)", width = "100%"),
        shiny::selectizeInput(ns("claude_model"), "Claude model", choices = NULL, options = list(create = TRUE))),
      shiny::conditionalPanel(sprintf("input['%s'] != 'claude'", ns("provider")),
        shiny::textInput(ns("base_url"), "Server address", value = "http://localhost:11434", width = "100%"),
        shiny::passwordInput(ns("local_key"), "Key (OpenAI-compatible servers only, if required)"),
        shiny::actionButton(ns("list_models"), "List installed models", class = "btn-sm btn-outline-secondary mb-2"),
        shiny::selectizeInput(ns("local_model"), "Local model", choices = NULL, options = list(create = TRUE))),
      shiny::actionButton(ns("test"), "Test connection", class = "btn-outline-primary"),
      shiny::uiOutput(ns("conn_msg")),
      shiny::hr(),
      shiny::h6("What may be sent"),
      shiny::tags$ul(class = "small",
        shiny::tags$li(shiny::strong("Claude (cloud): "), "the user guide, column names and counts, and messages with every real",
                       " name, ID and date replaced by placeholders; the blinded datasets in output (after QC PASS/WARN and",
                       " your one-time confirmation). Every request is checked for real values first and blocked if one is found."),
        shiny::tags$li(shiny::strong("Local model: "), "the same plus real values (codebook names, sample values), because",
                       " nothing leaves this computer."),
        shiny::tags$li("Every request is logged in private\\logs\\ai_log.jsonl. The API key is never logged."))),
    bslib::card(
      bslib::card_header("Ask"),
      shiny::selectInput(ns("task"), "Task", choices = NULL, width = "100%"),
      shiny::uiOutput(ns("task_help")),
      shiny::textAreaInput(ns("question"), "Your question or extra instructions (optional)", rows = 3, width = "100%"),
      shiny::uiOutput(ns("context_info")),
      shiny::div(class = "d-flex gap-2",
                 shiny::actionButton(ns("preview"), "Preview what will be sent", class = "btn-outline-primary"),
                 shiny::actionButton(ns("send"), "Send", class = "btn-primary")),
      shiny::uiOutput(ns("preview_box")),
      shiny::uiOutput(ns("answer")),
      shiny::uiOutput(ns("suggestions")))
  )
}

#' @keywords internal
mod_ai_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    conn_msg <- shiny::reactiveVal(NULL)
    prep <- shiny::reactiveVal(NULL)
    reply <- shiny::reactiveVal(NULL)
    send_err <- shiny::reactiveVal(NULL)   # error of the last Send; cleared when the model changes or on a new Send

    tasks <- tryCatch(ub_ai_tasks(), error = function(e) NULL)
    if (!is.null(tasks)) shiny::updateSelectInput(session, "task", choices = setNames(tasks$task, tasks$label))
    shiny::updateSelectizeInput(session, "claude_model", choices = ub_claude_models(), selected = ub_claude_models()[1])
    output$key_state <- shiny::renderUI(
      if (nzchar(Sys.getenv("ANTHROPIC_API_KEY"))) .ub_notice("ok", "Using the key in ANTHROPIC_API_KEY (from .Renviron).")
      else shiny::p(class = "small text-muted", "No ANTHROPIC_API_KEY set: type the key below, or add it to your .Renviron."))
    # messages about another model are stale once the model changes
    shiny::observeEvent(list(input$provider, input$claude_model, input$local_model, input$base_url), {
      send_err(NULL); conn_msg(NULL)
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$provider, {
      if (input$provider == "openai" && grepl("11434", input$base_url)) shiny::updateTextInput(session, "base_url", value = "http://localhost:1234/v1")
      if (input$provider == "ollama" && !grepl("11434", input$base_url)) shiny::updateTextInput(session, "base_url", value = "http://localhost:11434")
    })

    conn <- shiny::reactive({
      if (input$provider == "claude")
        ub_llm_connect("claude", model = input$claude_model, api_key = if (nzchar(input$api_key)) input$api_key else NULL)
      else ub_llm_connect(input$provider, model = input$local_model, base_url = input$base_url,
                          api_key = if (nzchar(input$local_key)) input$local_key else NULL)
    })
    shiny::observeEvent(input$list_models, {
      m <- tryCatch(ub_llm_list_models(ub_llm_connect(input$provider, base_url = input$base_url,
                                                      api_key = if (nzchar(input$local_key)) input$local_key else NULL)),
                    error = function(e) { conn_msg(.ub_notice("red", conditionMessage(e))); character() })
      if (length(m)) { shiny::updateSelectizeInput(session, "local_model", choices = m, selected = m[1]); conn_msg(.ub_notice("ok", length(m), " model(s) found.")) }
    })
    shiny::observeEvent(input$test, {
      r <- tryCatch(ub_llm_test(conn()), error = function(e) list(ok = FALSE, message = conditionMessage(e)))
      conn_msg(.ub_notice(if (isTRUE(r$ok)) "ok" else "red", r$message %||% "",
                          if (isTRUE(r$ok)) sprintf(" (%s, %.1f s)", r$model %||% "", r$latency_s %||% NA)))
    })
    output$conn_msg <- shiny::renderUI(conn_msg())

    filled_q <- FALSE
    shiny::observeEvent(state$ai_context, {
      ctx <- state$ai_context
      if (!is.null(ctx$task)) shiny::updateSelectInput(session, "task", selected = ctx$task)
      # a question filled in by another tab is replaced (or cleared) by the next context, never kept by accident
      if (!is.null(ctx$question) || isTRUE(filled_q)) shiny::updateTextAreaInput(session, "question", value = ctx$question %||% "")
      filled_q <<- !is.null(ctx$question)
      if (identical(ctx$task, "codebook") && input$provider == "claude")
        shiny::updateRadioButtons(session, "provider", selected = "ollama")
    })
    output$task_help <- shiny::renderUI({
      shiny::req(tasks, input$task)
      t <- tasks[tasks$task == input$task, ]
      shiny::tagList(shiny::p(class = "text-muted", t$description),
                     if (!t$claude_allowed && input$provider == "claude")
                       .ub_notice("amber", "This task needs real names and runs only with a local model. Choose a local model on the left."))
    })
    output$context_info <- shiny::renderUI({
      f <- state$ai_context$finding
      if (isTRUE(state$ai_context$release_failed))
        return(shiny::p(class = "small", shiny::strong("Included: "), "the QC findings of the last run (masked for Claude). ",
                        "The log is not sent: add a line from it to the question if needed."))
      if (is.null(f)) return(NULL)
      shiny::p(class = "small", shiny::strong("Included: "), "the QC finding ", shiny::code(paste(f$check, f$dataset, f$variable)),
               " (masked for Claude).")
    })

    do_prepare <- function() {
      .ub_need_study(state$ub_dir)
      p <- tryCatch(ub_ai_prepare(input$task, state$ub_dir, conn(), question = input$question,
                                  context = state$ai_context %||% list()), error = function(e) e)
      if (inherits(p, "error")) { prep(NULL); output$preview_box <- shiny::renderUI(.ub_notice("red", conditionMessage(p))); return(NULL) }
      prep(p); p
    }
    show_preview <- function(p) {
      parts <- as.data.frame(p$parts)
      parts$.status <- c(PUBLIC = "ok", RELEASED = "info", PRIVATE = "amber")[parts$class]
      output$preview_box <- shiny::renderUI(shiny::tagList(
        if (!isTRUE(p$allowed)) .ub_notice("red", shiny::strong("Blocked: "), p$blocked_reason %||% "",
                                           " Use a local model for this request."),
        if (isTRUE(p$needs_consent)) .ub_notice("amber", "The blinded datasets are pseudonymised, not anonymised. ",
          "Is sending them to the Claude API allowed by your data-protection rules for this study?",
          shiny::div(shiny::actionButton(ns("consent"), "Yes, allowed for this study", class = "btn-sm btn-dark mt-1"))),
        shiny::p(shiny::strong("Provider: "), p$provider, shiny::strong(" Size: "), format(p$size_chars, big.mark = ","), " characters",
                 if (length(p$chunks) > 1) paste0(" in ", length(p$chunks), " parts")),
        DT::renderDT(.ub_dt(parts, page_length = 10)),
        shiny::tags$details(shiny::tags$summary("Show the full text"),
                            shiny::tags$pre(class = "ub-log", paste0("SYSTEM:\n", p$system, "\n\nUSER:\n", p$user)))))
    }
    shiny::observeEvent(input$preview, { p <- do_prepare(); if (!is.null(p)) show_preview(p) })
    shiny::observeEvent(input$consent, { ub_ai_consent_set(state$ub_dir, TRUE); p <- do_prepare(); if (!is.null(p)) show_preview(p) })

    shiny::observeEvent(input$send, {
      send_err(NULL)
      p <- do_prepare()
      if (is.null(p)) return()
      if (!isTRUE(p$allowed) || isTRUE(p$needs_consent)) return(show_preview(p))
      reply(NULL)
      r <- shiny::withProgress(message = "Waiting for the model", tryCatch(ub_ai_send(p, conn(), state$ub_dir), error = function(e) e))
      if (inherits(r, "error")) return(send_err(conditionMessage(r)))
      reply(r)
    })
    output$answer <- shiny::renderUI({
      if (!is.null(send_err())) return(.ub_notice("red", send_err()))
      r <- reply(); if (is.null(r)) return(NULL)
      bslib::card(bslib::card_header("Answer"), shiny::HTML(commonmark::markdown_html(r$text %||% "", extensions = TRUE)))
    })

    # suggestions: rows highlighted in blue are applied; click a row to (un)highlight it
    sugg <- shiny::reactive({ s <- reply()$suggestions; if (is.null(s) || !NROW(s)) NULL else as.data.frame(s) })
    sugg_task <- shiny::reactive(prep()$task %||% "")
    has_setting <- function(s) if (is.null(s$path)) rep(TRUE, nrow(s)) else nzchar(s$path)
    output$suggestions <- shiny::renderUI({
      s <- sugg(); if (is.null(s)) return(NULL)
      task <- sugg_task()
      target <- switch(task, codebook = "the codebook", review = "Retrieve", "the study settings")
      if (task %in% c("triage", "unexpected") && !any(has_setting(s)))
        return(bslib::card(bslib::card_header("Findings"), DT::DTOutput(ns("sugg_table"))))
      bslib::card(
        bslib::card_header("Suggestions (nothing changes until you apply them)"),
        shiny::tags$ol(class = "small mb-2",
          shiny::tags$li(shiny::strong("Rows highlighted in blue will be applied."),
                         " Click a row to remove its highlight if you do not agree, or to highlight it again."),
          if (task %in% c("triage", "unexpected"))
            shiny::tags$li("Rows whose 'will set' column says 'explanation only' have nothing to apply."),
          shiny::tags$li("Click the green button to apply the highlighted rows to ", target, ".",
                         if (task != "review") " A backup of the previous version is kept.")),
        shiny::div(class = "mb-2",
          shiny::actionButton(ns("apply"), "Apply", icon = shiny::icon(if (task == "review") "share" else "check"),
                              class = "btn-success btn-lg fw-bold")),
        DT::DTOutput(ns("sugg_table")))
    })
    output$sugg_table <- DT::renderDT({
      s <- sugg(); shiny::req(s)
      d <- s
      if ("suggested_setting" %in% names(s)) {
        d$will_set <- ifelse(nzchar(s$path), paste(s$path, "=", s$value), "(explanation only)")
        d <- d[, c("code", "explanation", "will_set")]
      }
      .ub_dt(d, selection = list(mode = "multiple", selected = which(has_setting(s))))
    })
    shiny::observe({
      s <- sugg(); shiny::req(s)
      sel <- intersect(input$sugg_table_rows_selected, which(has_setting(s)))
      n <- length(sel); task <- sugg_task()
      lab <- if (!n) "Apply suggestions (highlight at least one row first)"
             else if (task == "review") sprintf("Send the %d highlighted finding%s to Retrieve", n, if (n == 1) "" else "s")
             else sprintf("Apply the %d highlighted suggestion%s to %s", n, if (n == 1) "" else "s",
                          if (task == "codebook") "the codebook" else "the settings")
      shiny::updateActionButton(session, "apply", label = lab)
    })
    selected_sugg <- function() {
      s <- sugg(); if (is.null(s)) return(NULL)
      s[intersect(input$sugg_table_rows_selected, which(has_setting(s))), , drop = FALSE]
    }
    shiny::observeEvent(input$apply, {
      s <- selected_sugg()
      if (is.null(s) || !nrow(s)) return(shiny::showNotification("No row with something to apply is highlighted.", type = "warning"))
      switch(sugg_task(), codebook = apply_codebook(s), review = to_retrieve(s), apply_settings(s))
    })

    apply_codebook <- function(s) {
      t <- ub_codebook_read(state$ub_dir); n <- 0
      for (i in seq_len(nrow(s))) {
        j <- which(paste(t$TYPE, t$ACTUAL_VALUE, sep = "|") == s$row_key[i] | t$ACTUAL_VALUE == s$row_key[i])
        if (length(j) == 1 && s$column[i] %in% .UB_CB_EDITABLE) { t[j, s$column[i]] <- s$value[i]; n <- n + 1 }
      }
      ub_codebook_write(state$ub_dir, t); state$refresh()
      shiny::showNotification(paste(n, "codebook change(s) applied and saved (backup kept)."), type = "message")
    }
    apply_settings <- function(s) {
      r <- ub_ai_apply_settings(state$ub_dir, s$path, s$value)
      if (length(r$problems)) return(shiny::showNotification(paste("Not applied:", paste(r$problems, collapse = "; ")), type = "error"))
      state$settings_reload <- Sys.time(); state$refresh()
      shiny::showNotification(paste(r$applied, "setting(s) applied (backup kept). Check them in 'Study settings', then run blinding again."),
                              type = "message", duration = 10)
    }
    to_retrieve <- function(s) {
      state$ai_findings <- s
      state$goto("retrieve")
    }
  })
}
