# AI layer, part 1: provider connections (Claude API, Ollama, OpenAI-compatible servers).
# No UI code and no data classification here: the gate (llm_gate.R) decides what may be sent.
# The API key lives only in the connection object, in memory (an environment, so that str() and print() do
# not show it). It is never written to disk or to a log.

.UB_CLAUDE_BASE <- "https://api.anthropic.com"
.UB_OLLAMA_BASE <- "http://localhost:11434"
.UB_ANTHROPIC_VERSION <- "2023-06-01"

#' Claude models offered in the AI tab
#' @return character vector of model IDs (the first is the default)
#' @export
ub_claude_models <- function() c("claude-opus-5-5", "claude-sonnet-5-5", "claude-haiku-5-5", "claude-fable-5-1")

.ub_llm_error <- function(...) {
  stop(structure(class = c("ub_llm_error", "error", "condition"),
                 list(message = paste0(...), call = NULL)))
}

#' Connect to an AI model
#'
#' Nothing is sent when connecting. The key is kept in memory only, in the returned object.
#' @param provider `"claude"` (Claude API), `"ollama"` (local Ollama) or `"openai"` (any OpenAI-compatible server,
#'   e.g. LM Studio, vLLM, llama.cpp)
#' @param model model ID; Claude default `claude-opus-5-5`. For local servers `NULL` means the first model the
#'   server lists
#' @param api_key Claude: key, default `Sys.getenv("ANTHROPIC_API_KEY")`. OpenAI-compatible: optional bearer key
#' @param base_url server address; Claude `https://api.anthropic.com`, Ollama `http://localhost:11434`;
#'   OpenAI-compatible: required, e.g. `http://localhost:1234/v1`
#' @param timeout seconds to wait for a reply
#' @param context_chars how many characters one request may hold (default 400000 for Claude, 24000 locally)
#' @return an object of class `ub_llm_conn`
#' @export
ub_llm_connect <- function(provider = c("claude", "ollama", "openai"), model = NULL, api_key = NULL, base_url = NULL,
                           timeout = 120, context_chars = NULL) {
  provider <- match.arg(provider)
  if (provider == "claude") {
    if (is.null(api_key) || !nzchar(api_key)) api_key <- Sys.getenv("ANTHROPIC_API_KEY", "")
    if (!nzchar(api_key)) .ub_llm_error("No Claude API key. Put ANTHROPIC_API_KEY=<your key> in your .Renviron file ",
                                        "(then restart R), or paste the key in the AI tab (kept for this session only).")
    if (is.null(base_url) || !nzchar(base_url)) base_url <- .UB_CLAUDE_BASE
    if (is.null(model) || !nzchar(model)) model <- ub_claude_models()[1]
  } else if (provider == "ollama") {
    if (is.null(base_url) || !nzchar(base_url)) base_url <- .UB_OLLAMA_BASE
  } else {
    if (is.null(base_url) || !nzchar(base_url))
      .ub_llm_error("Give the address of the OpenAI-compatible server, for example http://localhost:1234/v1")
  }
  base_url <- sub("/+$", "", trimws(base_url))
  if (provider == "ollama") base_url <- sub("/api$", "", base_url)
  if (is.null(context_chars)) context_chars <- if (provider == "claude") 400000 else 24000
  secret <- new.env(parent = emptyenv())
  secret$key <- if (is.null(api_key) || !nzchar(api_key)) NULL else api_key
  structure(list(provider = provider, model = if (is.null(model) || !nzchar(model)) NULL else model,
                 base_url = base_url, timeout = as.numeric(timeout), context_chars = as.numeric(context_chars),
                 .secret = secret),
            class = "ub_llm_conn")
}

#' @export
print.ub_llm_conn <- function(x, ...) {
  label <- c(claude = "Claude API", ollama = "Ollama (local)", openai = "OpenAI-compatible (local)")[[x$provider]]
  cat("<AI connection> ", label, "\n", sep = "")
  cat("  model:   ", if (is.null(x$model)) "(first model on the server)" else x$model, "\n", sep = "")
  cat("  address: ", x$base_url, "\n", sep = "")
  cat("  key:     ", if (is.null(x$.secret$key)) "none" else "set (hidden)", "\n", sep = "")
  cat("  context: ", format(x$context_chars, big.mark = ","), " characters per request\n", sep = "")
  invisible(x)
}

#' @export
format.ub_llm_conn <- function(x, ...) {
  paste0("<AI connection: ", x$provider, " ", if (is.null(x$model)) "" else x$model, " at ", x$base_url, ">")
}

.ub_is_local <- function(conn) conn$provider != "claude"

# ---------------------------------------------------------------------------------------------------------------
# HTTP
# ---------------------------------------------------------------------------------------------------------------
.ub_llm_request <- function(conn, path, body = NULL) {
  req <- httr2::request(paste0(conn$base_url, path))
  key <- conn$.secret$key
  if (conn$provider == "claude") {
    req <- httr2::req_headers(req, `x-api-key` = key, `anthropic-version` = .UB_ANTHROPIC_VERSION,
                              `content-type` = "application/json", .redact = "x-api-key")
  } else if (!is.null(key)) {
    req <- httr2::req_headers(req, Authorization = paste("Bearer", key), .redact = "Authorization")
  }
  if (!is.null(body)) req <- httr2::req_body_json(req, body, auto_unbox = TRUE, null = "null")
  req <- httr2::req_timeout(req, conn$timeout)
  httr2::req_error(req, is_error = function(resp) FALSE)
}

.ub_llm_perform <- function(conn, req, what = "request") {
  where <- if (conn$provider == "claude") "the Claude API" else paste0("the local model server at ", conn$base_url)
  resp <- tryCatch(httr2::req_perform(req), error = function(e) {
    msg <- conditionMessage(e)
    if (!is.null(e$parent)) msg <- paste(msg, conditionMessage(e$parent))
    if (grepl("timeout|timed out", msg, ignore.case = TRUE))
      .ub_llm_error("No reply from ", where, " within ", conn$timeout, " seconds (timeout). ",
                    "Try again, use a smaller model, or increase the timeout.")
    .ub_llm_error("Cannot connect to ", where, ". ",
                  if (conn$provider == "claude") "Check the internet connection (and any proxy)."
                  else "Is the server running? For Ollama: start the Ollama app, or run 'ollama serve'.",
                  " (", trimws(gsub("\\s+", " ", msg)), ")")
  })
  st <- httr2::resp_status(resp)
  if (st >= 400) .ub_llm_http_error(conn, resp, st, where)
  resp
}

.ub_llm_body_message <- function(resp) {
  b <- tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(b)) {
    txt <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
    return(substr(trimws(txt), 1, 300))
  }
  m <- if (is.list(b$error)) b$error$message else b$error
  if (is.null(m)) m <- b$message
  if (is.null(m)) "" else substr(as.character(m), 1, 300)
}

.ub_llm_http_error <- function(conn, resp, st, where) {
  detail <- .ub_llm_body_message(resp)
  detail <- if (nzchar(detail)) paste0(" Server message: ", detail) else ""
  model <- if (is.null(conn$model)) "(none chosen)" else conn$model
  if (st %in% c(401, 403))
    .ub_llm_error(if (conn$provider == "claude") "The Claude API did not accept the API key (" else
                    "The server did not accept the key (", st, "). Check the key.", detail)
  if (st == 404)
    .ub_llm_error("Model '", model, "' was not found on ", where, " (404). Choose a model from the list",
                  if (conn$provider == "ollama") paste0(", or download it with 'ollama pull ", model, "'") else "",
                  ", and check the address.", detail)
  if (st == 429) .ub_llm_error("Too many requests to ", where, " (429): wait a minute and try again.", detail)
  if (st == 413) .ub_llm_error("The request is too large for ", where, " (413). Lower the context size.", detail)
  if (st == 400) .ub_llm_error("The request was refused by ", where, " (400).", detail)
  if (grepl("out.of.memory|unable to allocate|failed to allocate", detail, ignore.case = TRUE))
    .ub_llm_error("The local model could not be loaded: not enough memory (", st, "). Close other programs, choose a ",
                  "smaller model, or lower the context size.", detail)
  if (st >= 500) .ub_llm_error(where, " had an internal error (", st, "). Try again later.", detail)
  .ub_llm_error(where, " answered with HTTP status ", st, ".", detail)
}

.ub_json <- function(resp) {
  force(resp)       # evaluate the request outside the tryCatch below, so its errors reach the caller
  txt <-tryCatch(httr2::resp_body_string(resp), error = function(e) "")
  b <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(b)) {
    # streamed reply (one JSON object per line, as Ollama sends when streaming): join the message pieces
    ln <- strsplit(txt, "\r?\n")[[1]]; ln <- ln[nzchar(trimws(ln))]
    objs <- lapply(ln, function(l) tryCatch(jsonlite::fromJSON(l, simplifyVector = FALSE), error = function(e) NULL))
    objs <- Filter(Negate(is.null), objs)
    if (!length(objs)) .ub_llm_error("The reply of the model server could not be read (not JSON): ",
                                     substr(gsub("\\s+", " ", txt), 1, 200))
    b <- objs[[length(objs)]]
    if (!is.null(b$message) || !is.null(objs[[1]]$message))
      b$message$content <- paste(vapply(objs, function(o) .ub_or(as.character(o$message$content), ""), ""), collapse = "")
  }
  if (!is.null(b$error) && is.null(b$message) && is.null(b$content) && is.null(b$choices))
    .ub_llm_error("The model server reported an error: ",
                  substr(as.character(if (is.list(b$error)) .ub_or(b$error$message, "") else b$error), 1, 300))
  b
}

#' Models available on the connection's server
#' @param conn a [ub_llm_connect()] connection
#' @return character vector of model IDs
#' @export
ub_llm_list_models <- function(conn) {
  stopifnot(inherits(conn, "ub_llm_conn"))
  if (conn$provider == "claude") return(ub_claude_models())
  if (conn$provider == "ollama") {
    b <- .ub_json(.ub_llm_perform(conn, httr2::req_method(.ub_llm_request(conn, "/api/tags"), "GET")))
    return(vapply(b$models, function(m) as.character(if (is.null(m$name)) m$model else m$name), ""))
  }
  b <- .ub_json(.ub_llm_perform(conn, httr2::req_method(.ub_llm_request(conn, "/models"), "GET")))
  vapply(b$data, function(m) as.character(m$id), "")
}

.ub_llm_model <- function(conn) {
  if (!is.null(conn$model)) return(conn$model)
  ms <- ub_llm_list_models(conn)
  if (!length(ms)) .ub_llm_error("The local server lists no models. For Ollama, download one first, ",
                                 "for example 'ollama pull qwen3.5:4b'.")
  ms[1]
}

.ub_strip_thinking <- function(x) trimws(gsub("(?s)<think>.*?</think>", "", x, perl = TRUE))

#' Send a chat request
#'
#' Low level: no data gate. Use [ub_ai_send()] for study content.
#' @param conn a [ub_llm_connect()] connection
#' @param system system prompt (one string)
#' @param messages list of `list(role = "user"/"assistant", content = "...")`, or one string (a user message)
#' @param max_tokens maximum length of the reply
#' @return list(text, usage = list(input_tokens, output_tokens), raw = parsed reply)
#' @export
ub_llm_chat <- function(conn, system, messages, max_tokens = 4000) {
  stopifnot(inherits(conn, "ub_llm_conn"))
  if (is.character(messages)) messages <- list(list(role = "user", content = paste(messages, collapse = "\n")))
  messages <- lapply(messages, function(m) list(role = as.character(m$role), content = as.character(m$content)))
  system <- paste(system, collapse = "\n")
  model <- .ub_llm_model(conn)
  if (conn$provider == "claude") {
    body <- list(model = model, max_tokens = as.integer(max_tokens), system = system, messages = messages)
    b <- .ub_json(.ub_llm_perform(conn, .ub_llm_request(conn, "/v1/messages", body)))
    parts <- Filter(function(p) identical(p$type, "text"), b$content)
    text <- paste(vapply(parts, function(p) as.character(p$text), ""), collapse = "")
    usage <- list(input_tokens = b$usage$input_tokens, output_tokens = b$usage$output_tokens)
  } else if (conn$provider == "ollama") {
    num_ctx <- as.integer(min(131072, max(4096, ceiling(conn$context_chars / 3) + max_tokens)))
    body <- list(model = model, messages = c(list(list(role = "system", content = system)), messages), stream = FALSE,
                 options = list(num_ctx = num_ctx, num_predict = as.integer(max_tokens)))
    b <- .ub_json(.ub_llm_perform(conn, .ub_llm_request(conn, "/api/chat", body)))
    text <- as.character(if (is.null(b$message$content)) "" else b$message$content)
    usage <- list(input_tokens = b$prompt_eval_count, output_tokens = b$eval_count)
  } else {
    body <- list(model = model, messages = c(list(list(role = "system", content = system)), messages),
                 max_tokens = as.integer(max_tokens), stream = FALSE)
    b <- .ub_json(.ub_llm_perform(conn, .ub_llm_request(conn, "/chat/completions", body)))
    text <- if (length(b$choices)) as.character(b$choices[[1]]$message$content) else ""
    if (!length(text)) text <- ""
    usage <- list(input_tokens = b$usage$prompt_tokens, output_tokens = b$usage$completion_tokens)
  }
  list(text = .ub_strip_thinking(text), usage = usage, raw = b)
}

#' Test a connection with a one-line request
#' @param conn a [ub_llm_connect()] connection
#' @return list(ok, model, latency_s, message); never stops
#' @export
ub_llm_test <- function(conn) {
  t0 <- Sys.time()
  model <- if (is.null(conn$model)) NA_character_ else conn$model
  r <- tryCatch({
    model <- .ub_llm_model(conn)
    conn$model <- model
    ub_llm_chat(conn, "You are a connection test. Answer with one word.", "Reply with the word OK.", max_tokens = 200)
  }, ub_llm_error = function(e) e)
  lat <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
  if (inherits(r, "ub_llm_error")) return(list(ok = FALSE, model = model, latency_s = lat, message = conditionMessage(r)))
  list(ok = TRUE, model = model, latency_s = lat,
       message = paste0("Connected to ", model, " in ", lat, " s. Reply: ", substr(gsub("\\s+", " ", r$text), 1, 80)))
}
