llm_sourced <- TRUE
source(test_path("test-llm-fixture.R"), local = TRUE)

test_that("Claude connection takes the key from the environment and never shows it", {
  withr::local_envvar(ANTHROPIC_API_KEY = LLM_KEY)
  cn <- ub_llm_connect("claude")
  expect_s3_class(cn, "ub_llm_conn")
  expect_equal(cn$model, "claude-opus-5-5")
  expect_equal(cn$base_url, "https://api.anthropic.com")
  expect_equal(cn$context_chars, 400000)
  shown <- c(utils::capture.output(print(cn)), utils::capture.output(str(cn)), format(cn),
             as.character(jsonlite::toJSON(unclass(cn)[setdiff(names(cn), ".secret")], auto_unbox = TRUE)))
  expect_false(any(grepl(LLM_KEY, shown, fixed = TRUE)))
  expect_true(any(grepl("hidden", shown)))
  expect_equal(ub_claude_models(), c("claude-opus-5-5", "claude-sonnet-5-5", "claude-haiku-5-5", "claude-fable-5-1"))
  expect_equal(ub_llm_list_models(cn), ub_claude_models())
})

test_that("a missing Claude key and a missing OpenAI-compatible URL give plain errors", {
  withr::local_envvar(ANTHROPIC_API_KEY = "")
  expect_error(ub_llm_connect("claude"), "ANTHROPIC_API_KEY", class = "ub_llm_error")
  expect_error(ub_llm_connect("openai"), "address", class = "ub_llm_error")
  expect_equal(ub_llm_connect("ollama")$context_chars, 24000)
  expect_equal(ub_llm_connect("ollama")$base_url, "http://localhost:11434")
})

test_that("Claude request: URL, headers and body", {
  cap <- NULL
  httr2::local_mocked_responses(function(req) { cap <<- req; claude_reply("Hello") })
  r <- ub_llm_chat(claude_conn(model = "claude-sonnet-5-5"), "SYS", "Q?", max_tokens = 123)
  expect_equal(r$text, "Hello")
  expect_equal(r$usage$input_tokens, 10L)
  expect_equal(cap$url, "https://api.anthropic.com/v1/messages")
  expect_equal(cap$headers[["anthropic-version"]], "2023-06-01")
  expect_equal(cap$headers[["content-type"]], "application/json")
  expect_true("x-api-key" %in% names(cap$headers))
  b <- cap$body$data
  expect_equal(b$model, "claude-sonnet-5-5")
  expect_equal(b$max_tokens, 123L)
  expect_equal(b$system, "SYS")
  expect_equal(b$messages[[1]], list(role = "user", content = "Q?"))
})

test_that("Ollama request: /api/chat without streaming, models from /api/tags", {
  cap <- list()
  httr2::local_mocked_responses(function(req) {
    cap[[length(cap) + 1L]] <<- req
    if (grepl("/api/tags$", req$url)) httr2::response_json(body = list(models = list(list(name = "qwen3.5:4b"), list(name = "llama3:8b"))))
    else ollama_reply("<think>hmm</think>Fine")
  })
  cn <- ub_llm_connect("ollama")                       # no model: the first one listed
  expect_equal(ub_llm_list_models(cn), c("qwen3.5:4b", "llama3:8b"))
  r <- ub_llm_chat(cn, "SYS", "Q?")
  expect_equal(r$text, "Fine")
  chat <- cap[[length(cap)]]
  expect_equal(chat$url, "http://localhost:11434/api/chat")
  b <- chat$body$data
  expect_false(b$stream)
  expect_equal(b$model, "qwen3.5:4b")
  expect_equal(b$messages[[1]], list(role = "system", content = "SYS"))
  expect_equal(b$messages[[2]], list(role = "user", content = "Q?"))
  expect_false("Authorization" %in% names(chat$headers))
})

test_that("OpenAI-compatible request: {base}/chat/completions, {base}/models, optional bearer key", {
  cap <- list()
  httr2::local_mocked_responses(function(req) {
    cap[[length(cap) + 1L]] <<- req
    if (grepl("/models$", req$url)) httr2::response_json(body = list(data = list(list(id = "local-model"))))
    else httr2::response_json(body = list(model = "local-model", choices = list(list(message = list(role = "assistant", content = "Yes"))),
                                          usage = list(prompt_tokens = 3L, completion_tokens = 1L)))
  })
  cn <- ub_llm_connect("openai", base_url = "http://localhost:1234/v1/", api_key = "lm-key")
  expect_equal(ub_llm_list_models(cn), "local-model")
  expect_equal(cap[[1]]$url, "http://localhost:1234/v1/models")
  r <- ub_llm_chat(cn, "SYS", "Q?", max_tokens = 50)
  expect_equal(r$text, "Yes")
  expect_equal(r$usage$output_tokens, 1L)
  expect_equal(cap[[length(cap)]]$url, "http://localhost:1234/v1/chat/completions")
  expect_true("Authorization" %in% names(cap[[length(cap)]]$headers))
  expect_equal(cap[[length(cap)]]$body$data$messages[[1]]$role, "system")
  expect_equal(cap[[length(cap)]]$body$data$max_tokens, 50L)
})

test_that("HTTP errors become plain ub_llm_error messages", {
  cn <- claude_conn()
  httr2::local_mocked_responses(function(req) httr2::response_json(status_code = 401,
    body = list(type = "error", error = list(type = "authentication_error", message = "invalid x-api-key"))))
  expect_error(ub_llm_chat(cn, "s", "u"), "did not accept the API key \\(401\\)", class = "ub_llm_error")

  httr2::local_mocked_responses(function(req) httr2::response_json(status_code = 404,
    body = list(type = "error", error = list(type = "not_found_error", message = "model: nope"))))
  expect_error(ub_llm_chat(claude_conn(model = "nope"), "s", "u"), "Model 'nope' was not found", class = "ub_llm_error")

  httr2::local_mocked_responses(function(req) httr2::response_json(status_code = 404, body = list(error = "model 'x' not found")))
  expect_error(ub_llm_chat(ub_llm_connect("ollama", model = "x"), "s", "u"), "ollama pull x", class = "ub_llm_error")

  httr2::local_mocked_responses(function(req) stop(errorCondition(
    "Failed to perform HTTP request. Timeout was reached: Operation timed out after 120001 milliseconds", class = "httr2_failure")))
  expect_error(ub_llm_chat(cn, "s", "u"), "timeout", class = "ub_llm_error")

  httr2::local_mocked_responses(function(req) stop(errorCondition(
    "Failed to perform HTTP request. Could not connect to server", class = "httr2_failure")))
  expect_error(ub_llm_chat(ollama_conn(), "s", "u"), "Is the server running", class = "ub_llm_error")

  httr2::local_mocked_responses(function(req) httr2::response_json(status_code = 500,
    body = list(error = "llama-server reported out-of-memory during startup")))
  expect_error(ub_llm_chat(ollama_conn(), "s", "u"), "not enough memory", class = "ub_llm_error")
})

test_that("ub_llm_test reports success and failure without stopping", {
  httr2::local_mocked_responses(function(req) claude_reply("OK"))
  r <- ub_llm_test(claude_conn())
  expect_true(r$ok)
  expect_equal(r$model, "claude-opus-5-5")
  expect_true(is.numeric(r$latency_s))
  httr2::local_mocked_responses(function(req) httr2::response_json(status_code = 401, body = list(error = list(message = "bad"))))
  r <- ub_llm_test(claude_conn())
  expect_false(r$ok)
  expect_match(r$message, "401")
  expect_false(grepl(LLM_KEY, r$message, fixed = TRUE))
})
