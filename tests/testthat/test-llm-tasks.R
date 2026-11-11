llm_sourced <- TRUE
source(test_path("test-llm-fixture.R"), local = TRUE)

test_that("the task table lists the six tasks", {
  t <- ub_ai_tasks()
  expect_named(t, c("task", "label", "claude_allowed", "description"))
  expect_equal(t$task, c("workflow", "triage", "codebook", "settings", "review", "unexpected"))
  expect_equal(t$claude_allowed, c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE))
})

test_that("prepare returns the documented fields, with a preview of parts by class", {
  f <- llm_study()
  p <- ub_ai_prepare("workflow", f, claude_conn(), question = "What is the next step?")
  expect_true(all(c("task", "provider", "allowed", "blocked_reason", "parts", "system", "user", "size_chars", "chunks",
                    "needs_consent") %in% names(p)))
  expect_named(p$parts, c("class", "label", "chars"))
  expect_true(all(p$parts$class %in% c("PUBLIC", "RELEASED", "PRIVATE")))
  expect_true(any(p$parts$label == "User guide"))
  expect_true(any(p$parts$label == "Study status"))
  expect_equal(p$size_chars, nchar(p$system) + nchar(p$user))
  expect_equal(length(p$chunks), 1L)
  # local models with a small context get the short system prompt
  pl <- ub_ai_prepare("workflow", f, ollama_conn())
  expect_lt(nchar(pl$system), nchar(p$system))
  expect_lte(pl$size_chars, 24000)
})

test_that("JSON suggestions are parsed and validated for each structured task", {
  f <- llm_study()
  cb <- "Here:\n```json\n[{\"row_key\": \"ANALYTE|SZ-M1\", \"column\": \"BLINDED_VALUE\", \"value\": \"ANL02\", \"reason\": \"alias\"},
  {\"row_key\": \"ANALYTE|NOPE\", \"column\": \"BLINDED_VALUE\", \"value\": \"ANL09\", \"reason\": \"no such row\"},
  {\"row_key\": \"TREATMENT|FICTINIB\", \"column\": \"TYPE\", \"value\": \"X\", \"reason\": \"not editable\"}]\n```"
  s <- .ub_ai_parse_suggestions("codebook", cb, f)
  expect_equal(nrow(s), 1L)
  expect_equal(s$row_key, "ANALYTE|SZ-M1")
  expect_equal(s$value, "ANL02")

  st <- "```json\n{\"suggestions\": [{\"path\": \"config_overrides/datasets/ADNCA/exact_vars/PCTEST\", \"value\": \"analyte_name\", \"reason\": \"PCTEST leaks\"},
  {\"path\": \"drop_vars/ADSL\", \"value\": [\"RACE\", \"ETHNIC\"], \"reason\": \"rare\"},
  {\"path\": \"rm -rf\", \"value\": 1, \"reason\": \"bad\"}]}\n```"
  s <- .ub_ai_parse_suggestions("settings", st, f)
  expect_equal(s$path, c("config_overrides/datasets/ADNCA/exact_vars/PCTEST", "drop_vars/ADSL"))
  expect_equal(jsonlite::fromJSON(s$value[1]), "analyte_name")
  expect_equal(jsonlite::fromJSON(s$value[2]), c("RACE", "ETHNIC"))

  rv <- "Findings.\n```json\n[{\"DATASET\": \"adnca\", \"UBRECID\": \"R00003\", \"SUBJID\": \"S0001\", \"FINDING\": \"AVAL drops to 0 at 2 h\"},
  {\"DATASET\": \"\", \"FINDING\": \"no dataset\"}]\n```"
  s <- .ub_ai_parse_suggestions("review", rv, f)
  expect_equal(names(s), c("DATASET", "UBRECID", "SUBJID", "FINDING"))
  expect_equal(s$DATASET, "ADNCA")
  expect_equal(nrow(s), 1L)

  tr <- "```json\n[{\"code\": \"pctest_no_atest\", \"explanation\": \"PCTEST is not blinded\", \"suggested_setting\": \"x = y\"},
  {\"code\": \"k_anonymity\", \"explanation\": \"small cells\"}]\n```"
  s <- .ub_ai_parse_suggestions("triage", tr, f)
  expect_equal(s$code, c("pctest_no_atest", "k_anonymity"))
  expect_equal(s$suggested_setting, c("x = y", ""))
  expect_equal(s$path, c("", ""))   # 'x' is not a setting: explanation only
  expect_equal(nrow(.ub_ai_parse_suggestions("unexpected", tr, f)), 2L)

  expect_null(.ub_ai_parse_suggestions("triage", "No JSON here.", f))
  expect_null(.ub_ai_parse_suggestions("triage", "```json\n{not json\n```", f))
  expect_null(.ub_ai_parse_suggestions("workflow", tr, f))
})

test_that("triage suggestions are split into settings and applied; list settings are merged", {
  d <- tempfile("study"); dir.create(d)
  ub <- ub_setup_study(d, "S1")$path
  tr <- paste0("```json\n[",
    "{\"code\": \"adam_treatment_text ADNCA.APROFILE\", \"explanation\": \"code it\", \"suggested_setting\": \"config_overrides.datasets.ADNCA.text_vars.APROFILE = \\\"any\\\"\"},",
    "{\"code\": \"rare_demographics ADNCA.RACE\", \"explanation\": \"drop\", \"suggested_setting\": \"drop_vars.ADNCA = [\\\"RACE\\\",\\\"ETHNIC\\\"]\"},",
    "{\"code\": \"rare_demographics ADNCA.ETHNIC\", \"explanation\": \"drop\", \"suggested_setting\": \"drop_vars.ADNCA = [\\\"RACE\\\",\\\"ETHNIC\\\"]\"},",
    "{\"code\": \"x\", \"explanation\": \"unquoted value\", \"suggested_setting\": \"config_overrides/datasets/ADPP/exact_vars/PCTEST = analyte_name\"},",
    "{\"code\": \"QC\", \"explanation\": \"No QC has been run yet\", \"suggested_setting\": \"\"}]\n```")
  s <- .ub_ai_parse_suggestions("triage", tr, ub)
  expect_equal(s$path, c("config_overrides/datasets/ADNCA/text_vars/APROFILE", "drop_vars/ADNCA", "drop_vars/ADNCA",
                         "config_overrides/datasets/ADPP/exact_vars/PCTEST", ""))
  o <- ub_read_options(ub); o$drop_vars <- list(ADNCA = list("COUNTRY")); ub_write_options(ub, o, backup = FALSE)
  r <- ub_ai_apply_settings(ub, s$path, s$value)
  expect_length(r$problems, 0)
  o <- ub_read_options(ub)
  expect_setequal(unlist(o$drop_vars$ADNCA), c("COUNTRY", "RACE", "ETHNIC"))   # merged, not replaced
  expect_equal(o$config_overrides$datasets$ADNCA$text_vars$APROFILE, "any")
  expect_equal(o$config_overrides$datasets$ADPP$exact_vars$PCTEST, "analyte_name")
  expect_equal(o$config_overrides$datasets$ADNCA$exact_vars$PCTEST, "analyte_name")   # template default kept
  expect_match(ub_ai_apply_settings(ub, "", "")$problems, "nothing|none")
})

test_that("send: Claude request is masked, reply parsed, log written without the key", {
  f <- llm_study()
  cap <- NULL
  httr2::local_mocked_responses(function(req) {
    cap <<- req
    claude_reply("PCTEST must be fixed.\n```json\n[{\"code\": \"pctest_no_atest\", \"explanation\": \"PCTEST is not blinded\", \"suggested_setting\": \"config_overrides/datasets/ADNCA/exact_vars/PCTEST = analyte_name\"}]\n```")
  })
  cn <- claude_conn()
  p <- ub_ai_prepare("triage", f, cn, question = "Is FICTINIB safe here?")
  r <- ub_ai_send(p, cn, f)
  expect_match(r$text, "PCTEST must be fixed")
  expect_equal(r$suggestions$code, "pctest_no_atest")
  sent <- jsonlite::toJSON(cap$body$data, auto_unbox = TRUE)
  expect_false(grepl("FICTINIB", sent, ignore.case = TRUE))
  log <- paste(readLines(file.path(f, "private", "logs", "ai_log.jsonl")), collapse = "\n")
  expect_false(grepl(LLM_KEY, log, fixed = TRUE))
  expect_false(grepl(LLM_KEY, r$log_line, fixed = TRUE))
  rec <- jsonlite::fromJSON(r$log_line[1])
  expect_equal(rec$provider, "claude")
  expect_equal(rec$task, "triage")
  expect_true(rec$allowed)
  expect_true(all(c("time", "model", "classes", "size", "prompt", "reply") %in% names(rec)))
  expect_false("PRIVATE" %in% rec$classes)
})

test_that("send: every chunk is sent and the replies are joined", {
  f <- llm_study(release = TRUE)
  d <- haven::read_xpt(file.path(f, "output", "adnca.xpt"))
  haven::write_xpt(d[rep(seq_len(nrow(d)), 40), ], file.path(f, "output", "adnca.xpt"))
  n <- 0L
  httr2::local_mocked_responses(function(req) {
    n <<- n + 1L
    ollama_reply(sprintf("Part done.\n```json\n[{\"DATASET\": \"ADNCA\", \"UBRECID\": \"R%05d\", \"SUBJID\": \"S0001\", \"FINDING\": \"check\"}]\n```", n))
  })
  cn <- ub_llm_connect("ollama", model = "qwen3.5:4b", context_chars = 9000)
  p <- ub_ai_prepare("review", f, cn)
  r <- ub_ai_send(p, cn, f)
  expect_equal(n, length(p$chunks))
  expect_gt(n, 1L)
  expect_equal(nrow(r$suggestions), n)
  expect_match(r$text, sprintf("Part %d of %d", n, n), fixed = TRUE)
  expect_equal(length(readLines(file.path(f, "private", "logs", "ai_log.jsonl"))), n)
})

test_that("send: a provider error is logged and reported plainly", {
  f <- llm_study()
  httr2::local_mocked_responses(function(req) httr2::response_json(status_code = 401, body = list(error = list(message = "invalid x-api-key"))))
  cn <- claude_conn()
  p <- ub_ai_prepare("workflow", f, cn)
  expect_error(ub_ai_send(p, cn, f), "API key", class = "ub_llm_error")
  log <- readLines(file.path(f, "private", "logs", "ai_log.jsonl"))
  expect_match(log[length(log)], "401")
  expect_false(any(grepl(LLM_KEY, log, fixed = TRUE)))
})
