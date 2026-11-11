llm_sourced <- TRUE
source(test_path("test-llm-fixture.R"), local = TRUE)

CLAUDE_TASKS <- c("workflow", "triage", "settings", "review", "unexpected")

planted_question <- paste("Why is FICTINIB (FCT-INJ) of subject SYN-PK-001-955-501 at SITE-777 coded? Also SZ-M1,",
                          "Crossover Group A, Dr Quillfeather, Moonbase Clinic, Zorblax Study Centre.")
planted_finding <- data.frame(severity = "amber", code = "pctest_no_atest", dataset = "ADNCA", variable = "PCTEST",
                              message = "PCTEST holds FICTINIB for 955-501", suggestion = "exact_vars",
                              sample_values = "Fictinib | SZ-M1", stringsAsFactors = FALSE)

test_that("no Claude prompt contains a planted private value, and nothing PRIVATE is included", {
  f <- llm_study(release = TRUE)
  ub_ai_consent_set(f, TRUE)
  httr2::local_mocked_responses(no_http)
  for (t in CLAUDE_TASKS) {
    p <- ub_ai_prepare(t, f, claude_conn(), question = planted_question, context = list(finding = planted_finding))
    expect_true(p$allowed, info = paste(t, p$blocked_reason))
    expect_false(any(p$parts$class == "PRIVATE"), info = t)
    full <- paste(p$system, p$user, paste(unlist(p$chunks), collapse = "\n"))
    for (x in LLM_PLANTED) expect_false(grepl(x, full, ignore.case = TRUE), info = paste(t, x))
    expect_match(p$user, "<TREATMENT_", fixed = TRUE, info = t)          # the question was masked, not dropped
  }
})

test_that("the final leak check blocks every Claude task when masking fails (no override)", {
  f <- llm_study(release = TRUE)
  ub_ai_consent_set(f, TRUE)
  httr2::local_mocked_responses(no_http)
  local_mocked_bindings(ub_ai_mask = function(text, values, dates = TRUE) text)    # simulate a masking bug
  for (t in CLAUDE_TASKS) {
    p <- ub_ai_prepare(t, f, claude_conn(), question = "Is Moonbase Clinic in the data?")
    expect_false(p$allowed, info = t)
    expect_match(p$blocked_reason, "Blocked: the prompt still contains", info = t)
    expect_match(p$blocked_reason, "term", info = t)
    expect_false(grepl("Moonbase", p$blocked_reason), info = t)               # positions, never the value
    expect_error(ub_ai_send(p, claude_conn(), f), "Not sent", class = "ub_llm_error")
  }
})

test_that("a private value in the released data blocks the review for Claude", {
  f <- llm_study(release = TRUE)
  ub_ai_consent_set(f, TRUE)
  d <- haven::read_xpt(file.path(f, "output", "adnca.xpt"))
  d$PARAMCD[3] <- "SZ-M1"                                                    # a leak in output/
  haven::write_xpt(d, file.path(f, "output", "adnca.xpt"))
  p <- ub_ai_prepare("review", f, claude_conn())
  expect_false(p$allowed)
  expect_match(p$blocked_reason, "analyte")
  expect_true(ub_ai_prepare("review", f, ollama_conn())$allowed)             # the local model may see it
})

test_that("codebook decisions never go to Claude; the local model gets the real CHECK rows", {
  f <- llm_study()
  p <- ub_ai_prepare("codebook", f, claude_conn())
  expect_false(p$allowed)
  expect_match(p$blocked_reason, "local model")
  expect_equal(p$user, "")
  expect_false(ub_ai_tasks()$claude_allowed[ub_ai_tasks()$task == "codebook"])
  pl <- ub_ai_prepare("codebook", f, ollama_conn())
  expect_true(pl$allowed)
  expect_true("PRIVATE" %in% pl$parts$class)
  expect_match(pl$user, "SZ-M1", fixed = TRUE)
  expect_match(pl$user, "Rows needing a decision (1)", fixed = TRUE)
})

test_that("the local model gets PRIVATE parts unmasked", {
  f <- llm_study()
  p <- ub_ai_prepare("triage", f, ollama_conn(), question = "What about FICTINIB?", context = list(finding = planted_finding))
  expect_true(p$allowed)
  expect_true("PRIVATE" %in% p$parts$class)
  expect_match(p$user, "FICTINIB", fixed = TRUE)
  expect_match(p$user, "Fictinib | SZ-M1", fixed = TRUE)
})

test_that("review needs QC PASS/WARN, and consent for Claude", {
  f <- llm_study(release = TRUE, qc = "WARN")
  p <- ub_ai_prepare("review", f, claude_conn())
  expect_false(p$allowed)
  expect_true(p$needs_consent)
  expect_false(ub_ai_consent_get(f))
  expect_true(ub_ai_prepare("review", f, ollama_conn())$allowed)             # local: no consent needed
  ub_ai_consent_set(f, TRUE)
  expect_true(ub_ai_consent_get(f))
  expect_true(file.exists(file.path(f, "private", "logs", "ai_consent.json")))
  p <- ub_ai_prepare("review", f, claude_conn())
  expect_true(p$allowed)
  expect_false(p$needs_consent)
  expect_true(all(p$parts$class == "RELEASED"))
  expect_match(p$user, "STUDY-XXX-S0001", fixed = TRUE)
  ub_ai_consent_set(f, FALSE)
  expect_true(ub_ai_prepare("review", f, claude_conn())$needs_consent)

  g <- llm_study(release = TRUE, qc = "FAIL")
  ub_ai_consent_set(g, TRUE)
  expect_match(ub_ai_prepare("review", g, ollama_conn())$blocked_reason, "QC PASS or WARN")
  h <- llm_study()                                                            # nothing released
  expect_match(ub_ai_prepare("review", h, ollama_conn())$blocked_reason, "nothing released")
})

test_that("large released data are split into several requests", {
  f <- llm_study(release = TRUE)
  d <- haven::read_xpt(file.path(f, "output", "adnca.xpt"))
  big <- d[rep(seq_len(nrow(d)), 40), ]
  big$UBRECID <- sprintf("R%05d", seq_len(nrow(big)))
  haven::write_xpt(big, file.path(f, "output", "adnca.xpt"))
  p <- ub_ai_prepare("review", f, ub_llm_connect("ollama", model = "m", context_chars = 9000))
  expect_true(p$allowed)
  expect_gt(length(p$chunks), 1L)
  expect_true(all(vapply(p$chunks, nchar, 1L) <= 9000))
  expect_match(p$chunk_summary, "requests")
  expect_equal(sum(vapply(p$chunks, function(x) lengths(regmatches(x, gregexpr("\n\"STUDY-XXX\",", x, fixed = TRUE))), 1L)), nrow(big))
})

test_that("a prompt altered after the gate is refused at send time", {
  f <- llm_study()
  httr2::local_mocked_responses(no_http)
  p <- ub_ai_prepare("workflow", f, claude_conn(), question = "next step?")
  expect_true(p$allowed)
  p$chunks[[1]] <- paste(p$chunks[[1]], "Moonbase Clinic")
  expect_error(ub_ai_send(p, claude_conn(), f), "Blocked", class = "ub_llm_error")
  pl <- ub_ai_prepare("workflow", f, ollama_conn())
  expect_error(ub_ai_send(pl, claude_conn(), f), "prepared for 'ollama'", class = "ub_llm_error")
  log <- readLines(file.path(f, "private", "logs", "ai_log.jsonl"))
  expect_true(all(vapply(log, function(l) isFALSE(jsonlite::fromJSON(l)$allowed), logical(1))))
})
