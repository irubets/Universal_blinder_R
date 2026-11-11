# Server logic of each tab, driven on a real synthetic study folder.

app_state <- function(ub = NULL, study = NULL) {
  s <- shiny::reactiveValues(ub_dir = ub, study_dir = study, tick = 0, ai_context = NULL, ai_findings = NULL,
                             settings_reload = NULL, went = NULL)
  s$refresh <- function() s$tick <- shiny::isolate(s$tick) + 1
  s$goto <- function(tab, step = NULL) { s$went <- tab; s$went_step <- step }
  s
}

synthetic_ub <- function(lab = FALSE) {
  d <- tempfile("study"); dir.create(d)
  src <- system.file("examples", "synthetic", "source", package = "universalblinder")
  ub <- ub_setup_study(d, "SYN")$path
  file.copy(list.files(src, full.names = TRUE), file.path(ub, "input"))
  o <- ub_read_options(ub)
  o$rename_vars <- list(ADSL = list(FCTFL = "TRTFL01", SYZFL = "TRTFL02"), ADEX = list(DOSEFCT = "DOSET01", DOSESYZ = "DOSET02"))
  ub_write_options(ub, o, backup = FALSE)
  ub
}

test_that("Blind & QC tab: pre-flight, first run stops for the codebook, second run releases", {
  ub <- synthetic_ub()
  st <- app_state(ub)
  shiny::testServer(mod_blind_server, args = list(state = st), {
    session$setInputs(preflight = 1)
    expect_true(nrow(pf()) > 0)
    expect_false(any(pf()$.status == "red"))
    session$setInputs(run = 1)
    expect_false(result()$released)
    cb <- ub_codebook_read(ub); cb$CHECK <- ""; ub_codebook_write(ub, cb)
    session$setInputs(run = 2)                                     # answered but not confirmed: the run does not start
    expect_false(result()$released)
    expect_equal(ub_codebook_review_status(ub)$status, "unconfirmed")
    ub_codebook_confirm_review(ub, "Test Reviewer")
    session$setInputs(run = 3)
    expect_true(result()$released)
    expect_true(result()$qc %in% c("PASS", "WARN"))
    expect_true(nrow(qc()) > 0)
    session$setInputs(qc_table_rows_selected = 1, qc_ai = 1)
    expect_equal(st$went, "ai")
    expect_equal(st$ai_context$task, "triage")
  })
})

test_that("Codebook tab: edit a cell, save with backup", {
  ub <- synthetic_ub()
  ub_blind_folder(ub, ub_options_for_engine(ub_read_options(ub)), quiet = TRUE)
  st <- app_state(ub)
  shiny::testServer(mod_codebook_server, args = list(state = st), {
    session$flushReact()
    t <- cb()
    expect_true(nrow(t) > 0)
    session$setInputs(only_attention = FALSE)
    col <- which(names(shown()) == "NOTE") - 1
    session$setInputs(table_cell_edit = data.frame(row = 1, col = col, value = "checked by me"))
    expect_true(dirty())
    session$setInputs(save = 1)
    expect_equal(ub_codebook_read(ub)$NOTE[shown()$.row[1]], "checked by me")
    expect_length(list.files(file.path(ub, "private", "codebook_backup")), 1)
  })
})

test_that("Codebook tab: highlighted CHECK rows are marked as decided in the app and saved", {
  ub <- synthetic_ub()
  ub_blind_folder(ub, ub_options_for_engine(ub_read_options(ub)), quiet = TRUE)
  cb0 <- ub_codebook_read(ub)
  cb0$CHECK[1:2] <- c("Is this the right drug?", "Same code as row 1?"); cb0$CHECK[-(1:2)] <- ""
  cb0$BLINDED_VALUE[cb0$BLINDED_VALUE == "?"] <- "TRT99"
  ub_codebook_write(ub, cb0)
  st <- app_state(ub)
  shiny::testServer(mod_codebook_server, args = list(state = st), {
    session$setInputs(only_attention = TRUE)                     # as the app starts: open rows only
    expect_equal(sum(cb()$.status == "amber"), 2)
    expect_match(output$decide_ui$html, "Mark the highlighted rows as decided")
    expect_null(output$done_ui)                                  # rows still open
    session$setInputs(decide = 1)                                # nothing highlighted: nothing changes
    expect_equal(sum(nzchar(ub_codebook_read(ub)$CHECK)), 2)
    session$setInputs(table_rows_selected = 1L, decide = 2)      # one row decided, saved
    expect_equal(sum(nzchar(ub_codebook_read(ub)$CHECK)), 1)
    expect_true(length(list.files(file.path(ub, "private", "codebook_backup"))) >= 1)
    session$setInputs(table_rows_selected = 1L, decide = 3)      # the list now shows only the other row
    expect_equal(sum(nzchar(ub_codebook_read(ub)$CHECK)), 0)
    expect_match(output$done_ui$html, "confirm the review")
    session$setInputs(to_run = 1)
    expect_equal(st$went, "blind"); expect_equal(st$went_step, "2. Run blinding")
  })
})

test_that("Study settings tab: switches and save", {
  ub <- synthetic_ub()
  st <- app_state(ub)
  shiny::testServer(mod_settings_server, args = list(state = st), {
    session$flushReact()
    session$setInputs(overrides_json = jsonlite::toJSON(ub_read_options(ub)$config_overrides, auto_unbox = TRUE))
    o <- .ub_switch_apply(ov(), .UB_OVERRIDE_SWITCHES$precision, TRUE)
    session$setInputs(overrides_json = jsonlite::toJSON(o, auto_unbox = TRUE))
    session$setInputs(add_drop_vars = 1)
    session$setInputs(tb_drop_vars_cell_edit = data.frame(row = c(1, 1), col = c(0, 1), value = c("*", "RACE, ETHNIC")))
    session$setInputs(save = 1)
    o2 <- ub_read_options(ub)
    expect_equal(o2$config_overrides$datasets$ADNCA$analyte_numeric$precision_from, "PCORRES")
    expect_equal(unlist(o2$drop_vars[["*"]]), c("RACE", "ETHNIC"))
    expect_equal(o2$rename_vars$ADSL$FCTFL, "TRTFL01")
  })
})

test_that("Prepare PC tab: reads columns of a lab file and reports unmatched rows", {
  ub <- synthetic_ub()
  pc <- haven::read_xpt(file.path(ub, "input", "pc.xpt"))
  pc$PCTPTNUM <- ifelse(pc$PCTPT == "PREDOSE", 0, suppressWarnings(as.numeric(sub("H.*", "", pc$PCTPT))))
  lab <- data.frame(SUBJECT_ID = pc$USUBJID, VISIT_NAME = ifelse(pc$VISIT == pc$VISIT[1], "LABVISIT", pc$VISIT), NOMINAL = pc$PCTPTNUM, CONC = pc$PCORRES, ANALYTE = pc$PCTESTCD)
  for (c in c("PCORRES", "PCSTRESC")) pc[[c]] <- ""
  pc$PCSTRESN <- NA_real_
  haven::write_xpt(pc, file.path(ub, "input", "pc.xpt"), name = "PC")
  utils::write.csv(lab, file.path(ub, "input", "lab.csv"), row.names = FALSE)
  st <- app_state(ub)
  shiny::testServer(mod_prepare_pc_server, args = list(state = st), {
    session$flushReact()
    expect_equal(pc_status()$status, "lab_file_not_merged")
    session$setInputs(results_file = "lab.csv", sheet = "", read_cols = 1)
    expect_setequal(cols(), c("SUBJECT_ID", "VISIT_NAME", "NOMINAL", "CONC", "ANALYTE"))
    an <- unique(pc$PCTESTCD)
    maps$analytes <- data.frame(LAB_NAME = an, PCTESTCD = an)
    session$setInputs(col_subject = "SUBJECT_ID", col_visit = "VISIT_NAME", col_nominal_time = "NOMINAL", col_result = "CONC",
                      col_analyte = "ANALYTE", col_unit = "", col_lloq = "", col_timepoint_name = "", col_specimen = "",
                      analyte_mode = "several", check_dataset = "", pctestcd = "", pctest = "")
    session$setInputs(run = 1)
    expect_false(isTRUE(result()$written))
    expect_true("LABVISIT" %in% maps$visit$LAB_VISIT)
    expect_true(file.exists(file.path(ub, "private", "reports", "prepare_pc_report.html")))
    expect_equal(maps$visit$.status[maps$visit$LAB_VISIT == "LABVISIT"], "red")
    session$setInputs(visit_map_cell_edit = data.frame(row = which(maps$visit$LAB_VISIT == "LABVISIT"), col = 1, value = pc$VISIT[1]))
    session$setInputs(run = 2)
    expect_true(isTRUE(result()$written))
    expect_equal(ub_check_pc_results(ub)$status, "ok")
    expect_equal(ub_read_options(ub)$prepare_pc$visit_map$LABVISIT, pc$VISIT[1])
  })
})

test_that("Retrieve tab: pasted findings return the original records", {
  ub <- synthetic_ub()
  opts <- ub_options_for_engine(ub_read_options(ub))
  ub_blind_folder(ub, opts, quiet = TRUE)
  cb <- ub_codebook_read(ub); cb$CHECK <- ""; ub_codebook_write(ub, cb)
  expect_true(ub_blind_folder(ub, opts, quiet = TRUE)$released)
  a <- haven::read_xpt(file.path(ub, "output", "adnca.xpt"))
  st <- app_state(ub)
  shiny::testServer(mod_retrieve_server, args = list(state = st), {
    session$setInputs(pasted = paste0("DATASET\tUBRECID\tFINDING\nADNCA\t", a$UBRECID[1], " ", a$UBRECID[2], "\todd value"))
    session$setInputs(use_pasted = 1)
    expect_equal(nrow(fd()), 1)
    session$setInputs(retrieve = 1)
    expect_equal(nrow(res()$ADNCA), 2)
  })
})

test_that("AI tab: tasks listed, codebook switches to a local model, Claude preview is masked", {
  ub <- synthetic_ub()
  st <- app_state(ub)
  shiny::testServer(mod_ai_server, args = list(state = st), {
    session$setInputs(provider = "claude", api_key = "sk-test-not-real", claude_model = "claude-opus-5-5",
                      base_url = "http://localhost:11434", local_key = "", local_model = "", task = "workflow",
                      question = "What do I do first?")
    session$setInputs(preview = 1)
    expect_true(prep()$allowed)
    expect_false(grepl("sk-test-not-real", prep()$user, fixed = TRUE))
    session$setInputs(task = "codebook", preview = 2)
    expect_false(prep()$allowed)
  })
})

test_that("AI tab: a send error disappears when the model changes; highlighted triage rows are applied", {
  ub <- synthetic_ub()
  st <- app_state(ub)
  fail <- TRUE
  testthat::local_mocked_bindings(ub_ai_send = function(p, conn, folder) {
    if (fail) stop("No reply from the local model server at http://localhost:11434 within 120 seconds (timeout).")
    s <- .ub_ai_parse_suggestions("triage", paste0("```json\n[",
      "{\"code\": \"rare_demographics ADNCA.RACE\", \"explanation\": \"drop\", \"suggested_setting\": \"drop_vars.ADNCA = [\\\"RACE\\\"]\"},",
      "{\"code\": \"rare_demographics ADNCA.ETHNIC\", \"explanation\": \"drop\", \"suggested_setting\": \"drop_vars.ADNCA = [\\\"ETHNIC\\\"]\"},",
      "{\"code\": \"QC\", \"explanation\": \"not run\", \"suggested_setting\": \"\"}]\n```"), folder)
    list(text = "ok", suggestions = s)
  })
  shiny::testServer(mod_ai_server, args = list(state = st), {
    session$setInputs(provider = "ollama", api_key = "", claude_model = "claude-opus-5-5", base_url = "http://localhost:11434",
                      local_key = "", local_model = "qwen", task = "triage", question = "")
    session$setInputs(send = 1)
    expect_match(send_err(), "timeout")
    session$setInputs(provider = "claude")
    expect_null(send_err())                                # stale error gone after switching
    fail <<- FALSE
    session$setInputs(send = 2)
    expect_null(send_err())
    expect_equal(nrow(sugg()), 3L)
    session$setInputs(sugg_table_rows_selected = c(1L, 3L))   # user un-highlighted ETHNIC; QC row has nothing to apply
    session$setInputs(apply = 1)
    expect_equal(unlist(ub_read_options(ub)$drop_vars$ADNCA), "RACE")
  })
})

test_that("Blind & QC tab: highlighted pre-flight suggestions are applied to the settings and the pre-flight re-runs", {
  ub <- synthetic_ub()
  st <- app_state(ub)
  shiny::testServer(mod_blind_server, args = list(state = st), {
    session$setInputs(preflight = 1)
    i <- which(pf()$code == "rare_demographics" & pf()$dataset == "ADNCA")
    expect_true(length(i) >= 1)
    v <- pf()$variable[i]
    session$setInputs(pf_apply = 1)                       # nothing highlighted: nothing changes
    expect_null(ub_read_options(ub)$drop_vars$ADNCA)
    session$setInputs(pf_rows_selected = i, pf_apply = 2)
    expect_setequal(unlist(ub_read_options(ub)$drop_vars$ADNCA), v)
    expect_false(any(pf()$code == "rare_demographics" & pf()$dataset == "ADNCA"))
    expect_false(is.null(st$settings_reload))
  })
})

test_that("Pre-flight tab: shows the saved choices, and a reset there brings the suggestions back", {
  ub <- synthetic_ub()
  st <- app_state(ub)
  shiny::testServer(mod_blind_server, args = list(state = st), {
    session$setInputs(preflight = 1)
    expect_false(grepl("drop_vars/ADNCA", output$pf_saved$html))   # the example study saves other settings
    i <- which(pf()$code == "rare_demographics" & pf()$dataset == "ADNCA")
    session$setInputs(pf_rows_selected = i, pf_apply = 1)
    expect_match(output$pf_saved$html, "saved choice")
    expect_match(output$pf_saved$html, "drop_vars/ADNCA")
    expect_match(output$pf_saved$html, "Reset settings to defaults")
    session$setInputs(reset_settings = 1, confirm_reset = 1)
    expect_null(ub_read_options(ub)$drop_vars$ADNCA)
    expect_true(any(pf()$code == "rare_demographics" & pf()$dataset == "ADNCA"))   # pre-flight re-ran
    expect_match(output$pf_saved$html, "the defaults")
  })
})

test_that("Run blinding tab: NOTHING WAS RELEASED lists the failed QC checks and hands them to the AI", {
  ub <- synthetic_ub()
  st <- app_state(ub)
  dir.create(file.path(ub, "private", "qc"), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(list(findings = data.frame(
    status = c("FAIL", "PASS"), check = c("dose_ratio_reveals_bsa", "ids_blinded"), dataset = c("ADPP", "ADNCA"),
    variable = c("DOSEA", ""), detail = c("DOSEA/DOSEP gives the body surface area", "ok"))),
    file.path(ub, "private", "qc", "qc_report.json"), auto_unbox = TRUE)
  shiny::testServer(mod_blind_server, args = list(state = st), {
    result(list(released = FALSE, qc = "FAIL"))
    h <- output$run_result$html
    expect_match(h, "QC failed")
    expect_match(h, "ADPP: dose_ratio_reveals_bsa")
    expect_match(h, "body surface area")
    expect_false(grepl("ids_blinded", h))                   # only the failed checks
    expect_match(h, "Ask the AI why")
    session$setInputs(why_ai = 1)
    expect_equal(st$went, "ai")
    expect_equal(st$ai_context$task, "triage")
    expect_true(st$ai_context$release_failed)
    expect_match(st$ai_context$question, "because QC failed")
    result(list(released = FALSE, error = "input folder is empty")); session$flushReact()   # an error: message kept, AI offered
    expect_match(output$run_result$html, "input folder is empty")
    expect_match(output$run_result$html, "Ask the AI why")
    result(list(released = FALSE, error = "3 new row(s) were added to the codebook")); session$flushReact()   # codebook stop: unchanged
    expect_match(output$run_result$html, "Review the codebook")
    expect_false(grepl("Ask the AI why", output$run_result$html))
  })
})

test_that("Run blinding tab: the codebook-review hint links to the tab and is shown only until the review is done", {
  ub <- synthetic_ub()
  st <- app_state(ub)
  shiny::testServer(mod_blind_server, args = list(state = st), {
    expect_match(output$first_run_hint$html, "Codebook review")
    expect_match(output$first_run_hint$html, "stops once")
    session$setInputs(hint_codebook = 1)
    expect_equal(st$went, "codebook")
    session$setInputs(run = 1)                               # stops for the codebook
    expect_match(output$first_run_hint$html, "Codebook review")
    cb <- ub_codebook_read(ub); cb$CHECK <- ""; ub_codebook_write(ub, cb)
    st$refresh(); session$flushReact()
    expect_match(output$first_run_hint$html, "not confirmed")       # answered: only the confirmation is left
    ub_codebook_confirm_review(ub, "Test Reviewer")
    st$refresh(); session$flushReact()
    expect_false(grepl("Codebook review</a>", output$first_run_hint$html %||% ""))
    expect_match(output$first_run_hint$html, "confirmed by Test Reviewer")
  })
})

test_that("Codebook review confirmation: needs every row answered, a name, and again after a code changes", {
  ub <- synthetic_ub()
  expect_equal(ub_codebook_review_status(ub)$status, "none")
  try(ub_blind_folder(ub, ub_options_for_engine(ub_read_options(ub)), quiet = TRUE), silent = TRUE)
  cb <- ub_codebook_read(ub)
  if (any(cb$.status != "ok")) {
    expect_equal(ub_codebook_review_status(ub)$status, "open")
    expect_error(ub_codebook_confirm_review(ub, "A"), "still need an answer")
  }
  cb$CHECK <- ""; cb$BLINDED_VALUE[cb$BLINDED_VALUE == "?"] <- "TRT99"; ub_codebook_write(ub, cb)
  expect_equal(ub_codebook_review_status(ub)$status, "unconfirmed")
  expect_error(ub_codebook_confirm_review(ub, "  "), "name")
  ub_codebook_confirm_review(ub, "Igor Test")
  s <- ub_codebook_review_status(ub)
  expect_equal(s$status, "confirmed"); expect_equal(s$reviewer, "Igor Test")
  cb <- ub_codebook_read(ub); cb$NOTE[1] <- "a note only"; ub_codebook_write(ub, cb)
  expect_equal(ub_codebook_review_status(ub)$status, "confirmed")       # a NOTE keeps the confirmation
  cb$BLINDED_VALUE[1] <- paste0(cb$BLINDED_VALUE[1], "X"); ub_codebook_write(ub, cb)
  expect_equal(ub_codebook_review_status(ub)$status, "changed")         # a code change needs a new one
})

test_that("Codebook tab: the review is confirmed with a name; Run blinding asks for it first", {
  ub <- synthetic_ub()
  try(ub_blind_folder(ub, ub_options_for_engine(ub_read_options(ub)), quiet = TRUE), silent = TRUE)
  cb <- ub_codebook_read(ub); cb$CHECK <- ""; cb$BLINDED_VALUE[cb$BLINDED_VALUE == "?"] <- "TRT99"; ub_codebook_write(ub, cb)
  st <- app_state(ub)
  shiny::testServer(mod_codebook_server, args = list(state = st), {
    session$setInputs(only_attention = TRUE)
    expect_match(output$done_ui$html, "I have reviewed the codebook")
    session$setInputs(reviewer = "", confirm_review = 1)
    expect_equal(ub_codebook_review_status(ub)$status, "unconfirmed")
    expect_match(output$msg$html, "name")
    session$setInputs(reviewer = "Igor Test", confirm_review = 2)
    expect_equal(ub_codebook_review_status(ub)$status, "confirmed")
    expect_match(output$done_ui$html, "confirmed")
    expect_match(output$done_ui$html, "Igor Test")
  })
  cb <- ub_codebook_read(ub); cb$BLINDED_VALUE[1] <- paste0(cb$BLINDED_VALUE[1], "X"); ub_codebook_write(ub, cb)
  st2 <- app_state(ub)
  shiny::testServer(mod_blind_server, args = list(state = st2), {
    session$setInputs(run = 1)                                     # codes changed: blocked, nothing run
    expect_null(result())
    session$setInputs(to_codebook_gate = 1)
    expect_equal(st2$went, "codebook")
  })
})

test_that("Study tab: saved settings are shown and can be reset to the defaults after a confirmation", {
  d <- tempfile("study"); dir.create(d)
  ub <- ub_setup_study(d, "RST")$path
  o <- ub_read_options(ub); o$drop_vars <- list(ADNCA = list("RACE", "ETHNIC")); ub_write_options(ub, o, backup = FALSE)
  st <- app_state(ub)
  shiny::testServer(mod_study_server, args = list(state = st), {
    expect_match(output$status$html, "2 saved choice")
    expect_match(output$status$html, "Reset settings to defaults")
    session$setInputs(reset_settings = 1)                     # asks first: nothing changed yet
    expect_equal(nrow(ub_settings_changes(ub)), 2)
    session$setInputs(confirm_reset = 1)
    expect_equal(nrow(ub_settings_changes(ub)), 0)
    expect_false(is.null(st$settings_reload))
    expect_match(output$status$html, "the defaults")
  })
})

# ---- Workflow 3 in the app ---------------------------------------------------------------------------------------
lab_inputs <- function(session, on = TRUE) {
  session$setInputs(results_file = "lab_results.csv", sheet = "", analyte_mode = "several", check_dataset = "",
                    col_subject = "SUBJECT_ID", col_visit = "VISIT_NAME", col_nominal_time = "HOUR NOMINAL",
                    col_result = "CONCENTRATION", col_unit = "UNIT", col_lloq = "LLOQ", col_analyte = "ANALYTE",
                    col_specimen = "SPECIMEN", col_timepoint_name = "", keep_cols = c("RUN_DATE", "COMMENT"),
                    lab_release_on = on)
}

test_that("Prepare PC tab: the Workflow 3 switch is saved and Check matching shows the counts", {
  s <- make_lab_study()
  st <- app_state(s$ub)
  shiny::testServer(mod_prepare_pc_server, args = list(state = st), {
    session$flushReact()
    lab_inputs(session, on = TRUE)
    o <- ub_read_options(s$ub)
    expect_true(.ub_lab_release_on(o))
    expect_setequal(unlist(o$lab_release$keep_columns), c("RUN_DATE", "COMMENT"))
    expect_equal(o$prepare_pc$columns$result, "CONCENTRATION")
    session$setInputs(check_matching = 1)
    h <- output$match_ui$html
    expect_match(h, "matched")
    expect_match(h, "999-999")
    expect_false(dir.exists(file.path(s$ub, "private", "lab")))          # Check matching writes nothing
  })
})

test_that("Prepare PC tab: an unmatched stop proposes Workflow 3; a settings error does not", {
  s <- make_lab_study()
  st <- app_state(s$ub)
  shiny::testServer(mod_prepare_pc_server, args = list(state = st), {
    session$flushReact()
    lab_inputs(session, on = FALSE)
    expect_false(.ub_lab_release_on(ub_read_options(s$ub)))
    session$setInputs(run = 1)                                          # the 26 h row, the repeat and the stranger don't match
    h <- output$run_msg$html
    expect_match(h, "Nothing was written")
    expect_match(h, "could not be matched to PC")
    expect_match(h, "Switch to Workflow 3")
    session$setInputs(to_workflow3 = 1)
    expect_true(.ub_lab_release_on(ub_read_options(s$ub)))
    expect_match(output$match_ui$html, "matched")
    session$setInputs(col_result = "", run = 2)                          # a settings error: no Workflow 3 prompt
    expect_false(grepl("Switch to Workflow 3", output$run_msg$html))
  })
})

test_that("Blind & QC: the PC window offers Workflow 3; with Workflow 3 on, the run goes and reports the lab file", {
  s <- make_lab_study()
  o <- ub_read_options(s$ub); o$lab_release$enabled <- FALSE; ub_write_options(s$ub, o, backup = FALSE)
  st <- app_state(s$ub)
  shiny::testServer(mod_blind_server, args = list(state = st), {
    session$setInputs(run = 1)                                          # PC without results, lab file not merged: asks first
    expect_null(result())
    session$setInputs(to_workflow3_modal = 1)
    expect_equal(st$went, "prepare_pc")
    expect_false(is.null(st$wf3_request))
  })
  o$lab_release$enabled <- TRUE; ub_write_options(s$ub, o, backup = FALSE)
  st2 <- app_state(s$ub)
  shiny::testServer(mod_blind_server, args = list(state = st2), {
    session$setInputs(run = 1)                                          # no window: the first run goes (codebook stop)
    expect_false(is.null(result()))
    cb <- ub_codebook_read(s$ub); cb$CHECK <- ""; cb$BLINDED_VALUE[cb$BLINDED_VALUE == "?"] <- "-"; ub_codebook_write(s$ub, cb)
    ub_codebook_confirm_review(s$ub, "Test")
    session$setInputs(run = 2)
    expect_true(result()$released)
    expect_match(output$run_result$html, "Lab file released: \\d+ rows, \\d+ matched to PC")
  })
})

test_that("the PC notice names Workflow 3, and says so when it is on", {
  s <- make_lab_study()
  pc <- ub_check_pc_results(s$ub)
  on <- as.character(.ub_pc_notice(pc, list(ub_dir = s$ub), shiny::NS("x")))
  expect_match(on, "Workflow 3: the lab file will be released blinded next to PC")
  o <- ub_read_options(s$ub); o$lab_release$enabled <- FALSE; ub_write_options(s$ub, o, backup = FALSE)
  off <- as.character(.ub_pc_notice(pc, list(ub_dir = s$ub), shiny::NS("x")))
  expect_match(off, "Go to Prepare PC")
  expect_match(off, "release the lab file blinded \\(Workflow 3\\)")
})

test_that("Pre-flight: a row proposes Workflow 3 only while it is off, and Apply leaves it as a choice", {
  s <- make_lab_study()
  o <- ub_read_options(s$ub)
  expect_false(any(ub_preflight(s$ub, o)$code == "workflow3_possible"))
  o$lab_release$enabled <- FALSE
  r <- ub_preflight(s$ub, o)
  i <- which(r$code == "workflow3_possible")
  expect_length(i, 1)
  expect_match(r$message[i], "Workflow 3")
  ub_write_options(s$ub, o, backup = FALSE)
  a <- ub_preflight_apply(s$ub, r$suggestion[i])
  expect_equal(a$applied, 0); expect_length(a$choose, 1)
})
