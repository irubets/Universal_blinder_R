test_that("settings tables round-trip to blind_options.json", {
  o <- ub_default_options()
  o$dataset_aliases <- list(ADCONC = "ADNCA")
  o$identities$treatment_aliases <- list(FICTINIB = list("FCT", "Fic"))
  o$identities$private_terms <- list("SYN-PK-001")
  o$rename_vars <- list(ADEX = list(DOSEFCT = "DOSET01"))
  o$drop_vars <- list("*" = list("RACE", "ETHNIC"))
  o$qc_waivers <- list("k_anonymity:ADSL.AGE")
  o$qc_waiver_notes <- list("k_anonymity:ADSL.AGE" = list(reason = "reviewed", reviewer = "IR"))
  tb <- .ub_opts_to_tables(o)
  expect_equal(tb$treatment_aliases$ABBREVIATIONS, "FCT, Fic")
  expect_equal(nrow(tb$blank_where), 3)
  o2 <- .ub_tables_to_opts(tb, o, jsonlite::toJSON(o$config_overrides, auto_unbox = TRUE))
  expect_equal(o2$dataset_aliases, list(ADCONC = "ADNCA"))
  expect_equal(o2$identities$treatment_aliases$FICTINIB, list("FCT", "Fic"))
  expect_equal(o2$rename_vars$ADEX$DOSEFCT, "DOSET01")
  expect_equal(unlist(o2$drop_vars[["*"]]), c("RACE", "ETHNIC"))
  expect_equal(o2$blank_where[[1]]$when$DOSEPU, list("mg/m2"))
  expect_equal(o2$config_overrides$datasets$ADNCA$exact_vars$PCTEST, "analyte_name")
  expect_length(ub_validate_options(o2), 0)
  tb$qc_waivers$REASON <- ""
  expect_match(ub_validate_options(.ub_tables_to_opts(tb, o)), "needs a reason")
})

test_that("config_overrides switches turn on and off without touching other entries", {
  ov <- list(datasets = list(ADNCA = list(exact_vars = list(PCTESTCD = "analyte_code", PCTEST = "analyte_name"))))
  sw <- .UB_OVERRIDE_SWITCHES
  expect_true(.ub_switch_on(ov, sw$pctest_adnca))
  expect_false(.ub_switch_on(ov, sw$precision))
  ov2 <- .ub_switch_apply(ov, sw$precision, TRUE)
  expect_equal(ov2$datasets$ADNCA$analyte_numeric$precision_from, "PCORRES")
  expect_true(.ub_switch_on(ov2, sw$pctest_adnca))
  ov3 <- .ub_switch_apply(ov2, sw$pctest_adnca, FALSE)
  expect_false(.ub_switch_on(ov3, sw$pctest_adnca))
  expect_equal(ov3$datasets$ADNCA$analyte_numeric$precision_from, "PCORRES")
})

test_that("the app UI builds and the server starts", {
  skip_if_not_installed("shiny")
  ui <- app_ui()
  expect_s3_class(ui, "shiny.tag.list")
  html <- as.character(ui)
  for (t in c("Workflow", "Study", "Prepare PC", "Study settings", "Codebook review", "Blind &amp; QC", "Retrieve",
              "AI assistant", "Setup / About")) expect_match(html, t, fixed = TRUE)
  shiny::testServer(app_server, {
    expect_null(state$ub_dir)
  })
})

test_that("study module opens a pasted UB folder with quotes", {
  d <- tempfile("study"); dir.create(d)
  ub <- ub_setup_study(d, "S1")$path
  shiny::testServer(mod_study_server, args = list(state = shiny::reactiveValues(ub_dir = NULL, study_dir = NULL, tick = 0,
      refresh = function() NULL, goto = function(t, step = NULL) NULL)), {
    session$setInputs(pasted = paste0('"', ub, '"'))
    session$setInputs(open_pasted = 1)
    expect_equal(normalizePath(state$ub_dir), normalizePath(ub))
  })
})

test_that("the user guide shown in the app is the repository's", {
  root <- test_path("..", "..", "USERGUIDE.md")
  skip_if_not(file.exists(root))
  inst <- system.file("app", "www", "docs", "USERGUIDE.md", package = "universalblinder")
  expect_identical(readLines(inst, warn = FALSE), readLines(root, warn = FALSE))
})

test_that("USERGUIDE.html is rebuilt after USERGUIDE.md changes, and the guide's diagram is installed", {
  md <- test_path("..", "..", "USERGUIDE.md")
  html <- test_path("..", "..", "USERGUIDE.html")
  skip_if_not(file.exists(md))
  expect_true(file.exists(html))
  h <- gsub("\\s+", " ", paste(readLines(html, warn = FALSE, encoding = "UTF-8"), collapse = " "))   # pandoc wraps lines
  emb <- grepl("data:image/svg", h, fixed = TRUE)
  h <- gsub("<[^>]+>", "", h)                                     # headings may hold <code>
  h <- gsub("\\s+", " ", h)
  heads <- sub("^#+ ", "", grep("^#{2,3} ", readLines(md, warn = FALSE, encoding = "UTF-8"), value = TRUE))
  heads <- gsub("[*`]", "", heads)
  missing <- heads[!vapply(heads, function(x) grepl(x, h, fixed = TRUE) ||
                             grepl(gsub("&", "&amp;", x, fixed = TRUE), h, fixed = TRUE), logical(1))]
  expect_equal(missing, character(), info = "USERGUIDE.html is out of date: run Rscript tools/build_userguide.R")
  expect_true(emb)                                         # the diagram is embedded: one self-contained file
  expect_true(nzchar(system.file("app", "www", "docs", "workflow.svg", package = "universalblinder")))
  expect_match(as.character(.ub_read_md(system.file("app", "www", "docs", "USERGUIDE.md", package = "universalblinder"))),
               "ubwww/docs/workflow.svg", fixed = TRUE)
})

test_that("run_app uses port 3838 by default; a port in use is detected", {
  expect_equal(formals(run_app)$port, quote(getOption("shiny.port", 3838L)))
  port <- httpuv::randomPort()
  expect_true(.ub_port_free(port))
  srv <- httpuv::startServer("127.0.0.1", port, list(call = function(req) list(status = 200L, headers = list(), body = "")))
  on.exit(httpuv::stopServer(srv))
  expect_false(.ub_port_free(port))
})

test_that("an R session holding an older copy of the package than the one installed is detected", {
  lib <- tempfile("lib"); dir.create(file.path(lib, "Meta"), recursive = TRUE)
  meta <- file.path(lib, "Meta", "package.rds"); saveRDS(list(), meta)
  stamp <- .ub_install_stamp(lib)
  expect_false(is.na(stamp))
  expect_false(.ub_session_stale(lib, stamp))
  Sys.setFileTime(meta, Sys.time() + 60)                      # reinstalled after this session loaded it
  expect_true(.ub_session_stale(lib, stamp))
  expect_false(.ub_session_stale(tempfile("nolib"), NA))      # devtools::load_all: no installed copy, no check
  expect_match(.ub_stale_message(), "Restart R")
})
