test_that("pasted paths are cleaned: quotes, slashes, trailing separators", {
  win <- .Platform$OS.type == "windows"
  p <- "C:\\x\\ABC-123"
  expect_equal(ub_clean_path(paste0('"', p, '"')), p)
  expect_equal(ub_clean_path(p), p)
  expect_equal(ub_clean_path(paste0("  '", p, "\\'  ")), p)
  expect_equal(ub_clean_path("\u201cC:\\x\\ABC 123\u201d"), "C:\\x\\ABC 123")
  if (win) {
    expect_equal(ub_clean_path("C:/x/ABC-123/"), p)
    expect_equal(ub_clean_path('"C:\\"'), "C:\\")
    expect_equal(ub_clean_path("\\\\server\\share\\study\\"), "\\\\server\\share\\study")
  }
  expect_equal(ub_clean_path(""), "")
  expect_equal(ub_clean_path(NULL), "")
  expect_equal(ub_clean_path('""'), "")
})

make_study_dir <- function() {
  d <- tempfile("study")
  dir.create(file.path(d, "sdtm"), recursive = TRUE)
  dir.create(file.path(d, "adam"), recursive = TRUE)
  src <- system.file("examples", "synthetic", "source", package = "universalblinder")
  for (f in c("pc.xpt", "ex.xpt", "adsl.xpt")) file.copy(file.path(src, f), file.path(d, "sdtm", f))
  file.copy(file.path(src, "adnca.xpt"), file.path(d, "adam", "adnca.xpt"))
  writeLines("SUBJECT,RESULT\n1,2", file.path(d, "adam", "lab_results.csv"))
  d
}

test_that("UB folder is created inside the study folder and never overwritten", {
  d <- make_study_dir()
  sn <- ub_guess_study_number(d)
  expect_true(nzchar(sn))
  r <- ub_setup_study(paste0('"', d, '"'), "ABC/123")
  expect_true(r$created)
  expect_equal(basename(r$path), "UB_ABC-123")
  for (x in c("input", "output", "private", "blind_options.json", "blind.R", "prepare_pc.R", "retrieve.R"))
    expect_true(file.exists(file.path(r$path, x)), info = x)
  expect_true(ub_is_ub_folder(r$path))
  writeLines("{\"mine\": 1}", file.path(r$path, "blind_options.json"))
  r2 <- ub_setup_study(d, "ABC/123")
  expect_false(r2$created)
  expect_equal(readLines(file.path(r$path, "blind_options.json")), "{\"mine\": 1}")
})

test_that("scan lists datasets with profile and role, skips the UB folder, marks copied files", {
  d <- make_study_dir()
  ub <- ub_setup_study(d, "S1")$path
  s <- ub_scan_study(d, ub)
  expect_setequal(s$file, c("pc.xpt", "ex.xpt", "adsl.xpt", "adnca.xpt", "lab_results.csv"))
  expect_equal(s$role[s$file == "pc.xpt"], "target")
  expect_equal(s$role[s$file == "ex.xpt"], "exposure")
  expect_equal(s$role[s$file == "lab_results.csv"], "results file?")
  expect_equal(s$role[s$file == "adsl.xpt"], "subject-level (optional)")
  expect_false(s$preselect[s$file == "adsl.xpt"])
  expect_equal(.ub_role_of(c("PP", "ADPP", "ADPD", "DM", "VS", "SUPPQUAL")),
               c("linked: PK parameters", "linked: PK parameters", "linked: PD", "subject-level (optional)", "other", "supplemental"))
  expect_true(all(s$preselect[s$file %in% c("pc.xpt", "ex.xpt", "adnca.xpt")]))
  expect_false(any(s$already_copied))
  cp <- ub_copy_to_input(ub, s$path[s$preselect])
  expect_true(all(cp$status == "copied"))
  s2 <- ub_scan_study(d, ub)
  expect_true(all(s2$already_copied[s2$preselect]))
  expect_false(any(grepl("UB_", s2$relative)))
  expect_true(all(ub_copy_to_input(ub, s$path[s$preselect])$status == "already there"))
})

test_that("settings round trip, validation of waivers, engine subset", {
  d <- tempfile("ub"); dir.create(d)
  o <- ub_default_options()
  expect_length(ub_validate_options(o), 0)
  o$qc_waivers <- list("residual_identity_text:ADNCA.PCTEST")
  expect_match(ub_validate_options(o), "needs a reason and a reviewer")
  o$qc_waiver_notes <- list("residual_identity_text:ADNCA.PCTEST" = list(reason = "checked", reviewer = "IR"))
  expect_length(ub_validate_options(o), 0)
  ub_write_options(d, o, backup = FALSE)
  o2 <- ub_read_options(d)
  expect_equal(o2$config_overrides$datasets$ADNCA$exact_vars$PCTEST, "analyte_name")
  e <- ub_options_for_engine(o2)
  expect_false(any(c("prepare_pc", "qc_waiver_notes", "_help") %in% names(e)))
  expect_identical(e$qc_waivers, "residual_identity_text:ADNCA.PCTEST")
  ub_write_options(d, o2)
  expect_length(list.files(file.path(d, "private", "settings_backup")), 1)
})

test_that("README files in input are not taken for lab results files", {
  d <- tempfile("study"); dir.create(d)
  ub <- ub_setup_study(d, "S1")$path
  expect_true(file.exists(file.path(ub, "input", "README.txt")))
  expect_length(ub_lab_files(ub), 0)
  writeLines("A,B\n1,2", file.path(ub, "input", "lab.csv"))
  expect_equal(ub_lab_files(ub), "lab.csv")
  expect_equal(ub_study_status(ub)$lab_files, "lab.csv")
})

test_that("saved settings that differ from the defaults are listed, and can be reset with a backup", {
  d <- tempfile("study"); dir.create(d)
  ub <- ub_setup_study(d, "RST")$path
  expect_equal(nrow(ub_settings_changes(ub)), 0)
  o <- ub_read_options(ub)
  o$drop_vars <- list(ADNCA = list("RACE", "ETHNIC"))
  o$config_overrides$datasets$ADNCA$text_vars <- list(APROFILE = "any")
  o$blank_where <- o$blank_where[-1]
  ub_write_options(ub, o, backup = FALSE)
  ch <- ub_settings_changes(ub)
  expect_equal(sum(ch$change == "added"), 3)
  expect_equal(sum(ch$change == "removed"), 1)
  expect_setequal(unique(ch$setting), c("drop_vars", "config_overrides", "blank_where"))
  expect_true(any(grepl("RACE", ch$item)))
  ub_reset_options(ub)
  expect_equal(nrow(ub_settings_changes(ub)), 0)
  b <- list.files(file.path(ub, "private", "settings_backup"), full.names = TRUE)
  expect_length(b, 1)
  expect_true(any(grepl("ETHNIC", readLines(b))))
  # a study without blind_options.json uses the defaults: nothing to list
  unlink(file.path(ub, "blind_options.json"))
  expect_equal(nrow(ub_settings_changes(ub)), 0)
})
