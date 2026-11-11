# ub_preflight(): pre-flight check of a study folder, on the synthetic example with planted issues.

pf_study <- function() {
  f <- tempfile("pfstudy")
  dir.create(file.path(f, "input"), recursive = TRUE)
  src <- system.file("examples", "synthetic", "source", package = "universalblinder")
  file.copy(list.files(src, pattern = "\\.xpt$", full.names = TRUE), file.path(f, "input"))
  f
}

pf_edit <- function(f, name, fn) {
  p <- file.path(f, "input", paste0(name, ".xpt"))
  rd <- ub_read_dataset(p)
  ub_write_dataset(fn(rd$data), p, rd$meta, name = toupper(name))
}

test_that("the synthetic example has no red findings and only names outside sample_values", {
  f <- pf_study()
  r <- ub_preflight(f)
  expect_named(r, c("severity", "code", "dataset", "variable", "message", "suggestion", "sample_values"))
  expect_false(any(r$severity == "red"), info = paste(r$code, r$dataset, r$variable, collapse = "; "))
  expect_true(all(r$severity %in% c("red", "amber", "info")))
})

test_that("planted issues are found, values stay in sample_values, and applied options are skipped", {
  f <- pf_study()
  pf_edit(f, "adnca", function(d) {
    d$PCTEST <- d$ATEST; d$ATEST <- NULL; d$ARESC <- NULL
    d$EXTRT <- d$TREAT
    d
  })
  pf_edit(f, "pc", function(d) {
    d$SUBJID <- sub("^SYN-PK-001-", "", d$USUBJID)
    d$PCORRES[2] <- "<0.731"
    d$PCORRES[1] <- sub("^BQL", "BLQ", d$PCORRES[1])
    d$PCREASND <- ""; d$PCREASND[3] <- "WITHDREW CONSENT ON 3JUN2021"
    d
  })
  pf_edit(f, "ex", function(d) { d$EXDOSE[1] <- 0; d })
  file.copy(file.path(f, "input", "ec.xpt"), file.path(f, "input", "zz.xpt"))
  r <- ub_preflight(f)
  has <- function(code, ds, var = NULL) any(r$code == code & r$dataset == ds & (is.null(var) | grepl(if (is.null(var)) "" else var, r$variable)))
  expect_true(has("no_profile", "ZZ"))
  expect_true(has("pctest_without_atest", "ADNCA", "PCTEST"))
  expect_true(has("precision_from_pcorres", "ADNCA"))
  expect_true(has("adam_treatment_text", "ADNCA", "EXTRT"))
  expect_true(has("pc_subjid", "PC", "SUBJID"))
  expect_true(has("result_text_unrecognised", "PC", "PCORRES"))
  expect_true(has("blq_spelling", "PC", "PCORRES"))
  expect_true(has("free_text_date", "PC", "PCREASND"))
  expect_true(has("dose_zero", "EX", "EXDOSE"))
  expect_true(has("rare_demographics", "ADNCA", "RACE"))
  expect_equal(r$severity[r$code == "pctest_without_atest"], "red")
  expect_equal(r$severity[r$code == "no_profile"], "red")
  expect_match(r$suggestion[r$code == "pctest_without_atest"], '"exact_vars":{"PCTEST":"analyte_name"}', fixed = TRUE)
  expect_match(r$sample_values[r$code == "result_text_unrecognised"], "<0.731", fixed = TRUE)
  # no data value outside sample_values
  planted <- c("<0.731", "Synthazole", "SYNTHAZOLE", "Fictinib", "3JUN2021", "SYN-PK-001", "901-100", "BLQ<(0")
  other <- unlist(r[, setdiff(names(r), "sample_values")])
  for (v in planted) expect_false(any(grepl(v, other, fixed = TRUE)), info = v)
  # applied options are not suggested again
  opts <- list(dataset_aliases = list(ZZ = "EC"),
               config_overrides = list(datasets = list(ADNCA = list(exact_vars = list(PCTEST = "analyte_name"),
                                                                    text_vars = list(EXTRT = "treatment"),
                                                                    analyte_numeric = list(precision_from = "PCORRES")),
                                                       PC = list(id_vars = list(subjid = "SUBJID")))),
               drop_vars = list(ADNCA = list("RACE"), EX = list("EXDOSE")),
               extra_suppress_vars = list(PC = list("PCREASND")))
  r2 <- ub_preflight(f, opts)
  for (cd in c("no_profile", "pctest_without_atest", "precision_from_pcorres", "pc_subjid", "free_text_date", "dose_zero"))
    expect_false(any(r2$code == cd), info = cd)
  expect_false(any(r2$code == "adam_treatment_text" & r2$variable == "EXTRT"))
  expect_false(any(r2$code == "rare_demographics" & r2$dataset == "ADNCA"))
})

test_that("PC without results and no exposure dataset are reported", {
  f <- pf_study()
  file.remove(file.path(f, "input", c("ex.xpt", "ec.xpt", "adex.xpt")))
  pf_edit(f, "pc", function(d) { d$PCORRES <- ""; d$PCSTRESC <- ""; d$PCSTRESN <- NA_real_; d })
  writeLines("a,b", file.path(f, "input", "lab_results.csv"))
  r <- ub_preflight(f)
  expect_true(any(r$code == "no_exposure"))
  expect_equal(r$severity[r$code == "pc_without_results"], "red")
  expect_equal(r$sample_values[r$code == "pc_without_results"], "lab_results.csv")
  expect_match(r$suggestion[r$code == "pc_without_results"], "ub_prepare_pc", fixed = TRUE)
})

test_that("a pre-flight suggestion is turned into setting paths; alternatives and conditions are not", {
  s <- .ub_pf_setting('drop_vars {"ADNCA":["RACE"]}.')
  expect_equal(s$path, "drop_vars/ADNCA"); expect_equal(s$value, '["RACE"]')
  s <- .ub_pf_setting('config_overrides: {"datasets":{"ADNCA":{"text_vars":{"APROFILE":"any"}}}}')
  expect_equal(s$path, "config_overrides/datasets/ADNCA/text_vars/APROFILE"); expect_equal(s$value, '"any"')
  s <- .ub_pf_setting('config_overrides: {"datasets":{"ADNCA":{"text_vars":{"EXTRT":"treatment"}}}} (and keep an exposure dataset in the input folder)')
  expect_equal(s$path, "config_overrides/datasets/ADNCA/text_vars/EXTRT")
  s <- .ub_pf_setting('config_overrides: {"datasets":{"ADNCA":{"exact_vars":{"PCTESTCD":"analyte_code","PCTEST":"analyte_name"}}}}')
  expect_equal(sort(s$path), sort(c("config_overrides/datasets/ADNCA/exact_vars/PCTESTCD", "config_overrides/datasets/ADNCA/exact_vars/PCTEST")))
  s <- .ub_pf_setting('extra_suppress_vars {"PC":["PCREASND"]} (blanked; the originals come back on retrieval).')
  expect_equal(s$path, "extra_suppress_vars/PC"); expect_equal(s$value, '["PCREASND"]')
  # a choice between settings, a condition, or no setting at all: left for the user
  expect_null(.ub_pf_setting('config_overrides: {"datasets":{"ADNCA":{"text_vars":{"X":"treatment"}}}}, or drop_vars {"ADNCA":["X"]}'))
  expect_null(.ub_pf_setting('drop_vars {"ADEX":["DOSEP"]}, or blank_where for the records with a mg dose.'))
  expect_null(.ub_pf_setting('drop_vars {"EX":["EXDOSE"]} (or remove the placebo records).'))
  expect_null(.ub_pf_setting('If it is mg/m2: drop_vars {"ADPP":["DOSEP"]}.'))
  expect_null(.ub_pf_setting("Copy an exposure dataset (ex.xpt, ec.xpt or adex.xpt) into the input folder, if the study has one."))
  expect_null(.ub_pf_setting(""))
  expect_null(.ub_pf_setting(NA_character_))
})

test_that("applying pre-flight suggestions merges drop_vars, keeps a backup, and the findings disappear", {
  f <- pf_study()
  ub <- ub_setup_study(dirname(f), "PFA")$path
  file.copy(list.files(file.path(f, "input"), full.names = TRUE), file.path(ub, "input"))
  pf_edit(ub, "adnca", function(d) { d$ETHNIC <- rep(c("HISPANIC OR LATINO", "NOT HISPANIC OR LATINO"), length.out = nrow(d)); d })
  o <- ub_read_options(ub); o$drop_vars <- list(ADNCA = list("AGEGR1")); ub_write_options(ub, o, backup = FALSE)
  r <- ub_preflight(ub, ub_read_options(ub))
  pick <- r$code == "rare_demographics" & r$dataset == "ADNCA"
  expect_equal(sum(pick), 2)
  sugg <- c(r$suggestion[pick], 'drop_vars {"EX":["EXDOSE"]} (or remove the placebo records).')
  a <- ub_preflight_apply(ub, sugg)
  expect_equal(a$applied, 2L)
  expect_equal(a$choose, 3L)
  expect_length(a$problems, 0)
  expect_setequal(unlist(ub_read_options(ub)$drop_vars$ADNCA), c("AGEGR1", "RACE", "ETHNIC"))
  expect_null(ub_read_options(ub)$drop_vars$EX)
  expect_true(length(list.files(file.path(ub, "private", "settings_backup"))) >= 1)
  r2 <- ub_preflight(ub, ub_read_options(ub))
  expect_false(any(r2$code == "rare_demographics" & r2$dataset == "ADNCA"))
  # nothing applicable selected
  a0 <- ub_preflight_apply(ub, "Copy an exposure dataset into the input folder.")
  expect_equal(a0$applied, 0L); expect_equal(a0$choose, 1L)
})

test_that("a DOSEP level given per m2 is found from DOSEA/DOSEP: DOSEA strongly suggested, DOSEP optional", {
  f <- pf_study()
  pf_edit(f, "adnca", function(d) {
    u <- unique(d$USUBJID); bsa <- u[seq_len(12)]
    m <- d$USUBJID %in% bsa
    d$DOSEP <- ifelse(m, 75, d$DOSEA)
    d$DOSEA <- ifelse(m, round(75 * d$BSABL), d$DOSEA)
    d
  })
  rows <- function(opts = NULL) { r <- ub_preflight(f, opts); r[r$code == "dose_reveals_bsa" & r$dataset == "ADNCA", ] }
  r <- rows()
  expect_setequal(r$variable, c("DOSEA", "DOSEP"))
  expect_equal(r$severity[r$variable == "DOSEA"], "red")
  expect_equal(r$severity[r$variable == "DOSEP"], "amber")
  expect_match(r$message[r$variable == "DOSEA"], "STRONGLY RECOMMENDED")
  expect_equal(r$suggestion[r$variable == "DOSEA"], 'drop_vars {"ADNCA":["DOSEA"]}.')
  expect_equal(r$suggestion[r$variable == "DOSEP"], 'drop_vars {"ADNCA":["DOSEP"]}.')
  expect_false(grepl("75", paste(r$message, r$suggestion, collapse = " ")))   # values only in sample_values
  # both rows can be applied together
  expect_equal(nrow(do.call(rbind, lapply(r$suggestion, .ub_pf_setting))), 2)
  # DOSEP left out: DOSEA is still strongly suggested, but no longer a QC stop
  r <- rows(list(drop_vars = list(ADNCA = list("DOSEP"))))
  expect_equal(r$variable, "DOSEA"); expect_equal(r$severity, "amber")
  # a blank_where whose condition variable is not in the dataset does not count
  r <- rows(list(blank_where = list(list(datasets = list("AD*"), vars = list("DOSEP"), when = list(DOSEPU = list("mg/m2"))))))
  expect_equal(r$severity[r$variable == "DOSEA"], "red")
  expect_true("DOSEP" %in% r$variable)
  # DOSEA left out: nothing left to suggest
  expect_equal(nrow(rows(list(drop_vars = list(ADNCA = list("DOSEA"))))), 0)
})

test_that("a blank_where rule counts in pre-flight only when its condition variable exists", {
  f <- pf_study()
  pf_edit(f, "adex", function(d) { d$DOSEP <- d$DOSEA; d })
  has_unit <- function(opts) any(ub_preflight(f, opts)$code == "dose_unit_unknown")
  bw <- list(blank_where = list(list(datasets = list("AD*"), vars = list("DOSEP"), when = list(DOSEPU = list("mg/m2")))))
  expect_true(has_unit(bw))
  bw2 <- list(blank_where = list(list(datasets = list("AD*"), vars = list("DOSEP"), when = list(DOSEP = list("75")))))
  expect_false(has_unit(bw2))
})
