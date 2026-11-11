# DM, VS, PP, ADPP, ADPD and SUPPPC: R must reproduce the Python reference output exactly, and the
# whole batch must pass on the extended synthetic study (tests/testthat/fixture_ext, built by
# `python -m universal_blinder.fixture <dir> --extended`).

.cmp_expected <- function(r, exp, nm) {
  expect_identical(names(r$data), names(exp))
  for (c in names(exp)) {
    x <- r$data[[c]]
    if (inherits(x, "Date")) x <- as.numeric(x - as.Date("1960-01-01"))
    if (inherits(x, "POSIXct")) x <- as.numeric(x) + 315619200
    if (inherits(x, "hms")) x <- as.numeric(x)
    if (is.numeric(x)) {
      e <- suppressWarnings(as.numeric(exp[[c]]))
      ok <- (is.na(x) & is.na(e)) | (!is.na(x) & !is.na(e) & abs(x - e) <= 1e-9 * pmax(1, abs(e)))
    } else ok <- x == exp[[c]]
    expect_true(all(ok), info = paste(nm, c))
  }
}

test_that("R reproduces the Python reference output for the extended profiles (same key)", {
  fx <- test_path("fixture_ext")
  cfg <- ub_load_config(file.path(fx, "blinding_config.json"))
  cfg$datasets$SUPPPC <- cfg$datasets$SUPPQUAL
  keys <- ub_load_keys(file.path(fx, "maps"))
  for (fn in list.files(file.path(fx, "source"), pattern = "\\.xpt$")) {
    nm <- toupper(sub("\\.xpt$", "", fn))
    src <- ub_read_dataset(file.path(fx, "source", fn))
    r <- ub_transform(src$data, src$meta, nm, cfg, keys, "blind")
    exp <- utils::read.csv(file.path(fx, "expected_blinded", paste0(tolower(nm), ".csv")), colClasses = "character",
                           na.strings = character(), check.names = FALSE)
    .cmp_expected(r, exp, nm)
  }
})

test_that("full batch passes on the extended study, with mixed STUDYIDs restored", {
  f <- file.path(tempfile("ext"))
  dir.create(file.path(f, "input"), recursive = TRUE)
  file.copy(list.files(test_path("fixture_ext", "source"), full.names = TRUE), file.path(f, "input"))
  opts <- list(identities = list(treatment_aliases = list(FICTINIB = c("FCT", "Fct"), SYNTHAZOLE = c("SYZ", "Syz")),
                                 analyte_aliases = list(SYNZM1 = "SZ-M1")),
               rename_vars = list(ADSL = list(FCTFL = "TRTFL01", SYZFL = "TRTFL02"),
                                  ADEX = list(DOSEFCT = "DOSET01", DOSESYZ = "DOSET02")))
  first_run(f, opts)
  res <- ub_blind_folder(f, opts, quiet = TRUE)
  expect_true(res$released, info = paste(res$error, collapse = " "))
  expect_true("supppc.xpt" %in% list.files(file.path(f, "output")))
  for (fn in list.files(file.path(f, "output"), pattern = "\\.xpt$", full.names = TRUE)) {
    x <- ub_read_dataset(fn)
    blob <- paste(c(unlist(lapply(x$data, function(v) unique(as.character(v)))), names(x$data), x$meta$labels, x$meta$file_label), collapse = " ")
    expect_false(grepl(REAL_LOOKING, blob, perl = TRUE), info = basename(fn))
  }
  un <- function(n) ub_read_dataset(file.path(f, "private", "unblinded", n))$data
  expect_true(all(un("pp.xpt")$STUDYID == "SYN-PK-001-A1"))
  expect_true(all(un("dm.xpt")$STUDYID == "SYN-PK-001"))
  # VS weight uses the ADSL WGTBL factor; other vitals untouched
  s <- ub_read_dataset(file.path(f, "input", "vs.xpt"))$data
  b <- source_order(file.path(f, "private"), "VS", ub_read_dataset(file.path(f, "output", "vs.xpt"))$data)
  expect_true(all(s$VSSTRESN[s$VSTESTCD == "HR"] == b$VSSTRESN[s$VSTESTCD == "HR"]))
  expect_true(any(s$VSSTRESN[s$VSTESTCD == "WEIGHT"] != b$VSSTRESN[s$VSTESTCD == "WEIGHT"]))

  # linked datasets stay traceable: values that correspond in the source (CDISC SDTMIG PP: PPCAT = PCTEST,
  # PPRFTDTC = PCRFTDTC, PPSPEC = PCSPEC; ADaM: same SUBJID/USUBJID, ATEST, visits) still correspond after
  # blinding, even when PP names the analyte by an alias (SZ-M1 = Synthazole M1)
  out <- function(n) ub_read_dataset(file.path(f, "output", n))$data
  pc <- out("pc.xpt"); pp <- out("pp.xpt"); adnca <- out("adnca.xpt"); adpp <- out("adpp.xpt"); adpd <- out("adpd.xpt")
  dm <- out("dm.xpt"); adsl <- out("adsl.xpt")
  pair <- function(d, a, b) unique(paste(d[[a]], d[[b]], sep = "\r"))
  expect_setequal(unique(pp$PPCAT), unique(pc$PCTEST))
  expect_true(all(pair(pp, "USUBJID", "PPCAT") %in% pair(pc, "USUBJID", "PCTEST")))
  expect_true(all(pair(pp, "USUBJID", "PPRFTDTC") %in% pair(pc, "USUBJID", "PCRFTDTC")))
  expect_true(all(pair(pp, "USUBJID", "VISIT") %in% pair(pc, "USUBJID", "VISIT")))
  expect_true(all(unique(pp$PPSPEC) %in% pc$PCSPEC))
  expect_setequal(unique(adpp$ATEST), unique(adnca$ATEST))
  expect_true(all(adpp$PPCAT == adpp$ATEST))
  expect_true(all(pair(adpp, "SUBJID", "ATEST") %in% pair(adnca, "SUBJID", "ATEST")))
  expect_true(all(pair(adpd, "USUBJID", "SUBJID") %in% pair(adnca, "USUBJID", "SUBJID")))
  expect_false(any(adpd$ATEST %in% adnca$ATEST))   # the PD biomarker is its own analyte
  expect_true(all(pair(adnca, "USUBJID", "AGE") %in% pair(adsl, "USUBJID", "AGE")))
  expect_true(all(pair(adpp, "USUBJID", "WGTBL") %in% pair(adsl, "USUBJID", "WGTBL")))
  expect_true(all(pair(dm, "USUBJID", "AGE") %in% pair(adsl, "USUBJID", "AGE")))
})

test_that("an unclassified PK parameter stops blinding", {
  fx <- test_path("fixture_ext")
  cfg <- ub_load_config(file.path(fx, "blinding_config.json"))
  keys <- ub_load_keys(file.path(fx, "maps"))
  src <- ub_read_dataset(file.path(fx, "source", "pp.xpt"))
  src$data$PPTESTCD[1] <- "NEWPARAM"
  expect_error(ub_transform(src$data, src$meta, "PP", cfg, keys, "blind"), "not classified")
})

test_that("BQL-imputed values are released as missing (default) or flagged", {
  fx <- test_path("fixture_ext")
  cfg <- ub_load_config(file.path(fx, "blinding_config.json"))
  keys <- ub_load_keys(file.path(fx, "maps"))
  src <- ub_read_dataset(file.path(fx, "source", "adpd.xpt"))
  bq <- startsWith(src$data$ARESC, "BQL")
  expect_true(any(bq))
  src$data$AVAL[bq] <- 0.5
  r <- ub_transform(src$data, src$meta, "ADPD", cfg, keys, "blind")
  expect_true(all(is.na(r$data$AVAL[bq])) && all(is.na(r$data$ACHG[bq])) && all(!is.na(r$data$AVAL[!bq])))
  expect_false(any(vapply(r$report$checks, function(c) c[[2]] == "bql_imputed_value", logical(1))))
  cfg$numeric_blinding$bql_imputed_action <- "flag"
  r <- ub_transform(src$data, src$meta, "ADPD", cfg, keys, "blind")
  expect_true(any(vapply(r$report$checks, function(c) c[[2]] == "bql_imputed_value", logical(1))))
  back <- ub_transform(r$data, r$meta, "ADPD", cfg, keys, "unblind")
  expect_true(all(abs(back$data$AVAL[bq] - 0.5) < 1e-12))
})

test_that("config_overrides reach the run without a 'config' entry (no partial matching)", {
  f <- file.path(tempfile("ovr"))
  dir.create(file.path(f, "input"), recursive = TRUE)
  file.copy(test_path("fixture_ext", "source", c("adsl.xpt", "ex.xpt", "pc.xpt", "pp.xpt")), file.path(f, "input"))
  cfg <- ub_load_config()
  sc <- setdiff(.chr_all <- unlist(cfg$param_scaling_tables$PK_PARAMETERS$scale), "CMAX")
  un <- c(unlist(cfg$param_scaling_tables$PK_PARAMETERS$unscaled), "CMAX")
  opts <- list(identities = list(treatment_aliases = list(FICTINIB = "FCT", SYNTHAZOLE = "SYZ"), analyte_aliases = list(SYNZM1 = "SZ-M1")),
               rename_vars = list(ADSL = list(FCTFL = "TRTFL01", SYZFL = "TRTFL02")),
               config_overrides = list(param_scaling_tables = list(PK_PARAMETERS = list(scale = sc, unscaled = un))))
  first_run(f, opts)
  res <- ub_blind_folder(f, opts, quiet = TRUE, steps = c("build_maps", "blind"))
  s <- ub_read_dataset(file.path(f, "input", "pp.xpt"))$data
  b <- ub_read_dataset(file.path(f, "private", "blinded", "pp.xpt"))$data
  expect_equal(sort(b$PPSTRESN[b$PPTESTCD == "CMAX"]), sort(s$PPSTRESN[s$PPTESTCD == "CMAX"]), tolerance = 1e-9)  # declared unscaled
})

test_that("an unknown analyte name in PP says what alias to add", {
  fx <- test_path("fixture_ext")
  cfg <- ub_load_config(file.path(fx, "blinding_config.json"))
  src <- lapply(c(PC = "pc", PP = "pp"), function(n) ub_read_dataset(file.path(fx, "source", paste0(n, ".xpt"))))
  e <- tryCatch(ub_build_keys(src, cfg, list(), list(), seed = 1), error = function(e) conditionMessage(e))
  expect_match(e, "'SZ-M1'", fixed = TRUE)
  expect_match(e, "SYNZM1 = Synthazole M1", fixed = TRUE)
})
