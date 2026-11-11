# record identifiers, re-sorting, drop/blank options, crosswalk verification and local retrieval

ext_folder <- function(opts = list()) {
  f <- file.path(tempfile("rec"))
  dir.create(file.path(f, "input"), recursive = TRUE)
  file.copy(list.files(test_path("fixture_ext", "source"), full.names = TRUE), file.path(f, "input"))
  base <- list(identities = list(treatment_aliases = list(FICTINIB = c("FCT", "Fct"), SYNTHAZOLE = c("SYZ", "Syz")),
                                 analyte_aliases = list(SYNZM1 = "SZ-M1")),
               rename_vars = list(ADSL = list(FCTFL = "TRTFL01", SYZFL = "TRTFL02"),
                                  ADEX = list(DOSEFCT = "DOSET01", DOSESYZ = "DOSET02")))
  first_run(f, c(base, opts))
  res <- ub_blind_folder(f, c(base, opts), quiet = TRUE)
  list(f = f, res = res, opts = c(base, opts))
}

test_that("record ids, order, drop and blank_where", {
  e <- ext_folder(list(drop_vars = list("*" = c("RACE", "ETHNIC", "LSTALVDT")),
                       blank_where = list(list(datasets = "ADEX", vars = "DOSEA", when = list(DOSEAU = "mg/m2")))))
  expect_true(e$res$released)
  seen <- character()
  for (fn in list.files(file.path(e$f, "output"), pattern = "\\.xpt$", full.names = TRUE)) {
    b <- ub_read_dataset(fn)$data
    expect_identical(names(b)[1], "UBRECID")
    expect_true(all(grepl("^[A-HJ-NP-Z2-9]{8}$", b$UBRECID)))
    expect_false(any(b$UBRECID %in% seen)); seen <- c(seen, b$UBRECID)
    expect_identical(as.vector(b$USUBJID), sort(as.vector(b$USUBJID), method = "radix"))
    expect_false(any(c("RACE", "ETHNIC", "LSTALVDT") %in% names(b)))
  }
  s <- ub_read_dataset(file.path(e$f, "input", "adex.xpt"))$data
  b <- source_order(file.path(e$f, "private"), "ADEX", ub_read_dataset(file.path(e$f, "output", "adex.xpt"))$data)
  m <- s$DOSEAU == "mg/m2"
  expect_true(any(m) && all(is.na(b$DOSEA[m])) && all(!is.na(b$DOSEA[!m])))
  expect_false(any(grepl("LSTALVDT", readLines(file.path(e$f, "private", "qc", "qc_report.md")))))
})

test_that("retrieval by record id and by group, in R and from Python-style findings", {
  e <- ext_folder()
  b <- ub_read_dataset(file.path(e$f, "output", "adnca.xpt"))$data
  keys <- ub_load_keys(file.path(e$f, "private", "maps"))
  r <- b[6, ]
  u_orig <- keys$subj_rev$USUBJID_ORIG[match(r$USUBJID, keys$subj_rev_key)]
  fcsv <- file.path(e$f, "findings.csv")
  writeLines(c("DATASET,UBRECID,SUBJID,ATEST,FINDING", paste0("ADNCA,", r$UBRECID, ",,,odd value"),
               paste0(",,", r$SUBJID, ",", r$ATEST, ",flat profile"), ",,S9999,,nothing"), fcsv)
  res <- ub_retrieve_folder(e$f, fcsv, e$opts, quiet = TRUE)
  a <- res$ADNCA
  expect_equal(sum(a$FINDING_NO == 1), 1)
  expect_identical(a$USUBJID[a$FINDING_NO == 1], u_orig)
  src <- ub_read_dataset(file.path(e$f, "input", "adnca.xpt"))$data
  nm_orig <- universalblinder:::ub_exact_map(keys, "analyte_name", "unblind")[[r$ATEST]]
  expect_equal(sum(a$FINDING_NO == 2), sum(src$USUBJID == u_orig & src$ATEST == nm_orig))
  pc <- ub_read_dataset(file.path(e$f, "input", "pc.xpt"))$data
  expect_equal(nrow(res$PC), sum(pc$USUBJID == u_orig & pc$PCTEST == nm_orig))
  expect_true(file.exists(file.path(e$f, "private", "retrieved", "adnca.csv")))
})

test_that("a planned dose per m2 without a unit variable is caught by QC", {
  f <- file.path(tempfile("bsa"))
  dir.create(file.path(f, "input"), recursive = TRUE)
  file.copy(list.files(test_path("fixture_ext", "source"), full.names = TRUE), file.path(f, "input"))
  x <- ub_read_dataset(file.path(f, "input", "adex.xpt"))
  x$data$DOSEP <- round(x$data$DOSEA / 1.8, 1)
  ub_write_dataset(x$data, file.path(f, "input", "adex.xpt"), x$meta, name = "ADEX")
  opts <- list(identities = list(treatment_aliases = list(FICTINIB = c("FCT", "Fct"), SYNTHAZOLE = c("SYZ", "Syz")),
                                 analyte_aliases = list(SYNZM1 = "SZ-M1")),
               rename_vars = list(ADSL = list(FCTFL = "TRTFL01", SYZFL = "TRTFL02"), ADEX = list(DOSEFCT = "DOSET01", DOSESYZ = "DOSET02")))
  first_run(f, opts)
  expect_false(ub_blind_folder(f, opts, quiet = TRUE)$released)
  expect_true(any(grepl("dose_ratio_reveals_bsa", readLines(file.path(f, "private", "qc", "qc_report.md")))))
  opts$drop_vars <- list(ADEX = "DOSEP")
  expect_true(ub_blind_folder(f, opts, quiet = TRUE)$released)
})

test_that("DOSEP is released as a whole number (a dose category); the source keeps its value", {
  f <- file.path(tempfile("dosep"))
  dir.create(file.path(f, "input"), recursive = TRUE)
  file.copy(list.files(test_path("fixture_ext", "source"), full.names = TRUE), file.path(f, "input"))
  x <- ub_read_dataset(file.path(f, "input", "adex.xpt"))
  x$data$DOSEP <- x$data$DOSEA + rep(c(0.4, 0.5, 0), length.out = nrow(x$data))
  ub_write_dataset(x$data, file.path(f, "input", "adex.xpt"), x$meta, name = "ADEX")
  opts <- list(identities = list(treatment_aliases = list(FICTINIB = c("FCT", "Fct"), SYNTHAZOLE = c("SYZ", "Syz")),
                                 analyte_aliases = list(SYNZM1 = "SZ-M1")),
               rename_vars = list(ADSL = list(FCTFL = "TRTFL01", SYZFL = "TRTFL02"), ADEX = list(DOSEFCT = "DOSET01", DOSESYZ = "DOSET02")))
  first_run(f, opts)
  expect_true(ub_blind_folder(f, opts, quiet = TRUE)$released)
  s <- ub_read_dataset(file.path(f, "input", "adex.xpt"))$data
  b <- source_order(file.path(f, "private"), "ADEX", ub_read_dataset(file.path(f, "output", "adex.xpt"))$data)
  ok <- !is.na(s$DOSEP)
  expect_true(any(s$DOSEP[ok] %% 1 != 0))
  expect_equal(as.numeric(b$DOSEP[ok]), floor(s$DOSEP[ok] + 0.5))
})
