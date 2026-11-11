test_that("full batch passes on the synthetic study and releases nothing private", {
  d <- example_study()
  res <- ub_batch_run(file.path(d, "study.json"))
  expect_equal(res$step, c("build_maps", "blind", "qc", "unblind", "verify", "release"))
  expect_true(all(res$status %in% c("DONE", "PASS", "WARN")))
  rel <- file.path(d, "release")
  expect_setequal(list.files(rel), c("RELEASE_MANIFEST.md", "qc_summary.md", "adsl.xpt", "ex.xpt", "ec.xpt", "pc.xpt", "adex.xpt", "adnca.xpt"))
  for (f in list.files(rel, full.names = TRUE)) {
    if (grepl("\\.xpt$", f)) {
      x <- ub_read_dataset(f)
      blob <- paste(c(unlist(lapply(x$data, function(v) unique(as.character(v)))), names(x$data), x$meta$labels, x$meta$file_label), collapse = " ")
    } else blob <- paste(readLines(f), collapse = " ")
    expect_false(grepl(REAL_LOOKING, blob, perl = TRUE), info = basename(f))
  }
  man <- jsonlite::fromJSON(file.path(d, "private", "maps", "maps_manifest.json"))
  expect_false(man$seeded)
})

test_that("review attacks fail on R output", {
  d <- example_study()
  ub_batch_run(file.path(d, "study.json"), steps = c("build_maps", "blind"))
  st <- ub_load_study(file.path(d, "study.json"))
  pc <- ub_read_dataset(ub_blinded_path(st, "PC"))$data
  expect_true(all(tapply(pc$PCLLOQ, pc$PCTESTCD, function(x) length(unique(x[!is.na(x)]))) == 1))
  adsl <- ub_read_dataset(ub_blinded_path(st, "ADSL"))$data
  a <- adsl$AGE[!is.na(adsl$AGE)]
  expect_true(all(abs(a - round(a, 1)) < 1e-9))
  expect_true(all(adsl$BRTHDTC == ""))
})

test_that("guards: unknown treatment, seeded key, constant date", {
  d <- example_study()
  ub_batch_run(file.path(d, "study.json"), steps = "build_maps")
  st <- ub_load_study(file.path(d, "study.json"))
  ex <- ub_read_dataset(ub_source_path(st, "EX"))
  ex$data$EXTRT[1] <- "NEWDRUG"
  ub_write_dataset(ex$data, ub_source_path(st, "EX"), ex$meta, name = "EX")
  r <- ub_batch_run(st, steps = "blind")
  expect_equal(r$status, "ERROR"); expect_match(r$detail, "not in the key")

  d2 <- example_study()
  edit_study(d2, function(s) { s$seed <- 123; s })
  r2 <- ub_batch_run(file.path(d2, "study.json"), steps = c("build_maps", "blind", "qc"))
  expect_equal(tail(r2$status, 1), "FAIL")

  d3 <- example_study()
  edit_study(d3, function(s) { s$config_overrides <- list(date_blinding = list(study_constant_date_vars = list())); s })
  r3 <- ub_batch_run(file.path(d3, "study.json"), steps = c("build_maps", "blind", "qc"))
  expect_equal(tail(r3$status, 1), "FAIL")
  q <- jsonlite::fromJSON(file.path(d3, "private", "qc", "qc_report.json"))
  expect_true("constant_shifted_date" %in% q$findings$check)
})

test_that("stale steps are refused and release needs a clean QC", {
  d <- example_study()
  edit_study(d, function(s) { s$rename_vars <- setNames(list(), character()); s })
  res <- ub_batch_run(file.path(d, "study.json"))
  expect_equal(tail(res$step, 1), "qc"); expect_equal(tail(res$status, 1), "FAIL")
  expect_error(ub_batch_step(file.path(d, "study.json"), "release"))
  ub_batch_step(file.path(d, "study.json"), "build_maps")
  st <- ub_batch_status(file.path(d, "study.json"))
  expect_true(st$stale[st$step == "blind"])
})

test_that("a key built by R is accepted and extended incrementally", {
  d <- example_study()
  ub_batch_run(file.path(d, "study.json"), steps = "build_maps")
  st <- ub_load_study(file.path(d, "study.json"))
  k1 <- ub_load_keys(st$paths$maps_dir)
  ub_batch_run(st, steps = "build_maps")
  k2 <- ub_load_keys(st$paths$maps_dir)
  expect_identical(k1$t$subjects, k2$t$subjects)
  expect_identical(k1$t$subject_factors$FACTOR, k2$t$subject_factors$FACTOR)
})

test_that("BLQ<(x) result text is blinded like BQL<(x) and round-trips", {
  # vendors write 'BLQ<(x)' as well as 'BQL<(x)': the LLOQ inside must be blinded too, the spelling kept
  d <- example_study()
  st <- ub_load_study(file.path(d, "study.json"))
  src <- ub_source_path(st, "PC")
  x <- ub_read_dataset(src); pc <- x$data
  for (c in c("PCORRES", "PCSTRESC")) pc[[c]] <- sub("BQL<", "BLQ<", pc[[c]], fixed = TRUE)
  pc$PCLLOQ <- NA_real_                       # LLOQ only in the text, as in the vendor file
  n_blq <- sum(startsWith(pc$PCORRES, "BLQ<"))
  expect_gt(n_blq, 0)
  ub_write_dataset(pc, src, x$meta, name = "PC")
  res <- ub_batch_run(st)
  expect_true(all(res$status %in% c("DONE", "PASS", "WARN")), info = paste(res$detail, collapse = " "))
  bl <- ub_read_dataset(ub_blinded_path(st, "PC"))$data
  expect_equal(sum(startsWith(bl$PCORRES, "BLQ<")), n_blq)
  expect_false(any(startsWith(bl$PCORRES, "BQL<")))
  # every BLQ text carries the LLOQ x the analyte's LLOQ factor, rounded to the text's decimals (comparing with
  # the real text would be flaky: a factor near 1 can round back to the same text)
  keys <- ub_load_keys(st$paths$maps_dir)
  real <- unique(pc[startsWith(pc$PCORRES, "BLQ<"), c("PCTESTCD", "PCORRES")])
  dec <- universalblinder:::ub_text_decimals(real$PCORRES)
  f <- unname(keys$lloq_factor[real$PCTESTCD])
  expected <- universalblinder:::ub_make_bql_text(
    universalblinder:::ub_round_half_away(universalblinder:::ub_bql_value(real$PCORRES) * f, dec), dec, "BLQ")
  expect_setequal(unique(bl$PCORRES[startsWith(bl$PCORRES, "BLQ<")]), unique(expected))
})

test_that("an analyte name identical to its code (PCTEST = PCTESTCD) is blinded with the code", {
  d <- example_study()
  st <- ub_load_study(file.path(d, "study.json"))
  src <- ub_source_path(st, "PC")
  x <- ub_read_dataset(src); pc <- x$data
  same <- pc$PCTESTCD == "SYNZ"
  pc$PCTEST[same] <- pc$PCTESTCD[same]
  ub_write_dataset(pc, src, x$meta, name = "PC")
  res <- ub_batch_run(st, steps = c("build_maps", "blind"))
  expect_true(all(res$status == "DONE"), info = paste(res$detail, collapse = " "))
  bl <- ub_read_dataset(ub_blinded_path(st, "PC"))$data
  s <- bl$PCTESTCD == unname(universalblinder:::ub_exact_map(ub_load_keys(st$paths$maps_dir), "analyte_code", "blind")["SYNZ"])
  expect_true(any(s))
  expect_equal(bl$PCTEST[s], bl$PCTESTCD[s])
})
