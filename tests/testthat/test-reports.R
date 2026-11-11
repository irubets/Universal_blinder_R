write_cb <- function(d) {
  dir.create(file.path(d, "private"), recursive = TRUE, showWarnings = FALSE)
  t <- data.frame(TYPE = c("TREATMENT", "ANALYTE", "TEXT", "ANALYTE"),
                  ACTUAL_VALUE = c("FICTINIB", "SYNZM1", "SYZ", "FCTX"),
                  BLINDED_VALUE = c("TRT01", "ANL01", "?", "ANL01"),
                  NOTE = c("", "", "", ""),
                  CHECK = c("", "", "answer needed", "is this really a spelling of it?"), stringsAsFactors = FALSE)
  utils::write.csv(t, file.path(d, "private", "codebook.csv"), row.names = FALSE)
}

test_that("codebook rows get red/amber/ok and reports are written", {
  d <- tempfile("ub"); dir.create(d); write_cb(d)
  t <- ub_codebook_read(d)
  expect_equal(t$.status, c("ok", "ok", "red", "amber"))
  r <- ub_codebook_reports(d)
  h <- paste(readLines(r$html), collapse = "\n")
  expect_match(h, "tr class=\"red\"")
  expect_match(h, "tr class=\"amber\"")
  expect_match(h, "must fix: 1")
  expect_lt(regexpr("SYZ", h), regexpr("FICTINIB", h))   # attention rows first
  expect_true(file.exists(r$xlsx))
})

test_that("decisions made in the xlsx are imported by key, also after re-sorting", {
  d <- tempfile("ub"); dir.create(d); write_cb(d)
  r <- ub_codebook_reports(d)
  x <- openxlsx::read.xlsx(r$xlsx)
  x <- x[rev(seq_len(nrow(x))), ]
  x$BLINDED_VALUE[x$ACTUAL_VALUE == "SYZ"] <- "TRT02"
  x$CHECK[x$ACTUAL_VALUE == "SYZ"] <- ""
  x$NOTE[x$ACTUAL_VALUE == "FCTX"] <- "confirmed"
  f <- file.path(d, "edited.xlsx")
  openxlsx::write.xlsx(x, f)
  imp <- ub_codebook_import_xlsx(d, f)
  expect_equal(nrow(imp$changes), 3)
  expect_equal(imp$updated$BLINDED_VALUE[imp$updated$ACTUAL_VALUE == "SYZ"], "TRT02")
  expect_equal(imp$updated$.status[imp$updated$ACTUAL_VALUE == "SYZ"], "ok")
  ub_codebook_write(d, imp$updated)
  expect_equal(ub_codebook_read(d)$BLINDED_VALUE[3], "TRT02")
  expect_length(list.files(file.path(d, "private", "codebook_backup")), 1)
})

test_that("prepare_pc report highlights unmatched rows", {
  f <- tempfile(fileext = ".html")
  res <- list(ok = FALSE, written = FALSE, n_lab = 3, n_matched = 2, messages = "1 lab row not matched",
              unmatched = data.frame(lab_row = 3, visit = "C9D9", reason = "visit not in PC", setting_to_change = "visit_map"),
              no_result = NULL, conflicts = NULL, aval_check = NULL)
  ub_prepare_pc_report(res, f)
  h <- paste(readLines(f), collapse = "")
  expect_match(h, "C9D9")
  expect_match(h, "tr class=\"red\"")
})
