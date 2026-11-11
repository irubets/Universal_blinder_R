# ub_publish_check(): pre-publication scan for private values from key (maps) folders.

pub_maps <- function() {
  d <- tempfile("pubmaps"); dir.create(d)
  utils::write.csv(data.frame(TYPE = c("TREATMENT", "TREATMENT", "ANALYTE"), ROLE = c("NAME", "ALIAS", "CODE"),
                              IDENTITY_KEY = c("ZORBLATIDE", "ZORBLATIDE", "ZRB"), SEQ = c("1", "1", "1"),
                              ORIG_TEXT = c("Zorblatide", "ZB", "ZRB7"), BLIND_TEXT = c("TRT01", "TRT01", "ANL01")),
                   file.path(d, "replace_terms.csv"), row.names = FALSE)
  utils::write.csv(data.frame(STUDYID_ORIG = "QXT-4417", USUBJID_ORIG = "QXT-4417-310-022", SUBJID_ORIG = "310-022",
                              STUDYID_BLIND = "STUDY-XXX", USUBJID_BLIND = "STUDY-XXX-S0001", SUBJID_BLIND = "S0001"),
                   file.path(d, "replace_subjid.csv"), row.names = FALSE)
  utils::write.csv(data.frame(DATASET = "ADSL", VAR_ORIG = "ZORFL", VAR_BLIND = "TRTFL01"),
                   file.path(d, "rename_vars.csv"), row.names = FALSE)
  writeLines('{"schema_version": 2}', file.path(d, "maps_manifest.json"))
  d
}

pub_repo <- function() {
  r <- tempfile("pubrepo"); dir.create(file.path(r, "R"), recursive = TRUE)
  writeLines(c("# Example", "Blinds a made-up study.", "Codes TRT01 and ANL01."), file.path(r, "README.md"))
  writeLines("f <- function(x) x + 1", file.path(r, "R", "f.R"))
  r
}

test_that(".ub_private_terms reads the R key format and extra terms", {
  m <- pub_maps()
  extra <- tempfile(fileext = ".txt"); writeLines(c("Quorvanta", "ab", ""), extra)
  t <- .ub_private_terms(m, extra)
  expect_named(t, c("kind", "term"))
  expect_setequal(t$term, c("Zorblatide", "ZRB7", "QXT-4417", "QXT-4417-310-022", "310-022", "ZORFL", "Quorvanta"))
  expect_equal(t$kind[t$term == "Zorblatide"], "treatment")
  expect_equal(t$kind[t$term == "310-022"], "subjid")
  expect_equal(t$kind[t$term == "Quorvanta"], "extra")
  expect_equal(nchar(t$term[1]), max(nchar(t$term)))      # longest first
  expect_equal(nrow(.ub_private_terms(character())), 0)
})

test_that("a clean folder passes; a planted term is found with its line", {
  m <- pub_maps(); r <- pub_repo()
  expect_equal(nrow(ub_publish_check(r, m, quiet = TRUE)), 0)
  expect_output(ub_publish_check(r, m), "clean")
  writeLines(c("x", "y", "This mentions zorblatide in passing.", "Zorblatides is another word."), file.path(r, "notes.md"))
  h <- ub_publish_check(r, m, quiet = TRUE)
  expect_equal(nrow(h), 1)
  expect_equal(h$file, "notes.md"); expect_equal(h$line, 3L); expect_equal(h$kind, "treatment")
  expect_equal(h$term, "Zorblatide")
  out <- capture.output(ub_publish_check(r, m))
  expect_false(any(grepl("orblatide", out, ignore.case = TRUE)))       # the summary never prints the term
})

test_that("key files, logs and data files are flagged; SYNTHETIC data is scanned", {
  m <- pub_maps(); r <- pub_repo()
  file.copy(file.path(m, "replace_subjid.csv"), file.path(r, "replace_subjid.csv"))
  writeLines("run ok", file.path(r, "run.log"))
  haven::write_xpt(data.frame(USUBJID = "S0001"), file.path(r, "real.xpt"), version = 5)
  haven::write_xpt(data.frame(USUBJID = "QXT-4417-310-022"), file.path(r, "syn.xpt"), version = 5, label = "SYNTHETIC test")
  haven::write_xpt(data.frame(USUBJID = "S0002"), file.path(r, "syn2.xpt"), version = 5, label = "SYNTHETIC ok")
  h <- ub_publish_check(r, m, quiet = TRUE)
  expect_true(any(h$file == "replace_subjid.csv" & h$kind == "key_file"))
  expect_true(any(h$file == "run.log" & h$kind == "log_file"))
  expect_true(any(h$file == "real.xpt" & h$kind == "unlabelled_data"))
  expect_true(any(h$file == "syn.xpt" & h$kind == "usubjid"))
  expect_false(any(h$file == "syn2.xpt"))
})

test_that("extra terms are matched case-insensitively on word boundaries", {
  m <- pub_maps(); r <- pub_repo()
  writeLines("see QUORVANTA-like and quorvanta.", file.path(r, "a.txt"))
  h <- ub_publish_check(r, m, extra_terms = c("Quorvanta"), quiet = TRUE)
  expect_equal(nrow(h), 1); expect_equal(h$kind, "extra")
})
