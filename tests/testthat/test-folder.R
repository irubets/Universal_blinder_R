RENAMES <- list(rename_vars = list(ADSL = list(FCTFL = "TRTFL01", SYZFL = "TRTFL02"),
                                   ADEX = list(DOSEFCT = "DOSET01", DOSESYZ = "DOSET02")))

test_that("ub_blind_folder releases blinded files under the same names", {
  src <- system.file("examples", "synthetic", "source", package = "universalblinder")
  skip_if(!nzchar(src))
  f <- file.path(tempfile("study"))
  dir.create(file.path(f, "input"), recursive = TRUE)
  file.copy(list.files(src, pattern = "\\.xpt$", full.names = TRUE), file.path(f, "input"))
  first <- ub_blind_folder(f, RENAMES, quiet = TRUE)                              # stops once: new rows in the codebook
  expect_false(first$released)
  cb <- utils::read.csv(file.path(f, "private", "codebook.csv"), colClasses = "character")
  code <- function(tp, a) cb$BLINDED_VALUE[cb$TYPE == tp & cb$ACTUAL_VALUE == a]
  expect_setequal(c(code("TREATMENT", "FICTINIB"), code("TREATMENT", "SYNTHAZOLE")), c("TRT01", "TRT02"))
  expect_equal(code("TREATMENT", "Fct"), code("TREATMENT", "FICTINIB"))
  expect_equal(substring(code("ANALYTE", "FICT"), 4), substring(code("TREATMENT", "FICTINIB"), 4))
  res <- ub_blind_folder(f, RENAMES, quiet = TRUE, log_file = file.path(f, "blind.log"))   # the codebook as written
  expect_true(res$released)
  expect_equal(res$qc, "WARN")
  expect_setequal(list.files(file.path(f, "output")),
                  c(list.files(file.path(f, "input")), "RELEASE_MANIFEST.md", "qc_summary.md"))
  expect_true(any(grepl("FINISHED -- QC WARN", readLines(file.path(f, "blind.log")))))
  key1 <- readLines(file.path(f, "private", "maps", "replace_subjid.csv"))
  expect_true(ub_blind_folder(f, RENAMES, quiet = TRUE)$released)            # re-run reuses the key
  expect_identical(readLines(file.path(f, "private", "maps", "replace_subjid.csv")), key1)
})

test_that("require_codebook_review (blind.R): the first run is allowed, later runs need a confirmed review", {
  src <- system.file("examples", "synthetic", "source", package = "universalblinder")
  skip_if(!nzchar(src))
  f <- file.path(tempfile("study"))
  dir.create(file.path(f, "input"), recursive = TRUE)
  file.copy(list.files(src, pattern = "\\.xpt$", full.names = TRUE), file.path(f, "input"))
  first <- ub_blind_folder(f, RENAMES, quiet = TRUE, require_codebook_review = TRUE)
  expect_false(first$released)
  expect_true(file.exists(file.path(f, "private", "codebook.csv")))   # the usual first stop, not the review check
  cb <- ub_codebook_read(f); cb$CHECK <- ""; ub_codebook_write(f, cb)
  r <- ub_blind_folder(f, RENAMES, quiet = TRUE, require_codebook_review = TRUE)
  expect_false(r$released)
  expect_match(r$error, "codebook review is not confirmed")
  expect_match(r$error, "ub_codebook_confirm_review")
  expect_false(dir.exists(file.path(f, "output")) && length(list.files(file.path(f, "output"))) > 0)
  expect_true(any(grepl("NOTHING WAS RELEASED", readLines(file.path(f, "private", "logs", "blind.log")))))
  ub_codebook_confirm_review(f, "Script User")
  expect_true(ub_blind_folder(f, RENAMES, quiet = TRUE, require_codebook_review = TRUE)$released)
  cb <- ub_codebook_read(f); cb$BLINDED_VALUE[1] <- paste0(cb$BLINDED_VALUE[1], "X"); ub_codebook_write(f, cb)
  r <- ub_blind_folder(f, RENAMES, quiet = TRUE, require_codebook_review = TRUE)
  expect_false(r$released)
  expect_match(r$error, "codes changed after the review confirmed by Script User")
  expect_true(length(list.files(file.path(f, "output"))) > 0)        # refused before anything: the last release stays
  # blind.R (the template copied to every study folder) asks for it
  tpl <- readLines(system.file("template_folder", "blind.R", package = "universalblinder"))
  expect_true(any(grepl("require_codebook_review = TRUE", tpl, fixed = TRUE)))
})

test_that("ub_blind_folder refuses unknown names and empty folders, and clears stale output", {
  src <- system.file("examples", "synthetic", "source", package = "universalblinder")
  skip_if(!nzchar(src))
  f <- file.path(tempfile("study"))
  dir.create(file.path(f, "input"), recursive = TRUE)
  dir.create(file.path(f, "output"))
  file.copy(file.path(src, c("adnca.xpt", "adex.xpt")), file.path(f, "input"))
  file.copy(file.path(src, "adnca.xpt"), file.path(f, "input", "adxx.xpt"))
  writeLines("stale", file.path(f, "output", "old.xpt"))
  res <- ub_blind_folder(f, quiet = TRUE)
  expect_false(res$released)
  expect_match(res$error, "ADXX")
  expect_false(file.exists(file.path(f, "output", "old.xpt")))
  expect_false(file.exists(file.path(f, "private", "maps", "maps_manifest.json")))
  ub_blind_folder(f, c(list(dataset_aliases = list(ADXX = "ADNCA")), RENAMES), quiet = TRUE)                 # writes the codebook
  expect_true(ub_blind_folder(f, c(list(dataset_aliases = list(ADXX = "ADNCA")), RENAMES), quiet = TRUE)$released)
  e <- tempfile("empty")
  res <- ub_blind_folder(e, quiet = TRUE)
  expect_false(res$released)
  expect_match(res$error, "no .xpt")
})

syn_folder <- function() {
  src <- system.file("examples", "synthetic", "source", package = "universalblinder")
  f <- file.path(tempfile("study"))
  dir.create(file.path(f, "input"), recursive = TRUE)
  file.copy(list.files(src, pattern = "\\.xpt$", full.names = TRUE), file.path(f, "input"))
  f
}

test_that("PC without results and a results file in input: stop, or continue by choice", {
  f <- syn_folder()
  expect_equal(ub_check_pc_results(f)$status, "ok")
  expect_true(ub_check_pc_results(f)$has_results)
  p <- file.path(f, "input", "pc.xpt")
  x <- ub_read_dataset(p)
  x$data$PCORRES <- ""; x$data$PCSTRESC <- ""; x$data$PCSTRESN <- NA_real_
  ub_write_dataset(x$data, p, x$meta, name = "PC")
  writeLines("SUBJECT,VISIT,CONC", file.path(f, "input", "lab_results.csv"))
  chk <- ub_check_pc_results(f)
  expect_equal(chk[c("has_pc", "has_results", "lab_files", "merged", "status")],
               list(has_pc = TRUE, has_results = FALSE, lab_files = "lab_results.csv", merged = FALSE,
                    status = "lab_file_not_merged"))
  expect_match(chk$message, "lab_results.csv")
  res <- ub_blind_folder(f, RENAMES, quiet = TRUE)
  expect_false(res$released)
  expect_match(res$error, "Prepare PC", fixed = TRUE)
  expect_match(res$error, "lab_results.csv", fixed = TRUE)
  expect_match(res$error, "continue without PC results", fixed = TRUE)
  expect_false(grepl("prepare_pc|python", res$error, ignore.case = TRUE))
  expect_false(file.exists(file.path(f, "private", "maps", "maps_manifest.json")))   # stopped before anything was built
  # the user chooses to go on: the choice is logged, the results file is never treated as a dataset
  res <- ub_blind_folder(f, RENAMES, quiet = TRUE, steps = "build_maps", continue_without_results = TRUE)
  lg <- readLines(file.path(f, "private", "logs", "blind.log"))
  expect_true("Continuing without PC results (user choice)" %in% lg)
  expect_true(file.exists(file.path(f, "private", "codebook.csv")))
  expect_false(any(grepl("lab_results", lg[grepl("^datasets to blind", lg)])))
  # without a results file it is only a warning
  file.remove(file.path(f, "input", "lab_results.csv"))
  expect_equal(ub_check_pc_results(f)$status, "no_results")
  ub_blind_folder(f, RENAMES, quiet = TRUE, steps = "build_maps")
  lg <- readLines(file.path(f, "private", "logs", "blind.log"))
  expect_true(any(startsWith(lg, "WARNING: pc.xpt has no concentration results")))
  expect_false("Continuing without PC results (user choice)" %in% lg)
  # Prepare PC has run (the delivered PC is kept in private/source_original)
  dir.create(file.path(f, "private", "source_original"), recursive = TRUE)
  file.copy(p, file.path(f, "private", "source_original", "pc.xpt"))
  expect_true(ub_check_pc_results(f)$merged)
  # no PC at all
  file.remove(p)
  expect_equal(ub_check_pc_results(f)[c("has_pc", "status")], list(has_pc = FALSE, status = "no_pc"))
})

test_that("the log goes to private/logs/blind.log by default; on_progress sees every step", {
  f <- syn_folder()
  calls <- list()
  r <- ub_blind_folder(f, RENAMES, quiet = TRUE, steps = "build_maps",
                       on_progress = function(step, msg) calls[[length(calls) + 1L]] <<- c(step, msg))
  lg <- file.path(f, "private", "logs", "blind.log")
  expect_true(file.exists(lg))
  expect_true(any(grepl("Universal Blinder -- study folder", readLines(lg))))
  expect_equal(calls, list(c("build_maps", "started"), c("build_maps", "ERROR")))   # stops once for the codebook
  calls <- list()
  r <- ub_blind_folder(f, RENAMES, quiet = TRUE,
                       on_progress = function(step, msg) calls[[length(calls) + 1L]] <<- c(step, msg))
  expect_true(r$released)
  expect_equal(vapply(calls, `[`, "", 1), rep(c("build_maps", "blind", "qc", "unblind", "verify", "release"), each = 2))
  expect_equal(vapply(calls, `[`, "", 2)[c(TRUE, FALSE)], rep("started", 6))
})
