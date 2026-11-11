# the codebook: generated codes, review stop, special values, user edits kept and applied (mirrors test_codebook.py)
RN <- list(rename_vars = list(ADSL = list(FCTFL = "TRTFL01", SYZFL = "TRTFL02"),
                              ADEX = list(DOSEFCT = "DOSET01", DOSESYZ = "DOSET02")))
cb_folder <- function() {
  f <- file.path(tempfile("cb"))
  dir.create(file.path(f, "input"), recursive = TRUE)
  file.copy(list.files(test_path("fixture_ext", "source"), full.names = TRUE), file.path(f, "input"))
  f
}
rd <- function(p) utils::read.csv(p, colClasses = "character", na.strings = character())
wr <- function(p, x) utils::write.csv(x, p, row.names = FALSE)
cd <- function(x, tp, a) x$BLINDED_VALUE[x$TYPE == tp & x$ACTUAL_VALUE == a]
bl <- function(f, n) ub_read_dataset(file.path(f, "private", "blinded", paste0(n, ".xpt")))$data

test_that("codebook flow: codes, '?' stop, '-' kept, blank filled, applied", {
  f <- cb_folder(); tab <- file.path(f, "private", "codebook.csv")
  r <- ub_blind_folder(f, RN, quiet = TRUE)
  expect_false(r$released); expect_true(file.exists(tab))
  x <- rd(tab)
  expect_setequal(c(cd(x, "TREATMENT", "FICTINIB"), cd(x, "TREATMENT", "SYNTHAZOLE")), c("TRT01", "TRT02"))
  expect_equal(substring(cd(x, "ANALYTE", "FICT"), 4), substring(cd(x, "TREATMENT", "FICTINIB"), 4))
  expect_equal(cd(x, "ANALYTE", "Fictinib"), cd(x, "ANALYTE", "FICT"))
  expect_setequal(c(cd(x, "ANALYTE", "SYNZM1"), cd(x, "ANALYTE", "SMARK")), c("ANL03", "ANL04"))
  expect_equal(cd(x, "ARMCD", "XGA"), paste0("ARMCD", substring(cd(x, "ARM", "Crossover Group A"), 4)))
  expect_setequal(x$BLINDED_VALUE[x$TYPE == "REGIMEN"], sprintf("REGIMEN%02d", 1:4))
  expect_equal(cd(x, "TREATMENT", "Fct"), cd(x, "TREATMENT", "FICTINIB"))
  expect_equal(cd(x, "ANALYTE", "SZ-M1"), "?")
  r <- ub_blind_folder(f, RN, quiet = TRUE)
  expect_false(r$released); expect_match(paste(r$results$detail, collapse = " "), "'SZ-M1' -- give its code", fixed = TRUE)
  x$BLINDED_VALUE[x$ACTUAL_VALUE == "SZ-M1"] <- cd(x, "ANALYTE", "SYNZM1")
  x$BLINDED_VALUE[x$ACTUAL_VALUE == "Syz"] <- "-"; x$NOTE[x$ACTUAL_VALUE == "Syz"] <- "my own note"
  x[nrow(x) + 1, ] <- c("TEXT", "ORDER A", "", "", "")
  wr(tab, x)
  r <- ub_blind_folder(f, RN, quiet = TRUE)
  expect_false(r$released); expect_match(paste(r$results$detail, collapse = " "), "filled in")
  expect_equal(cd(rd(tab), "TEXT", "ORDER A"), "TERM01")
  r <- ub_blind_folder(f, RN, quiet = TRUE)
  expect_true(r$released, info = paste(r$error, collapse = " "))
  y <- rd(tab)
  expect_equal(y$BLINDED_VALUE[y$ACTUAL_VALUE == "Syz"], "-"); expect_equal(y$NOTE[y$ACTUAL_VALUE == "Syz"], "my own note")
  adsl <- bl(f, "adsl"); adnca <- bl(f, "adnca"); ex <- bl(f, "ex")
  expect_setequal(unique(adsl$ARM), c(cd(y, "ARM", "Crossover Group A"), cd(y, "ARM", "Crossover Group B")))
  expect_true(all(adsl$TRT01P %in% y$BLINDED_VALUE[y$TYPE == "REGIMEN"]))
  expect_setequal(unique(ex$EXTRT), c("TRT01", "TRT02"))
  expect_true(paste("SC", cd(y, "TREATMENT", "FICTINIB")) %in% adnca$TREAT && "Oral Syz" %in% adnca$TREAT)
  expect_true("TERM01" %in% adsl$SEQUENCE && !any(grepl("ORDER A", adsl$SEQUENCE)))
})

test_that("codes can be changed; an unknown code is reported", {
  f <- cb_folder(); tab <- file.path(f, "private", "codebook.csv")
  ub_blind_folder(f, RN, quiet = TRUE)
  x <- rd(tab)
  x$BLINDED_VALUE[x$ACTUAL_VALUE == "SZ-M1"] <- "ANL99"
  wr(tab, x)
  r <- ub_blind_folder(f, RN, quiet = TRUE)
  expect_false(r$released); expect_match(paste(r$results$detail, collapse = " "), "'ANL99' is not the code of an analyte", fixed = TRUE)
  old <- cd(x, "TREATMENT", "FICTINIB")
  x$BLINDED_VALUE[x$BLINDED_VALUE == old] <- "TRT07"
  x$BLINDED_VALUE[x$ACTUAL_VALUE == "SZ-M1"] <- cd(x, "ANALYTE", "SYNZM1")
  x$BLINDED_VALUE[x$TYPE == "ARM" & x$ACTUAL_VALUE == "Crossover Group A"] <- "-"
  wr(tab, x)
  expect_true(ub_blind_folder(f, RN, quiet = TRUE)$released)
  expect_true("TRT07" %in% bl(f, "ex")$EXTRT && "SC TRT07" %in% bl(f, "adnca")$TREAT)
  expect_true("Crossover Group A" %in% bl(f, "adsl")$ARM)
})

test_that("CHECK marks the rows a reviewer must decide on", {
  f <- cb_folder(); tab <- file.path(f, "private", "codebook.csv")
  ub_blind_folder(f, RN, quiet = TRUE, steps = "build_maps")
  x <- rd(tab)
  expect_equal(names(x), c("TYPE", "ACTUAL_VALUE", "BLINDED_VALUE", "NOTE", "CHECK"))
  expect_match(x$CHECK[x$ACTUAL_VALUE == "SZ-M1"], "^answer needed")
  expect_equal(x$CHECK[x$ACTUAL_VALUE == "Fct"], paste0("is this really a spelling of it? Keep ", cd(x, "TREATMENT", "FICTINIB"), ", or set '-'"))
  expect_equal(x$CHECK[x$TYPE == "TREATMENT" & x$ACTUAL_VALUE == "FICTINIB"], "")
  # a codebook written before CHECK existed (4 columns) is still read, and its rows are kept
  x$BLINDED_VALUE[x$ACTUAL_VALUE == "SZ-M1"] <- cd(x, "ANALYTE", "SYNZM1")
  wr(tab, x[, 1:4])
  r <- ub_blind_folder(f, RN, quiet = TRUE, steps = "build_maps")
  expect_equal(r$results$status, "DONE", info = paste(r$results$detail, collapse = " "))
})

# a synthetic study folder whose ADNCA has PARAMCD but no ATEST, and a PARAMCD value not used anywhere else
nameless_folder <- function(partner = TRUE, param = TRUE) {
  src <- system.file("examples", "synthetic", "source", package = "universalblinder")
  f <- file.path(tempfile("nl"))
  dir.create(file.path(f, "input"), recursive = TRUE)
  file.copy(list.files(src, pattern = "\\.xpt$", full.names = TRUE), file.path(f, "input"))
  p <- file.path(f, "input", "adnca.xpt")
  x <- ub_read_dataset(p); a <- x$data
  if (partner) a$PCTESTCD <- a$PARAMCD
  a$PARAMCD[a$PARAMCD == "SYNZ"] <- "SYNZP"
  drop <- c("ATEST", if (!param) "PARAM")
  a <- a[, setdiff(names(a), drop)]
  x$meta$labels <- x$meta$labels[setdiff(names(x$meta$labels), drop)]
  x$meta$formats <- x$meta$formats[setdiff(names(x$meta$formats), drop)]
  ub_write_dataset(a, p, x$meta, name = "ADNCA")
  f
}
NL_OPTS <- c(RN, list(config_overrides = list(datasets = list(ADNCA = list(exact_vars = list(PCTESTCD = "analyte_code"))))))

test_that("an analyte code without a name gets the code of its analyte, flagged in CHECK", {
  # the same records also carry PCTESTCD: SYNZP is probably another code of SYNZ
  f <- nameless_folder(partner = TRUE); tab <- file.path(f, "private", "codebook.csv")
  first_run(f, NL_OPTS)
  x <- rd(tab)
  expect_equal(cd(x, "ANALYTE", "SYNZP"), cd(x, "ANALYTE", "SYNZ"))
  expect_match(x$CHECK[x$ACTUAL_VALUE == "SYNZP"], "^same analyte as SYNZ \\(on the same records in ADNCA.PCTESTCD\\)\\? Keep ANL")
  expect_match(x$NOTE[x$ACTUAL_VALUE == "SYNZP"], "analyte code without a name (ADNCA.PARAMCD); given the code of SYNZ", fixed = TRUE)
  # a drug that is also an analyte legitimately has a TRT code and an ANL code
  expect_match(cd(x, "TREATMENT", "SYNTHAZOLE"), "^TRT")
  expect_match(cd(x, "ANALYTE", "SYNZ"), "^ANL")
  r <- ub_blind_folder(f, NL_OPTS, quiet = TRUE, steps = "build_maps")      # accepted as it is
  expect_equal(r$results$status, "DONE", info = paste(r$results$detail, collapse = " "))
  expect_true(any(grepl("is shared by 2 analytes", readLines(r$results$log))))
  # only PARAM names the analyte
  f <- nameless_folder(partner = FALSE); tab <- file.path(f, "private", "codebook.csv")
  first_run(f, RN)
  x <- rd(tab)
  expect_equal(cd(x, "ANALYTE", "SYNZP"), cd(x, "ANALYTE", "SYNZ"))
  expect_match(x$CHECK[x$ACTUAL_VALUE == "SYNZP"], "^same analyte as Synthazole \\(named in ADNCA.PARAM '")
  # nothing says which analyte it is: a new code, and the reviewer is asked
  f <- nameless_folder(partner = FALSE, param = FALSE); tab <- file.path(f, "private", "codebook.csv")
  first_run(f, RN)
  x <- rd(tab)
  k <- cd(x, "ANALYTE", "SYNZP")
  expect_false(k %in% x$BLINDED_VALUE[x$ACTUAL_VALUE != "SYNZP"])
  expect_match(x$CHECK[x$ACTUAL_VALUE == "SYNZP"], "^analyte code without a name: a separate analyte, or another code of one of ")
})

test_that("a stale analyte row (e.g. a placeholder replaced by Prepare PC) stops build_maps and names its line", {
  f <- cb_folder(); tab <- file.path(f, "private", "codebook.csv")
  first_run(f, RN)
  x <- rd(tab)
  x$BLINDED_VALUE[x$ACTUAL_VALUE == "SZ-M1"] <- cd(x, "ANALYTE", "SYNZM1")
  x[nrow(x) + 1, ] <- c("ANALYTE", "ANALYTE", "ANL09", "analyte code (PC.PCTESTCD)", "")
  x[nrow(x) + 1, ] <- c("ANALYTE", "Analyte", "ANL09", "analyte name of ANALYTE (PC.PCTEST)", "")
  wr(tab, x)
  r <- ub_blind_folder(f, RN, quiet = TRUE, steps = "build_maps")
  d <- paste(r$results$detail, collapse = " ")
  expect_equal(r$results$status, "ERROR")
  expect_match(d, paste0("line ", nrow(x), ": ANALYTE 'ANALYTE' -- 'ANL09' is not the code of an analyte"), fixed = TRUE)
  expect_match(d, "'Analyte' no longer occurs in the data", fixed = TRUE)
  expect_match(d, "delete this row from the codebook", fixed = TRUE)
  wr(tab, x[seq_len(nrow(x) - 2), ])
  expect_equal(ub_blind_folder(f, RN, quiet = TRUE, steps = "build_maps")$results$status, "DONE")
})
