# Workflow 3: the lab results file is released blinded next to PC (it cannot be merged into PC).

test_that("Check matching counts the rows and writes nothing", {
  s <- make_lab_study()
  p <- ub_lab_release_prepare(s$ub, write = FALSE)
  n <- nrow(s$lab)
  expect_equal(p$counts[["left_out"]], 1)
  expect_match(p$left_out$reason, "not in PC")
  expect_equal(p$left_out$lab_row, n + 1L)                                # the last row (header = row 1)
  expect_equal(p$counts[["rows"]], n - 1)
  expect_equal(p$counts[["no_pc"]], 1)                                    # the 26 h sample
  expect_equal(sum(p$labpc$MATCHST == "matched"), p$counts[["matched"]])
  expect_equal(p$counts[["matched"]], n - 2)                              # every row but the 26 h one and the stranger
  first <- p$labpc[p$labpc$LABROW == 2L, ]; rep <- p$labpc[p$labpc$LABROW == n, ]
  expect_equal(first$PCSRCROW, rep$PCSRCROW)                              # the repeat points to the same PC record
  expect_equal(first$MATCHST, "matched"); expect_equal(rep$MATCHST, "matched")
  expect_false(dir.exists(file.path(s$ub, "private", "lab")))
})

test_that("LABPC carries PC's codes, ISO dates and short names", {
  s <- make_lab_study()
  p <- ub_lab_release_prepare(s$ub, write = FALSE)
  L <- p$labpc
  expect_true(all(nchar(names(L)) <= 8))
  expect_setequal(unique(L$PCTESTCD), unique(s$pc_full$PCTESTCD))
  expect_true(all(L$PCTEST == s$pc_full$PCTEST[match(L$PCTESTCD, s$pc_full$PCTESTCD)]))
  expect_true(all(L$USUBJID %in% s$pc_full$USUBJID))                     # partial IDs resolved
  expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", L$K01DTC)))
  expect_equal(L$K01DTC[1], format(as.Date(substr(s$pc_full$PCDTC[1], 1, 10)) + 3))
  expect_equal(L$K02[L$LABROW == 51L], "reanalysis of FICTINIB sample")
  expect_equal(p$layout$columns$lab_name, names(s$lab))                  # the lab's order
  expect_equal(p$layout$columns$labpc_name[p$layout$columns$lab_name == "RUN_DATE"], "K01DTC")
  expect_equal(L$LVISIT[1], "P1D1"); expect_equal(L$VISIT[1], "PERIOD 1 DAY 1")
  expect_equal(L$PCORRES[1], s$lab$CONCENTRATION[1])
  bql <- grepl("^BQL", L$PCORRES); expect_true(any(bql)); expect_true(all(is.na(L$PCSTRESN[bql])))
  expect_false(p$layout$analyte_is_name)
})

test_that("specimen matches whatever its case and spaces", {
  a <- ub_lab_release_prepare(make_lab_study()$ub, write = FALSE)
  b <- ub_lab_release_prepare(make_lab_study(function(l) { l$SPECIMEN <- " PLASMA "; l })$ub, write = FALSE)
  expect_equal(b$counts[["matched"]], a$counts[["matched"]])
  c <- ub_lab_release_prepare(make_lab_study(function(l) { l$SPECIMEN <- "Urine"; l })$ub, write = FALSE)
  expect_equal(c$counts[["matched"]], 0)
})

test_that("an Excel lab file with a sheet is read the same", {
  skip_if_not_installed("openxlsx")
  s <- make_lab_study()
  openxlsx::write.xlsx(list(Notes = data.frame(x = 1), Results = s$lab), file.path(s$ub, "input", "lab_results.xlsx"))
  o <- ub_read_options(s$ub); o$prepare_pc$results_file <- "lab_results.xlsx"; o$prepare_pc$sheet <- "Results"
  p <- ub_lab_release_prepare(s$ub, o, write = FALSE)
  q <- ub_lab_release_prepare(s$ub, write = FALSE)
  expect_equal(p$counts, q$counts)
})

test_that("settings errors stop before anything is written", {
  s <- make_lab_study()
  o <- ub_read_options(s$ub)
  bad <- function(f) { x <- o; f(x) }
  expect_error(ub_lab_release_prepare(s$ub, bad(function(x) { x$prepare_pc$results_file <- ""; x })), "results_file")
  expect_error(ub_lab_release_prepare(s$ub, bad(function(x) { x$prepare_pc$columns$result <- ""; x })), "result")
  expect_error(ub_lab_release_prepare(s$ub, bad(function(x) { x$lab_release$keep_columns <- list("NOPE"); x })), "not a column")
  expect_error(ub_lab_release_prepare(s$ub, bad(function(x) { x$lab_release$keep_columns <- list("CONCENTRATION"); x })),
               "already a role")
  expect_error(ub_lab_release_prepare(s$ub, bad(function(x) { x$prepare_pc$results_file <- "nothere.csv"; x })), "not found")
  expect_false(dir.exists(file.path(s$ub, "private", "lab")))
})

test_that("write = TRUE writes LABPC, the layout and the report", {
  s <- make_lab_study()
  p <- ub_lab_release_prepare(s$ub, write = TRUE)
  expect_true(file.exists(file.path(s$ub, "private", "lab", "labpc.xpt")))
  expect_true(file.exists(file.path(s$ub, "private", "lab", "labpc_layout.json")))
  rep <- file.path(s$ub, "private", "reports", "lab_release_report.csv")
  expect_true(file.exists(rep)); expect_true(file.exists(sub("csv$", "html", rep)))
  expect_true(any(grepl("999-999", readLines(rep))))
  back <- ub_read_dataset(file.path(s$ub, "private", "lab", "labpc.xpt"))$data
  expect_equal(nrow(back), nrow(p$labpc)); expect_true(is.numeric(back$PCSTRESN))
  lay <- jsonlite::fromJSON(file.path(s$ub, "private", "lab", "labpc_layout.json"))
  expect_equal(lay$lab_md5, unname(tools::md5sum(file.path(s$ub, "input", "lab_results.csv"))))
})

# ---- Task 4: LABPC in the run -------------------------------------------------------------------------------------
lab_run <- function(s, steps = NULL) {
  opts <- ub_options_for_engine(ub_read_options(s$ub))
  first <- ub_blind_folder(s$ub, opts, quiet = TRUE)                              # stops once for the codebook
  if (isTRUE(first$released)) return(first)
  cb <- ub_codebook_read(s$ub); cb$CHECK <- ""; cb$BLINDED_VALUE[cb$BLINDED_VALUE == "?"] <- "-"; ub_codebook_write(s$ub, cb)
  ub_blind_folder(s$ub, opts, quiet = TRUE, steps = steps)
}

test_that("the run blinds LABPC with PC's factor and adds nothing to the key", {
  s <- make_lab_study()
  r <- lab_run(s)
  expect_true(r$released)
  bl <- ub_read_dataset(file.path(s$ub, "private", "blinded", "labpc.xpt"))$data
  src <- ub_read_dataset(file.path(s$ub, "private", "lab", "labpc.xpt"))$data
  keys <- ub_load_keys(file.path(s$ub, "private", "maps"))
  expect_false(any(endsWith(as.character(keys$subj_fwd_key), "999-999")))        # the stranger was left out
  expect_setequal(unique(src$USUBJID), intersect(unique(src$USUBJID), keys$subj_fwd_key))
  o <- src[match(bl$LABROW, src$LABROW), ]
  num <- !is.na(o$PCSTRESN) & o$MATCHST == "matched"
  expect_true(sum(num) > 100)
  f <- keys$analyte_factor[paste(o$USUBJID[num], o$PCTESTCD[num], sep = "\r")]
  expect_false(anyNA(f))
  d <- vapply(o$PCORRES[num], function(t) { p <- strsplit(t, ".", fixed = TRUE)[[1]]; if (length(p) == 2) nchar(p[2]) else 0L }, 1L)
  expect_equal(unname(bl$PCSTRESN[num]), unname(ub_round_half_away(o$PCSTRESN[num] * f, d)))
  expect_false(any(bl$USUBJID %in% src$USUBJID))                                  # subjects blinded
  expect_true(all(grepl("^ANL", bl$PCTESTCD)))
  expect_equal(bl$LVISIT, o$LVISIT); expect_equal(bl$MATCHST, o$MATCHST)          # the lab's own keys unchanged
})

test_that("LABPC adds no subject and no codebook row", {
  on <- make_lab_study(); off <- make_lab_study()
  x <- ub_read_options(off$ub); x$lab_release$enabled <- FALSE; ub_write_options(off$ub, x, backup = FALSE)
  ub_blind_folder(on$ub, ub_options_for_engine(ub_read_options(on$ub)), quiet = TRUE, steps = "build_maps")
  ub_blind_folder(off$ub, ub_options_for_engine(ub_read_options(off$ub)), quiet = TRUE, steps = "build_maps",
                  continue_without_results = TRUE)
  n <- function(ub, f) nrow(utils::read.csv(file.path(ub, "private", f)))
  expect_equal(n(on$ub, "codebook.csv"), n(off$ub, "codebook.csv"))           # the first run stops here, before the key
})

test_that("kept columns are blinded: the date shifted, the drug name coded", {
  s <- make_lab_study()
  lab_run(s)
  bl <- ub_read_dataset(file.path(s$ub, "private", "blinded", "labpc.xpt"))$data
  src <- ub_read_dataset(file.path(s$ub, "private", "lab", "labpc.xpt"))$data
  keys <- ub_load_keys(file.path(s$ub, "private", "maps"))
  o <- src[match(bl$LABROW, src$LABROW), ]
  sh <- keys$shift[o$USUBJID]
  expect_equal(as.Date(bl$K01DTC), as.Date(o$K01DTC) + as.integer(sh))
  expect_false(any(grepl("FICTINIB", bl$K02, ignore.case = TRUE)))
  expect_true(any(grepl("^reanalysis of TRT\\d+", bl$K02)))
})

test_that("odd result texts pass through", {
  s <- make_lab_study(function(l) { l$CONCENTRATION[3] <- ">ULOQ"; l$CONCENTRATION[4] <- ""; l })
  r <- lab_run(s)
  expect_true(r$released)
  bl <- ub_read_dataset(file.path(s$ub, "private", "blinded", "labpc.xpt"))$data
  expect_equal(bl$PCORRES[bl$LABROW == 4L], ">ULOQ")
  expect_true(is.na(bl$PCSTRESN[bl$LABROW == 4L]))
  expect_equal(bl$PCORRES[bl$LABROW == 5L], "")
})

test_that("an analyte given by name gets the blinded name", {
  s <- make_lab_study(function(l) {
    l$ANALYTE <- c(FICT = "Fictinib", SYNZ = "Synthazole", SYNZM1 = "Synthazole M1")[l$ANALYTE]; l })
  o <- ub_read_options(s$ub)
  o$prepare_pc$analytes <- list(Fictinib = "FICT", Synthazole = "SYNZ", `Synthazole M1` = "SYNZM1")
  ub_write_options(s$ub, o, backup = FALSE)
  p <- ub_lab_release_prepare(s$ub, write = FALSE)
  expect_true(p$layout$analyte_is_name)
  expect_equal(p$layout$columns$labpc_name[p$layout$columns$role == "analyte"], "PCTEST")
  lab_run(s)
  bl <- ub_read_dataset(file.path(s$ub, "private", "blinded", "labpc.xpt"))$data
  expect_true(all(grepl("^ANL", bl$PCTEST)))
})

test_that("PC without results does not stop the run in Workflow 3", {
  s <- make_lab_study()
  r <- ub_blind_folder(s$ub, ub_options_for_engine(ub_read_options(s$ub)), quiet = TRUE, steps = "build_maps")
  expect_false(grepl("Prepare PC", r$error %||% ""))
  expect_true(file.exists(file.path(s$ub, "private", "lab", "labpc.xpt")))
  expect_true(any(grepl("Workflow 3", readLines(file.path(s$ub, "private", "logs", "blind.log")))))
})

# ---- Task 5: the released lab file -------------------------------------------------------------------------------
read_lab_out <- function(p) utils::read.csv(p, colClasses = "character", check.names = FALSE, na.strings = character(),
                                            fileEncoding = "UTF-8")

test_that("the released lab file has the lab layout and the PC link", {
  s <- make_lab_study()
  r <- lab_run(s)
  expect_true(r$released)
  out <- file.path(s$ub, "output")
  expect_true(file.exists(file.path(out, "lab_results_blinded.csv")))
  expect_false(file.exists(file.path(out, "labpc.xpt")))
  L <- read_lab_out(file.path(out, "lab_results_blinded.csv"))
  expect_equal(names(L), c("UBRECID", "LAB_ROW", names(s$lab), "PC_UBRECID", "MATCH_STATUS"))
  expect_equal(nrow(L), nrow(s$lab) - 1)
  pc <- ub_read_dataset(file.path(out, "pc.xpt"))$data
  linked <- nzchar(L$PC_UBRECID)
  expect_true(all(L$PC_UBRECID[linked] %in% pc$UBRECID))
  expect_true(all(L$MATCH_STATUS[linked] == "matched"))
  expect_true(all(!nzchar(L$PC_UBRECID[L$MATCH_STATUS == "no PC record at this visit/time"])))
  bl <- ub_read_dataset(file.path(s$ub, "private", "blinded", "labpc.xpt"))$data
  b <- bl[match(L$UBRECID, bl$UBRECID), ]
  p <- pc[match(L$PC_UBRECID[linked], pc$UBRECID), ]
  expect_equal(p$USUBJID, L$SUBJECT_ID[linked])                                   # same blinded subject
  expect_equal(p$USUBJID, b$USUBJID[linked])
  expect_equal(p$VISIT, b$VISIT[linked])                                          # the translated visit
  expect_equal(as.numeric(p$PCTPTNUM), as.numeric(b$PCTPTNUM[linked]))
  rep <- L[L$LAB_ROW %in% c("2", as.character(nrow(s$lab))), ]
  expect_equal(rep$PC_UBRECID[1], rep$PC_UBRECID[2]); expect_true(nzchar(rep$PC_UBRECID[1]))
  expect_equal(unique(L$VISIT_NAME), c("P1D1", "P2D1"))                           # the lab's own texts
  expect_equal(unique(L$SPECIMEN), "Plasma")
  txt <- paste(readLines(file.path(out, "lab_results_blinded.csv"), encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl(REAL_LOOKING, txt, perl = TRUE))
  man <- readLines(file.path(out, "RELEASE_MANIFEST.md"))
  expect_true(any(grepl("lab_results_blinded.csv", man, fixed = TRUE)))
  expect_true(any(grepl(sprintf("Lab file: %d rows released, %d matched to PC, %d not matched",
                                nrow(L), sum(L$MATCH_STATUS == "matched"), sum(L$MATCH_STATUS != "matched")), man, fixed = TRUE)))
})

test_that("QC: lab_match_rate warns under 50 percent, and the file is still released", {
  s <- make_lab_study()
  o <- ub_read_options(s$ub); o$prepare_pc$visit_map <- list(P1D1 = "X", P2D1 = "Y"); ub_write_options(s$ub, o, backup = FALSE)
  r <- lab_run(s)
  expect_true(r$released)
  f <- jsonlite::fromJSON(file.path(s$ub, "private", "qc", "qc_report.json"))$findings
  expect_true(any(f$check == "lab_match_rate" & f$status == "WARN"))
})

test_that("QC fails a kept lab column whose name is a drug name", {
  s <- make_lab_study(function(l) { names(l)[names(l) == "COMMENT"] <- "FICTINIB"; l })
  o <- ub_read_options(s$ub); o$lab_release$keep_columns <- list("RUN_DATE", "FICTINIB"); ub_write_options(s$ub, o, backup = FALSE)
  r <- lab_run(s)
  expect_false(r$released)
  f <- jsonlite::fromJSON(file.path(s$ub, "private", "qc", "qc_report.json"))$findings
  expect_true(any(f$check == "identity_in_varname" & f$dataset == "LABPC" & f$status == "FAIL"))
})

test_that("verify rebuilds the lab file and catches a change", {
  s <- make_lab_study()
  lab_run(s)
  v <- jsonlite::fromJSON(file.path(s$ub, "private", "qc", "verify_report.json"))
  expect_equal(v$LABPC$status, "PASS")
  p <- file.path(s$ub, "private", "blinded", "lab_results_blinded.csv")
  x <- read_lab_out(p); x$CONCENTRATION[5] <- "999"
  utils::write.csv(x, p, row.names = FALSE, fileEncoding = "UTF-8")
  ub_blind_folder(s$ub, ub_options_for_engine(ub_read_options(s$ub)), quiet = TRUE, steps = "verify")
  v <- jsonlite::fromJSON(file.path(s$ub, "private", "qc", "verify_report.json"))
  expect_equal(v$LABPC$status, "FAIL")
  expect_match(paste(v$LABPC$notes, collapse = " "), "lab file")
})

test_that("the next run clears an old lab file from output", {
  s <- make_lab_study()
  dir.create(file.path(s$ub, "output"), showWarnings = FALSE)
  writeLines("x", file.path(s$ub, "output", "old_blinded.csv"))
  ub_blind_folder(s$ub, ub_options_for_engine(ub_read_options(s$ub)), quiet = TRUE, steps = "build_maps")
  expect_false(file.exists(file.path(s$ub, "output", "old_blinded.csv")))
})
