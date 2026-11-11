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
  expect_equal(names(L), c("UBRECID", names(s$lab), "PC_UBRECID", "MATCH_STATUS"))   # no LAB_ROW: it would show the source order
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
  rep <- L[match(bl$UBRECID[bl$LABROW %in% c(2L, nrow(s$lab))], L$UBRECID), ]
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

# ---- Task 6: Retrieve ----------------------------------------------------------------------------------------------
test_that("Retrieve returns the original lab rows, under any name of the lab file", {
  s <- make_lab_study()
  lab_run(s)
  L <- read_lab_out(file.path(s$ub, "output", "lab_results_blinded.csv"))
  pick <- L[c(3, 10), ]
  for (ds in c("lab_results.csv", "LAB_RESULTS", "lab_results_blinded.csv", "LABPC")) {
    res <- ub_retrieve_folder(s$ub, list(c(DATASET = ds, UBRECID = paste(pick$UBRECID, collapse = ";"), FINDING = "odd")),
                              quiet = TRUE)
    got <- res$LABPC
    expect_equal(nrow(got), 2)
    lr <- ub_read_dataset(file.path(s$ub, "private", "blinded", "labpc.xpt"))$data
    row <- as.integer(lr$LABROW[match(pick$UBRECID, lr$UBRECID)])
    orig <- s$lab[row - 1L, ]
    expect_equal(got$SUBJECT_ID, orig$SUBJECT_ID)
    expect_equal(got$CONCENTRATION, orig$CONCENTRATION)
    expect_equal(got$RUN_DATE, orig$RUN_DATE)
    expect_equal(got$LAB_ROW, row)
  }
  expect_true(file.exists(file.path(s$ub, "private", "retrieved", "labpc.csv")))
  # findings on the datasets still work next to it
  pc <- ub_read_dataset(file.path(s$ub, "output", "pc.xpt"))$data
  res <- ub_retrieve_folder(s$ub, list(c(DATASET = "PC", UBRECID = pc$UBRECID[1]),
                                       c(DATASET = "lab_results.csv", UBRECID = pick$UBRECID[1])), quiet = TRUE)
  expect_equal(nrow(res$PC), 1); expect_equal(nrow(res$LABPC), 1)
})

test_that("Retrieve refuses a changed lab file", {
  s <- make_lab_study()
  lab_run(s)
  L <- read_lab_out(file.path(s$ub, "output", "lab_results_blinded.csv"))
  cat("901-100,P1D1,1,FICT,Plasma,1.0,ng/mL,0.5,01-Jan-2025,\n", file = file.path(s$ub, "input", "lab_results.csv"), append = TRUE)
  expect_error(ub_retrieve_folder(s$ub, list(c(DATASET = "lab_results.csv", UBRECID = L$UBRECID[1])), quiet = TRUE),
               "lab file changed")
})

test_that("a lab finding needs record identifiers", {
  s <- make_lab_study()
  lab_run(s)
  expect_error(ub_retrieve_folder(s$ub, list(c(DATASET = "lab_results.csv", VISIT_NAME = "P1D1")), quiet = TRUE), "UBRECID")
})

# ---- final review fixes -------------------------------------------------------------------------------------------
rename_lab <- function(s, new) {
  file.rename(file.path(s$ub, "input", "lab_results.csv"), file.path(s$ub, "input", new))
  o <- ub_read_options(s$ub); o$prepare_pc$results_file <- new; ub_write_options(s$ub, o, backup = FALSE)
}

test_that("C1: the released lab file has a fixed name, whatever the lab file is called", {
  s <- make_lab_study()
  rename_lab(s, "SYN-PK-001_bioanalysis_2023-03-15.csv")
  expect_true(lab_run(s)$released)
  out <- list.files(file.path(s$ub, "output"))
  expect_true("lab_results_blinded.csv" %in% out)
  expect_false(any(grepl("SYN-PK|2023", out)))
  expect_false(any(grepl("SYN-PK|2023-03-15", readLines(file.path(s$ub, "output", "RELEASE_MANIFEST.md")))))
  L <- read_lab_out(file.path(s$ub, "output", "lab_results_blinded.csv"))
  res <- ub_retrieve_folder(s$ub, list(c(DATASET = "SYN-PK-001_bioanalysis_2023-03-15.csv", UBRECID = L$UBRECID[1])), quiet = TRUE)
  expect_equal(nrow(res$LABPC), 1)
})

test_that("C2: kept dates in common lab formats are read and shifted; a date-named column that can't be read stops", {
  conv <- function(v) .ub_lab_dates(v, "RUN_DATE")
  expect_equal(conv(c("16.03.2023", "01.12.2024"))$iso, c("2023-03-16", "2024-12-01"))
  expect_equal(conv(c("2023/03/16", "2024/12/01"))$iso, c("2023-03-16", "2024-12-01"))
  expect_equal(conv(c("16-Mar-23", "01-Dec-24"))$iso, c("2023-03-16", "2024-12-01"))
  expect_equal(conv(c("45001", "45627"))$iso, c("2023-03-16", "2024-12-01"))             # Excel day numbers
  expect_equal(conv(c("45001.5", ""))$iso, c("2023-03-16T12:00", ""))
  expect_true(conv(c("45001", "45627"))$is_date)
  expect_false(.ub_lab_dates(c("45001", "45627"), "RUN_NO")$is_date)                     # a number column is not a date
  s <- make_lab_study(function(l) { l$RUN_DATE <- format(as.Date(substr(l$RUN_DATE, 1, 11), "%d-%b-%Y"), "%d.%m.%Y"); l })
  lab_run(s)
  bl <- ub_read_dataset(file.path(s$ub, "private", "blinded", "labpc.xpt"))$data
  src <- ub_read_dataset(file.path(s$ub, "private", "lab", "labpc.xpt"))$data
  keys <- ub_load_keys(file.path(s$ub, "private", "maps"))
  o <- src[match(bl$LABROW, src$LABROW), ]
  expect_equal(as.Date(bl$K01DTC), as.Date(o$K01DTC) + as.integer(keys$shift[o$USUBJID]))
  s2 <- make_lab_study(function(l) { l$RUN_DATE[3] <- "pending"; l })
  expect_error(ub_lab_release_prepare(s2$ub, write = FALSE), "looks like a date column")
})

test_that("C2: a date in a lab text released as it is fails QC", {
  s <- make_lab_study(function(l) { l$VISIT_NAME[5] <- "UNS 15.03.2023"; l })
  r <- lab_run(s)
  expect_false(r$released)
  f <- jsonlite::fromJSON(file.path(s$ub, "private", "qc", "qc_report.json"))$findings
  expect_true(any(f$check == "free_text_date" & f$dataset == "LABPC" & f$status == "FAIL"))
})

test_that("C3: one analyte code: rows link only to PC records of that analyte", {
  s <- make_lab_study(function(l) l[l$ANALYTE == "SYNZ", ])
  o <- ub_read_options(s$ub); o$prepare_pc$analytes <- NULL; o$prepare_pc$columns$analyte <- NULL
  o$prepare_pc$pctestcd <- "SYNZ"; o$prepare_pc$pctest <- "Synthazole"; ub_write_options(s$ub, o, backup = FALSE)
  p <- ub_lab_release_prepare(s$ub, write = FALSE)
  pc <- ub_read_dataset(file.path(s$ub, "input", "pc.xpt"))$data
  expect_equal(p$counts[["several"]], 0)
  expect_true(p$counts[["matched"]] >= p$counts[["rows"]] - 2)
  m <- p$labpc$PCSRCROW[!is.na(p$labpc$PCSRCROW)]
  expect_true(all(pc$PCTESTCD[m] == "SYNZ"))
})

test_that("C4: a result with a number that is neither a plain number nor BLQ<(x) is left out, not released", {
  odd <- c("12.3 ng/mL", "1,234.5", "<0.500", "BLQ<0.5")
  s <- make_lab_study(function(l) { l$CONCENTRATION[3:6] <- odd; l$CONCENTRATION[7] <- ">ULOQ"; l })
  p <- ub_lab_release_prepare(s$ub, write = FALSE)
  expect_true(all(3:6 + 1L %in% p$left_out$lab_row))
  expect_match(p$left_out$reason[p$left_out$lab_row == 4L], "neither a number")
  expect_false(8L %in% p$left_out$lab_row)                                              # >ULOQ carries no value
  lab_run(s)
  txt <- readLines(file.path(s$ub, "output", "lab_results_blinded.csv"), encoding = "UTF-8")
  for (x in odd) expect_false(any(grepl(x, txt, fixed = TRUE)))
})

test_that("I1: a kept column holding numbers is refused", {
  s <- make_lab_study(function(l) { l$RAWCONC <- l$LLOQ; l })
  o <- ub_read_options(s$ub); o$lab_release$keep_columns <- list("RUN_DATE", "RAWCONC"); ub_write_options(s$ub, o, backup = FALSE)
  expect_error(ub_lab_release_prepare(s$ub, write = FALSE), "holds numbers")
})

test_that("I4: lab findings are found after a later release without the lab file", {
  s <- make_lab_study()
  lab_run(s)
  L <- read_lab_out(file.path(s$ub, "output", "lab_results_blinded.csv"))
  o <- ub_read_options(s$ub); o$lab_release$enabled <- FALSE; ub_write_options(s$ub, o, backup = FALSE)
  ub_codebook_confirm_review(s$ub, "Test")
  r <- ub_blind_folder(s$ub, ub_options_for_engine(o), quiet = TRUE, continue_without_results = TRUE)
  expect_true(r$released)
  res <- ub_retrieve_folder(s$ub, list(c(DATASET = "lab_results.csv", UBRECID = L$UBRECID[1])), quiet = TRUE)
  expect_equal(nrow(res$LABPC), 1)
})
