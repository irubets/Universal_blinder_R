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
