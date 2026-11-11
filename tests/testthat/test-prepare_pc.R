# Workflow 2: ub_prepare_pc() fills results from a separate lab file into PC before blinding.

PPC_RESULT_VARS <- c("PCORRES", "PCSTRESC", "PCSTRESN", "PCORRESU", "PCSTRESU", "PCLLOQ")

ppc_hours <- function(tpt) {
  m <- regmatches(tpt, regexec("^\\s*([0-9.]+)\\s*H", tpt))
  vapply(m, function(z) if (length(z) == 2) as.numeric(z[2]) else 0, 0)
}

# A study folder whose PC has the sample records but no results, plus the results as a lab CSV.
ppc_study <- function(one_analyte = NULL) {
  f <- tempfile("ppcstudy")
  dir.create(file.path(f, "input"), recursive = TRUE)
  src <- system.file("examples", "synthetic", "source", package = "universalblinder")
  file.copy(list.files(src, pattern = "\\.xpt$", full.names = TRUE), file.path(f, "input"))
  rd <- ub_read_dataset(file.path(f, "input", "pc.xpt"))
  pc <- rd$data
  if (!is.null(one_analyte)) { pc <- pc[pc$PCTESTCD == one_analyte, , drop = FALSE]; rownames(pc) <- NULL }
  pc$PCTPTNUM <- ppc_hours(pc$PCTPT)
  full <- pc
  parts <- strsplit(full$USUBJID, "-", fixed = TRUE)
  lab <- data.frame(Subject = vapply(parts, function(p) paste(utils::tail(p, 2), collapse = "-"), ""),
                    Visit = sub(" DAY ", "D", sub("PERIOD ", "P", full$VISIT, fixed = TRUE), fixed = TRUE),
                    `Nominal Hour` = full$PCTPTNUM, Analyte = full$PCTEST, Conc = full$PCORRES,
                    Unit = full$PCORRESU, LLOQ = full$PCLLOQ, check.names = FALSE, stringsAsFactors = FALSE)
  keep <- full$PCORRES != ""
  lab <- lab[keep, , drop = FALSE]
  utils::write.csv(lab, file.path(f, "input", "lab_results.csv"), row.names = FALSE)
  for (c in PPC_RESULT_VARS) pc[[c]] <- if (c %in% c("PCSTRESN", "PCLLOQ")) NA_real_ else ""
  if (!is.null(one_analyte)) { pc$PCTESTCD <- "ANALYTE"; pc$PCTEST <- "Analyte" }   # placeholder, as delivered
  ub_write_dataset(pc, file.path(f, "input", "pc.xpt"), rd$meta, name = "PC")
  visits <- as.list(stats::setNames(full$VISIT[keep], lab$Visit))
  visits <- visits[!duplicated(names(visits))]
  list(folder = f, full = full, lab = lab, visits = visits)
}

PPC_SETTINGS <- list(results_file = "lab_results.csv",
                     columns = list(subject = "Subject", visit = "Visit", nominal_time = "Nominal Hour", result = "Conc",
                                    unit = "Unit", lloq = "LLOQ", analyte = "Analyte"),
                     check_dataset = "")

ppc_set <- function(...) utils::modifyList(PPC_SETTINGS, list(...))

test_that("several analytes fill PC; the original PC is kept; the log is written", {
  s <- ppc_study()
  analytes <- as.list(stats::setNames(s$full$PCTESTCD, s$full$PCTEST)); analytes <- analytes[!duplicated(names(analytes))]
  r <- ub_prepare_pc(s$folder, ppc_set(visit_map = s$visits, analytes = analytes), quiet = TRUE)
  expect_true(r$ok); expect_true(r$written)
  expect_equal(r$n_matched, r$n_lab)
  expect_equal(nrow(r$unmatched), 0)
  pc <- ub_read_dataset(file.path(s$folder, "input", "pc.xpt"))$data
  expect_equal(pc$PCORRES, s$full$PCORRES, ignore_attr = TRUE)
  expect_equal(pc$PCORRESU, s$full$PCORRESU, ignore_attr = TRUE)
  expect_equal(pc$PCSTRESC, s$full$PCORRES, ignore_attr = TRUE)                    # the reported text, in the reported unit
  expect_true(all(abs(ifelse(is.na(pc$PCSTRESN), 0, pc$PCSTRESN) - ifelse(is.na(s$full$PCSTRESN), 0, s$full$PCSTRESN)) < 1e-12))
  expect_true(all(is.na(pc$PCSTRESN) == is.na(s$full$PCSTRESN)))
  expect_true(file.exists(file.path(s$folder, "private", "source_original", "pc.xpt")))
  expect_match(paste(readLines(r$log_file), collapse = "\n"), "Matched: ", fixed = TRUE)
})

test_that("the filled PC can be blinded and released", {
  s <- ppc_study()
  analytes <- as.list(stats::setNames(s$full$PCTESTCD, s$full$PCTEST)); analytes <- analytes[!duplicated(names(analytes))]
  expect_true(ub_prepare_pc(s$folder, ppc_set(visit_map = s$visits, analytes = analytes), quiet = TRUE)$ok)
  opts <- list(rename_vars = list(ADSL = list(FCTFL = "TRTFL01", SYZFL = "TRTFL02"),
                                  ADEX = list(DOSEFCT = "DOSET01", DOSESYZ = "DOSET02")))
  first <- ub_blind_folder(s$folder, opts, quiet = TRUE)              # codebook review stop
  expect_false(first$released)
  res <- ub_blind_folder(s$folder, opts, quiet = TRUE)
  expect_true(res$released)
})

test_that("one analyte replaces the placeholder code; a rerun gives the same file", {
  s <- ppc_study("SYNZ")
  st <- ppc_set(visit_map = s$visits, pctestcd = "SYNZ", pctest = "Synthazole")
  r <- ub_prepare_pc(s$folder, st, quiet = TRUE)
  expect_true(r$ok)
  pc <- ub_read_dataset(file.path(s$folder, "input", "pc.xpt"))$data
  expect_equal(unique(pc$PCTESTCD), "SYNZ"); expect_equal(unique(pc$PCTEST), "Synthazole")
  expect_equal(pc$PCORRES, s$full$PCORRES, ignore_attr = TRUE)
  expect_true(any(grepl("was ANALYTE", r$messages, fixed = TRUE)))
  expect_true(ub_prepare_pc(s$folder, st, quiet = TRUE)$ok)
  again <- ub_read_dataset(file.path(s$folder, "input", "pc.xpt"))$data
  expect_identical(again, pc)
})

test_that("an unmatched visit stops without writing and names visit_map", {
  s <- ppc_study("SYNZ")
  before <- readBin(file.path(s$folder, "input", "pc.xpt"), "raw", 1e7)
  r <- ub_prepare_pc(s$folder, ppc_set(visit_map = list(), pctestcd = "SYNZ", pctest = "Synthazole"), quiet = TRUE)
  expect_false(r$ok); expect_false(r$written)
  expect_identical(readBin(file.path(s$folder, "input", "pc.xpt"), "raw", 1e7), before)
  log <- paste(readLines(file.path(s$folder, "private", "logs", "prepare_pc.log")), collapse = "\n")
  expect_match(log, "not in PC.VISIT", fixed = TRUE)
  expect_match(log, "visit_map", fixed = TRUE)
  expect_equal(nrow(r$unmatched), r$n_lab)
  expect_true(all(r$unmatched$setting_to_change == "visit_map"))
  expect_setequal(r$unmatched_values$visits, unique(s$lab$Visit))
  expect_setequal(r$pc_values$visits, unique(s$full$VISIT))
})

test_that("a result that is not a number or BQL/BLQ stops; BLQ<(x) is accepted", {
  s <- ppc_study("SYNZ")
  lab <- s$lab
  lab$Conc[1] <- sub("^BQL", "BLQ", lab$Conc[1])
  lab$Conc[2] <- "<0.5"
  utils::write.csv(lab, file.path(s$folder, "input", "lab_results.csv"), row.names = FALSE)
  st <- ppc_set(visit_map = s$visits, pctestcd = "SYNZ", pctest = "Synthazole")
  r <- ub_prepare_pc(s$folder, st, quiet = TRUE)
  expect_false(r$ok)
  expect_equal(nrow(r$unmatched), 1)
  expect_equal(r$unmatched$lab_row, 3L)                  # header = row 1
  expect_match(r$unmatched$reason, "neither a number")
  lab$Conc[2] <- s$lab$Conc[2]
  utils::write.csv(lab, file.path(s$folder, "input", "lab_results.csv"), row.names = FALSE)
  r <- ub_prepare_pc(s$folder, st, quiet = TRUE)
  expect_true(r$ok)
  pc <- ub_read_dataset(file.path(s$folder, "input", "pc.xpt"))$data
  expect_true(startsWith(pc$PCORRES[1], "BLQ<("))
  expect_true(is.na(pc$PCSTRESN[1]))
})

test_that("values already in PC are kept and reported; write = FALSE writes nothing", {
  s <- ppc_study("SYNZ")
  orig <- file.path(s$folder, "input", "pc.xpt")
  rd <- ub_read_dataset(orig); pc <- rd$data
  i <- which(s$full$PCORRES != "" & !grepl("^BQL", s$full$PCORRES))[1]
  pc$PCORRES[i] <- "999"
  ub_write_dataset(pc, orig, rd$meta, name = "PC")
  before <- readBin(orig, "raw", 1e7)
  st <- ppc_set(visit_map = s$visits, pctestcd = "SYNZ", pctest = "Synthazole")
  dry <- ub_prepare_pc(s$folder, st, write = FALSE, quiet = TRUE)
  expect_true(dry$ok); expect_false(dry$written)
  expect_identical(readBin(orig, "raw", 1e7), before)
  expect_false(dir.exists(file.path(s$folder, "private")))
  r <- ub_prepare_pc(s$folder, st, quiet = TRUE)
  expect_true(r$ok)
  expect_equal(nrow(r$conflicts), 1)
  expect_equal(r$conflicts$variable, "PCORRES")
  expect_equal(r$conflicts$pc_value, "999")
  out <- ub_read_dataset(orig)$data
  expect_equal(out$PCORRES[i], "999")
  expect_equal(out$PCSTRESC[i], s$full$PCORRES[i])
})

test_that("cross-check against AVAL flags a differing record", {
  s <- ppc_study("SYNZ")
  chk <- data.frame(USUBJID = s$full$USUBJID, VISIT = s$full$VISIT, PCTPT = s$full$PCTPT,
                    AVAL = ifelse(is.na(s$full$PCSTRESN), 0, s$full$PCSTRESN), stringsAsFactors = FALSE)
  ub_write_dataset(chk, file.path(s$folder, "input", "chk.xpt"), name = "CHK")
  st <- ppc_set(visit_map = s$visits, pctestcd = "SYNZ", pctest = "Synthazole", check_dataset = "chk.xpt")
  r <- ub_prepare_pc(s$folder, st, write = FALSE, quiet = TRUE)
  expect_equal(nrow(r$aval_check), r$n_matched)
  expect_true(all(r$aval_check$status == "ok"))
  chk$AVAL[5] <- chk$AVAL[5] + 1
  ub_write_dataset(chk, file.path(s$folder, "input", "chk.xpt"), name = "CHK")
  r <- ub_prepare_pc(s$folder, st, write = FALSE, quiet = TRUE)
  expect_equal(sum(r$aval_check$status == "red"), 1)
  expect_true(any(grepl("cross-check: 1 of", r$messages, fixed = TRUE)))
})

test_that("missing settings and unknown columns give plain messages", {
  s <- ppc_study("SYNZ")
  r <- ub_prepare_pc(s$folder, list(results_file = ""), write = FALSE, quiet = TRUE)
  expect_false(r$ok); expect_match(r$messages[length(r$messages)], "results_file is empty")
  r <- ub_prepare_pc(s$folder, ppc_set(pctestcd = "", pctest = ""), write = FALSE, quiet = TRUE)
  expect_match(paste(r$messages, collapse = " "), "pctestcd and pctest")
  st <- ppc_set(pctestcd = "SYNZ", pctest = "Synthazole")
  st$columns$visit <- "VISIT NAME"
  r <- ub_prepare_pc(s$folder, st, write = FALSE, quiet = TRUE)
  expect_match(paste(r$messages, collapse = " "), "not in lab_results.csv")
  expect_false(any(grepl("python", r$messages, ignore.case = TRUE)))
})

test_that("Excel (named sheet) and tab-separated text files are read; helpers list files and columns", {
  skip_if_not_installed("openxlsx"); skip_if_not_installed("readxl")
  s <- ppc_study("SYNZ")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Notes"); openxlsx::writeData(wb, "Notes", data.frame(x = "read me"))
  lab <- s$lab; lab$`Nominal Hour` <- as.numeric(lab$`Nominal Hour`); lab$LLOQ <- as.numeric(lab$LLOQ)
  openxlsx::addWorksheet(wb, "Results"); openxlsx::writeData(wb, "Results", lab)
  openxlsx::saveWorkbook(wb, file.path(s$folder, "input", "lab.xlsx"))
  utils::write.table(s$lab, file.path(s$folder, "input", "lab.txt"), sep = "\t", row.names = FALSE, quote = FALSE)
  expect_setequal(ub_lab_files(s$folder), c("lab.txt", "lab.xlsx", "lab_results.csv"))
  expect_equal(ub_prepare_pc_columns(s$folder, "lab.xlsx", sheet = "Results"), names(s$lab))
  expect_equal(ub_prepare_pc_columns(s$folder, "lab.txt"), names(s$lab))
  for (st in list(ppc_set(results_file = "lab.xlsx", sheet = "Results"), ppc_set(results_file = "lab.txt"))) {
    st <- utils::modifyList(st, list(visit_map = s$visits, pctestcd = "SYNZ", pctest = "Synthazole"))
    r <- ub_prepare_pc(s$folder, st, write = FALSE, quiet = TRUE)
    expect_true(r$ok, info = paste(r$messages, collapse = "\n"))
    expect_equal(r$n_matched, nrow(s$lab))
  }
  r <- ub_prepare_pc(s$folder, utils::modifyList(PPC_SETTINGS, list(results_file = "lab.xlsx", sheet = "Nope",
                                                                     pctestcd = "SYNZ", pctest = "Synthazole")),
                     write = FALSE, quiet = TRUE)
  expect_match(paste(r$messages, collapse = " "), "no sheet 'Nope'")
})

test_that("a predose sample of the next day is matched through time_map", {
  s <- ppc_study("SYNZ")
  lab <- s$lab
  lab$Tpt <- s$full$PCTPT[s$full$PCORRES != ""]
  pre <- which(lab$`Nominal Hour` == 0)[1]
  lab$`Nominal Hour`[pre] <- 99             # the lab's own hour for it
  utils::write.csv(lab, file.path(s$folder, "input", "lab_results.csv"), row.names = FALSE)
  st <- ppc_set(visit_map = s$visits, pctestcd = "SYNZ", pctest = "Synthazole")
  st$columns$timepoint_name <- "Tpt"
  r <- ub_prepare_pc(s$folder, st, write = FALSE, quiet = TRUE)
  expect_false(r$ok)
  expect_equal(r$unmatched$setting_to_change, "visit_map or time_map")
  expect_equal(r$unmatched_values$timepoints, lab$Tpt[pre])
  # the time point name of that row maps to 0 h; every other PREDOSE row is at 0 h already
  st$time_map <- stats::setNames(list(0), lab$Tpt[pre])
  expect_true(ub_prepare_pc(s$folder, st, write = FALSE, quiet = TRUE)$ok)
})
