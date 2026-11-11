example_study <- function() {
  d <- tempfile("ubstudy")
  dir.create(d)
  src <- system.file("examples", "synthetic", package = "universalblinder")
  file.copy(file.path(src, "source"), d, recursive = TRUE)
  file.copy(file.path(src, "study.json"), d)
  d
}
edit_study <- function(dir, fn) {
  p <- file.path(dir, "study.json")
  s <- jsonlite::fromJSON(p, simplifyVector = FALSE)
  s <- fn(s)
  writeLines(jsonlite::toJSON(s, auto_unbox = TRUE, pretty = TRUE, null = "null"), p)
}
REAL_LOOKING <- "(?i)fictinib|synthazole|\\bFCT\\b|\\bSYZ\\b|SZ-M1|SYN-PK-001|\\b90[1-4]-\\d{3}\\b"

# rows of a released/retrieved dataset put back in SOURCE order via the latest crosswalk (UBRECID dropped)
source_order <- function(private_dir, nm, df) {
  root <- file.path(private_dir, "crosswalk")
  rid <- sort(list.dirs(root, full.names = FALSE, recursive = FALSE), decreasing = TRUE)[1]
  x <- utils::read.csv(file.path(root, rid, paste0(tolower(nm), ".csv")), colClasses = c("character", "integer"))
  o <- df[order(x$SRC_ROW[match(df$UBRECID, x$UBRECID)]), setdiff(names(df), "UBRECID"), drop = FALSE]
  rownames(o) <- NULL
  o
}

# the first run of a study folder writes the codebook and stops once for its review
first_run <- function(f, opts = list()) {
  r <- ub_blind_folder(f, opts, quiet = TRUE, steps = "build_maps")
  testthat::expect_false(r$released)
  testthat::expect_true(file.exists(file.path(f, "private", "codebook.csv")))
  invisible(r)
}

# Workflow 3: a UB folder whose PC has no results, and the results as a lab CSV that cannot all be matched:
# partial subject IDs, lab visit names (visit_map), title-case specimen, a run date (dd-Mon-yyyy) and a comment naming
# the drug (both kept), one unscheduled sample (26 h), one repeat of row 1, one subject that is not in PC.
make_lab_study <- function(edit_lab = NULL) {
  d <- tempfile("labstudy"); dir.create(d)
  src <- system.file("examples", "synthetic", "source", package = "universalblinder")
  ub <- ub_setup_study(d, "SYN")$path
  file.copy(list.files(src, full.names = TRUE), file.path(ub, "input"))
  rd <- ub_read_dataset(file.path(ub, "input", "pc.xpt")); pc <- rd$data
  m <- regmatches(pc$PCTPT, regexec("^\\s*([0-9.]+)\\s*H", pc$PCTPT))
  pc$PCTPTNUM <- vapply(m, function(z) if (length(z) == 2) as.numeric(z[2]) else 0, 0)
  full <- pc
  parts <- strsplit(full$USUBJID, "-", fixed = TRUE)
  dt <- as.Date(substr(full$PCDTC, 1, 10)) + 3
  lab <- data.frame(SUBJECT_ID = vapply(parts, function(p) paste(utils::tail(p, 2), collapse = "-"), ""),
                    VISIT_NAME = sub(" DAY ", "D", sub("PERIOD ", "P", full$VISIT, fixed = TRUE), fixed = TRUE),
                    `HOUR NOMINAL` = format(full$PCTPTNUM, trim = TRUE), ANALYTE = full$PCTESTCD, SPECIMEN = "Plasma",
                    CONCENTRATION = full$PCORRES, UNIT = full$PCORRESU, LLOQ = format(full$PCLLOQ, trim = TRUE),
                    RUN_DATE = sprintf("%02d-%s-%s", as.integer(format(dt, "%d")), month.abb[as.integer(format(dt, "%m"))],
                                       format(dt, "%Y")),
                    COMMENT = ifelse(seq_len(nrow(full)) %% 50 == 0, "reanalysis of FICTINIB sample", ""),
                    check.names = FALSE, stringsAsFactors = FALSE)
  extra <- lab[1, ]; extra$`HOUR NOMINAL` <- "26"                      # unscheduled: no PC record
  stranger <- lab[1, ]; stranger$SUBJECT_ID <- "999-999"              # not in PC
  lab <- rbind(lab, extra, lab[1, ], stranger)                         # lab[1, ] again = a repeat
  if (is.function(edit_lab)) lab <- edit_lab(lab)
  utils::write.csv(lab, file.path(ub, "input", "lab_results.csv"), row.names = FALSE)
  for (c in c("PCORRES", "PCSTRESC", "PCSTRESN", "PCORRESU", "PCSTRESU", "PCLLOQ"))
    pc[[c]] <- if (c %in% c("PCSTRESN", "PCLLOQ")) NA_real_ else ""
  ub_write_dataset(pc, file.path(ub, "input", "pc.xpt"), rd$meta, name = "PC")
  o <- ub_read_options(ub)
  o$rename_vars <- list(ADSL = list(FCTFL = "TRTFL01", SYZFL = "TRTFL02"), ADEX = list(DOSEFCT = "DOSET01", DOSESYZ = "DOSET02"))
  o$prepare_pc <- list(results_file = "lab_results.csv",
                       columns = list(subject = "SUBJECT_ID", visit = "VISIT_NAME", nominal_time = "HOUR NOMINAL",
                                      result = "CONCENTRATION", unit = "UNIT", lloq = "LLOQ", analyte = "ANALYTE",
                                      specimen = "SPECIMEN"),
                       visit_map = list(P1D1 = "PERIOD 1 DAY 1", P2D1 = "PERIOD 2 DAY 1"),
                       analytes = list(FICT = "FICT", SYNZ = "SYNZ", SYNZM1 = "SYNZM1"))
  o$lab_release <- list(enabled = TRUE, keep_columns = list("RUN_DATE", "COMMENT"))
  ub_write_options(ub, o, backup = FALSE)
  list(ub = ub, lab = lab, pc_full = full)
}
