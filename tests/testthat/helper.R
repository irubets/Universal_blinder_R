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
