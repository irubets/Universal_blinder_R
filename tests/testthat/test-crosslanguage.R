test_that("R reproduces the Python reference output exactly (same key)", {
  fx <- test_path("fixture")
  src_dir <- system.file("examples", "synthetic", "source", package = "universalblinder")
  cfg <- ub_load_config(file.path(fx, "blinding_config.json"))
  keys <- ub_load_keys(file.path(fx, "maps"))
  for (nm in c("ADSL", "EX", "EC", "PC", "ADEX", "ADNCA")) {
    src <- ub_read_dataset(file.path(src_dir, paste0(tolower(nm), ".xpt")))
    r <- ub_transform(src$data, src$meta, nm, cfg, keys, "blind")
    exp <- utils::read.csv(file.path(fx, "expected_blinded", paste0(tolower(nm), ".csv")), colClasses = "character",
                           na.strings = character(), check.names = FALSE)
    expect_identical(names(r$data), names(exp))
    for (c in names(exp)) {
      x <- r$data[[c]]
      if (inherits(x, "Date")) x <- as.numeric(x - as.Date("1960-01-01"))
      if (inherits(x, "POSIXct")) x <- as.numeric(x) + 315619200
      if (inherits(x, "hms")) x <- as.numeric(x)
      if (is.numeric(x)) {
        e <- suppressWarnings(as.numeric(exp[[c]]))
        ok <- (is.na(x) & is.na(e)) | (!is.na(x) & !is.na(e) & abs(x - e) <= 1e-9 * pmax(1, abs(e)))
      } else ok <- x == exp[[c]]
      expect_true(all(ok), info = paste(nm, c))
    }
    em <- utils::read.csv(file.path(fx, "expected_blinded", paste0(tolower(nm), "_meta.csv")), colClasses = "character", na.strings = character())
    expect_identical(r$meta$file_label, em$LABEL[1])
    lab <- setNames(em$LABEL[-1], em$VARIABLE[-1])
    expect_identical(unname(substr(r$meta$labels[names(lab)], 1, 40)), unname(substr(lab, 1, 40)))
  }
})
