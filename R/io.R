#' Read a dataset keeping its SAS metadata
#'
#' @param path .sas7bdat, .xpt or .csv file
#' @return list(data = data.frame, meta = list(file_label, labels, formats))
#' @export
ub_read_dataset <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("sas7bdat", "xpt")) {
    df <- if (ext == "xpt") haven::read_xpt(path) else haven::read_sas(path)
    file_label <- attr(df, "label"); if (is.null(file_label)) file_label <- ""
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    labels <- vapply(names(df), function(c) { l <- attr(df[[c]], "label"); if (is.null(l)) "" else l }, "")
    formats <- vapply(names(df), function(c) { f <- attr(df[[c]], "format.sas"); if (is.null(f)) "" else f }, "")
    for (c in names(df)) if (is.character(df[[c]])) df[[c]][is.na(df[[c]])] <- ""
    return(list(data = df, meta = list(file_label = file_label, labels = labels, formats = formats)))
  }
  if (ext %in% c("csv", "tsv")) {
    df <- utils::read.csv(path, sep = if (ext == "tsv") "\t" else ",", colClasses = "character",
                          na.strings = character(), check.names = FALSE)
    return(list(data = df, meta = list(file_label = "", labels = setNames(rep("", ncol(df)), names(df)),
                                       formats = setNames(rep("", ncol(df)), names(df)))))
  }
  ub_error("Unsupported dataset format: ", path)
}

#' Write a dataset (.xpt = SAS transport V5, or .csv)
#' @param df data.frame
#' @param path output file (.xpt or .csv)
#' @param meta list(file_label, labels, formats)
#' @param name XPT member name (default: file name)
#' @export
ub_write_dataset <- function(df, path, meta = NULL, name = NULL) {
  ext <- tolower(tools::file_ext(path))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (ext == "sas7bdat") ub_error("Cannot write .sas7bdat; write .xpt (SAS transport V5) or .csv instead")
  if (ext == "xpt") {
    for (c in names(df)) {
      if (is.character(df[[c]])) {
        if (any(nchar(df[[c]], type = "bytes") > 200, na.rm = TRUE))
          ub_error("Character values longer than 200 bytes cannot be stored in XPT V5: ", c)
        df[[c]][is.na(df[[c]])] <- ""
      }
      if (!is.null(meta)) {
        l <- meta$labels[c]; if (!is.na(l) && nzchar(l)) attr(df[[c]], "label") <- substr(l, 1, 40)
        f <- meta$formats[c]; if (!is.na(f) && nzchar(f)) attr(df[[c]], "format.sas") <- sub("\\.$", "", f)
      }
    }
    member <- toupper(substr(if (is.null(name)) tools::file_path_sans_ext(basename(path)) else name, 1, 8))
    lab <- if (is.null(meta)) NULL else substr(meta$file_label, 1, 40)
    haven::write_xpt(df, path, version = 5, name = member, label = if (length(lab) && nzchar(lab)) lab else NULL)
    return(invisible(path))
  }
  utils::write.csv(df, path, row.names = FALSE, na = "")
  invisible(path)
}
