# Date shifting -- mirrors python/dates.py. Works on R's Date / POSIXct classes
# (as read by haven) as well as on raw SAS numbers carrying a date format.

.DATE_FMTS <- c("DATE", "YYMMDD", "MMDDYY", "DDMMYY", "E8601DA", "IS8601DA", "B8601DA", "YYMMDDN",
                "MONYY", "WORDDATE", "WEEKDATE", "NLDATE", "DAY", "JULIAN", "MINGUO", "YYMON")
.DTM_FMTS <- c("DATETIME", "E8601DT", "IS8601DT", "B8601DT", "DATEAMPM", "MDYAMPM", "NLDATM", "E8601DN")
.TIME_FMTS <- c("TIME", "HHMM", "HOUR", "MMSS", "E8601TM", "IS8601TM", "B8601TM", "TOD", "NLTIME")

ub_classify_format <- function(fmt) {
  if (is.null(fmt) || is.na(fmt) || !nzchar(fmt)) return(NA_character_)
  b <- sub("[0-9.]+$", "", toupper(sub("^\\$", "", trimws(fmt))))
  starts <- function(set) any(startsWith(b, set))
  if (starts(.TIME_FMTS) && !starts(.DTM_FMTS)) return("time")
  if (starts(.DTM_FMTS)) return("datetime")
  if (starts(.DATE_FMTS)) return("date")
  NA_character_
}

ub_classify_columns <- function(df, formats, dcfg) {
  out <- character()
  excl <- toupper(.chr(dcfg$exclude_vars))
  by_fmt <- !isFALSE(dcfg$detect_by_format)
  by_name <- !isFALSE(dcfg$detect_by_name_suffix)
  for (col in names(df)) {
    cu <- toupper(col)
    if (length(excl) && any(vapply(excl, function(p) .glob_match(cu, p), logical(1)))) next
    x <- df[[col]]
    if (inherits(x, "hms") || inherits(x, "difftime")) next
    if (inherits(x, "POSIXct")) { out[col] <- "datetime"; next }
    if (inherits(x, "Date")) { out[col] <- "date"; next }
    is_num <- is.numeric(x)
    kind <- NA_character_
    if (is_num && by_fmt) {
      k <- ub_classify_format(formats[col])
      if (!is.na(k) && k %in% c("date", "datetime")) kind <- k
      else if (!is.na(k) && k == "time") next
    }
    if (is.na(kind) && by_name) {
      if (endsWith(cu, "DY") || (endsWith(cu, "TM") && !endsWith(cu, "DTM"))) next
      if (is_num && endsWith(cu, "DTM")) kind <- "datetime"
      else if (is_num && endsWith(cu, "DT")) kind <- "date"
      else if (!is_num && endsWith(cu, "DTC")) kind <- "dtc"
    }
    if (!is.na(kind)) out[col] <- kind
  }
  out
}

.ISO_FULL <- "^(\\d{4})-(\\d{2})-(\\d{2})(T.*)?$"
.ISO_PARTIAL <- "^\\d{4}(-(\\d{2}|-))?(-(\\d{2}|-))?(T.*)?$"

# returns list(value = chr, status = chr) for vectors
ub_shift_dtc <- function(x, days) {
  days <- rep_len(as.integer(days), length(x))
  val <- x; st <- rep("blank", length(x))
  v <- trimws(ifelse(is.na(x), "", x))
  full <- grepl(.ISO_FULL, v, perl = TRUE)
  if (any(full)) {
    d <- suppressWarnings(as.Date(substr(v[full], 1, 10), format = "%Y-%m-%d"))
    ok <- !is.na(d)
    idx <- which(full)
    val[idx[ok]] <- paste0(format(d[ok] + days[idx[ok]], "%Y-%m-%d"), substring(v[idx[ok]], 11))
    st[idx[ok]] <- "shifted"
    st[idx[!ok]] <- "invalid"
  }
  part <- !full & nzchar(v) & grepl(.ISO_PARTIAL, v, perl = TRUE)
  st[part] <- "partial"
  st[!full & !part & nzchar(v)] <- "invalid"
  list(value = val, status = st)
}

ub_shift_values <- function(x, days, kind) {
  days <- as.integer(days)
  if (inherits(x, "Date")) return(x + days)
  if (inherits(x, "POSIXct")) return(x + days * 86400)
  if (kind == "date") return(x + days)
  x + days * 86400
}

# numeric SAS day number for any date representation (used for checks/comparisons)
.as_sas_day <- function(x, kind) {
  if (inherits(x, "Date")) return(as.numeric(x - as.Date("1960-01-01")))
  if (inherits(x, "POSIXct")) return(floor((as.numeric(x) + 315619200) / 86400))
  if (kind == "datetime") return(floor(x / 86400))
  as.numeric(x)
}
