# Numeric helpers (same formulas and rounding fuzz as the reference implementation).

ub_round_half_away <- function(x, d) {
  y <- abs(x) * 10^d
  # fuzz capped at 1e-6: uncapped, 1e-9 * y exceeds 0.5 once y > 5e8 (e.g. an AUC with 6+ decimals)
  r <- floor(y + 0.5 + pmin(1e-9 * pmax(1, y), 1e-6)) / 10^d
  out <- sign(x) * r
  out[!is.na(out) & out == 0] <- 0
  out
}

ub_infer_decimals <- function(x, max_dec = 6L) {
  vapply(x, function(v) {
    if (is.na(v)) return(0L)
    for (d in 0:max_dec) if (abs(v - round(v, d)) <= 1e-9 * max(1, abs(v))) return(as.integer(d))
    as.integer(max_dec)
  }, 0L, USE.NAMES = FALSE)
}

ub_text_decimals <- function(x) {
  vapply(x, function(t) {
    if (is.na(t)) return(0L)
    m <- regmatches(t, gregexpr("\\d+\\.(\\d+)", t, perl = TRUE))[[1]]
    if (!length(m)) return(0L)
    nchar(sub("^\\d+\\.", "", m[length(m)]))
  }, 0L, USE.NAMES = FALSE)
}

.PLAIN_NUM <- "^\\s*[-+]?(\\d+(\\.\\d*)?|\\.\\d+)([eE][-+]?\\d+)?\\s*$"
# 'BQL<(x)' and the vendor spelling 'BLQ<(x)' (any case, blanks allowed); the prefix is kept when the text is rewritten
.BQL_RX <- "^\\s*(BQL|BLQ)\\s*<\\s*\\(\\s*([0-9.]+)\\s*\\)\\s*$"

ub_is_plain_number <- function(x) !is.na(x) & grepl(.PLAIN_NUM, x, perl = TRUE)
# TRUE for a BQL result text such as 'BQL<(1.00)' or 'BLQ<(0.500)' (vectorised; NA and non-text -> FALSE)
.ub_is_bql <- function(x) {
  if (!is.character(x)) return(rep(FALSE, length(x)))
  !is.na(x) & grepl(.BQL_RX, x, perl = TRUE, ignore.case = TRUE)
}
ub_is_bql <- function(x) .ub_is_bql(x)
# the LLOQ inside a BQL text (NA when the text is not one)
ub_bql_value <- function(x) {
  ok <- .ub_is_bql(x); out <- rep(NA_real_, length(x))
  out[ok] <- as.numeric(sub(.BQL_RX, "\\2", x[ok], perl = TRUE, ignore.case = TRUE))
  out
}
# 'BQL' or 'BLQ' as written in the BQL text (default 'BQL')
ub_bql_prefix <- function(x) {
  ok <- .ub_is_bql(x); out <- rep("BQL", length(x))
  out[ok] <- sub(.BQL_RX, "\\1", x[ok], perl = TRUE, ignore.case = TRUE)
  out
}

ub_format_fixed <- function(value, decimals) {
  out <- character(length(value))
  for (i in seq_along(value)) {
    v <- value[i]
    if (is.na(v)) { out[i] <- ""; next }
    d <- max(decimals[min(i, length(decimals))], 0)
    s <- formatC(ub_round_half_away(v, d), format = "f", digits = d)
    if (startsWith(s, "-") && as.numeric(s) == 0) s <- substring(s, 2)
    out[i] <- s
  }
  out
}

ub_make_bql_text <- function(lloq, decimals, prefix = "BQL") paste0(prefix, "<(", ub_format_fixed(lloq, decimals), ")")

.LEAD_NUM <- "[-+]?(\\d+(\\.\\d*)?|\\.\\d+)"

# multiply the number of a plain-number text by m, keeping its own decimals (+delta)
ub_scale_text <- function(txt, m, delta = 0L) {
  out <- txt
  ok <- ub_is_plain_number(txt) & !is.na(m)
  if (any(ok)) {
    d <- pmax(ub_text_decimals(txt[ok]) + delta, 0L)
    out[ok] <- ub_format_fixed(ub_round_half_away(as.numeric(txt[ok]) * m[ok], d), d)
  }
  out
}

# scale the first number inside a text such as '2.5 ng/mL' (decimals: own + delta)
ub_scale_lead_number <- function(txt, m, delta) {
  out <- txt
  for (i in seq_along(txt)) {
    t <- txt[i]
    if (is.na(t) || is.na(m[i])) next
    r <- regexpr(.LEAD_NUM, t, perl = TRUE)
    if (r < 0) next
    num <- regmatches(t, r)
    d <- max(ub_text_decimals(num) + delta, 0L)
    new <- ub_format_fixed(ub_round_half_away(as.numeric(num) * m[i], d), d)
    out[i] <- paste0(substr(t, 1, r - 1), new, substr(t, r + attr(r, "match.length"), nchar(t)))
  }
  out
}
