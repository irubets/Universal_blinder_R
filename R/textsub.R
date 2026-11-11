# Case-preserving, word-bounded text substitution -- mirrors python/textsub.py.

ub_title_case <- function(s) {
  vapply(s, function(x) {
    if (is.na(x) || !nzchar(x)) return(x)
    ch <- strsplit(x, "", fixed = TRUE)[[1]]
    prev <- FALSE
    for (i in seq_along(ch)) {
      a <- grepl("^[A-Za-z]$", ch[i])
      if (a) ch[i] <- if (prev) tolower(ch[i]) else toupper(ch[i])
      prev <- a
    }
    paste(ch, collapse = "")
  }, "", USE.NAMES = FALSE)
}

ub_case_style <- function(s) {
  if (identical(s, toupper(s))) return("UPPER")
  if (identical(s, tolower(s))) return("LOWER")
  if (identical(s, ub_title_case(s))) return("TITLE")
  "MIXED"
}

ub_apply_style <- function(target, style) {
  switch(style, UPPER = toupper(target), LOWER = tolower(target), TITLE = ub_title_case(target), target)
}

.rx_escape <- function(x) gsub("([^A-Za-z0-9 ])", "\\\\\\1", x, perl = TRUE)

# rules: data.frame(find, replace, priority)
.order_rules <- function(rules) {
  if (!nrow(rules)) return(rules)
  rules <- rules[order(rules$priority), , drop = FALSE]
  rules <- rules[!duplicated(toupper(rules$find)), , drop = FALSE]
  rules[order(-nchar(rules$find), rules$priority, toupper(rules$find), rules$replace, method = "radix"), , drop = FALSE]
}

#' Build a case-preserving substitution function
#' @param rules data.frame with columns find, replace, priority
#' @param preserve_case FALSE: write the replacement exactly as given (codebook codes)
#' @return function(character vector) -> character vector
#' @keywords internal
ub_substituter <- function(rules, preserve_case = TRUE) {
  rules <- .order_rules(rules)
  pats <- paste0("(?<![A-Za-z0-9])", .rx_escape(rules$find), "(?![A-Za-z0-9])")
  one <- function(x) {
    if (is.na(x) || !nzchar(x)) return(x)
    for (i in seq_along(pats)) {
      m <- gregexpr(pats[i], x, perl = TRUE, ignore.case = TRUE)[[1]]
      if (m[1] == -1) next
      starts <- as.integer(m); lens <- attr(m, "match.length")
      out <- ""; pos <- 1L
      for (j in seq_along(starts)) {
        hit <- substr(x, starts[j], starts[j] + lens[j] - 1L)
        out <- paste0(out, substr(x, pos, starts[j] - 1L), if (preserve_case) ub_apply_style(rules$replace[i], ub_case_style(hit)) else rules$replace[i])
        pos <- starts[j] + lens[j]
      }
      x <- paste0(out, substr(x, pos, nchar(x)))
    }
    x
  }
  f <- function(v) {
    v <- as.character(v)
    u <- unique(v)
    r <- vapply(u, one, "", USE.NAMES = FALSE)
    r[match(v, u)]
  }
  attr(f, "rules") <- rules
  f
}
