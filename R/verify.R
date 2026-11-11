#' Round-trip verification: source vs blind -> unblind
#' @param name dataset name
#' @param src,rt list(data, meta) of the source and the blind->unblind round-trip
#' @param cfg configuration
#' @export
ub_verify_dataset <- function(name, src, rt, cfg) {
  ds <- cfg$datasets[[name]]
  res <- list(dataset = name, status = "PASS", columns = list(), notes = character())
  s <- src$data; r <- rt$data
  if (!identical(names(s), names(r))) {
    res$status <- "FAIL"; res$notes <- paste("column sets differ:", paste(setdiff(union(names(s), names(r)), intersect(names(s), names(r))), collapse = ", "))
    return(res)
  }
  if (nrow(s) != nrow(r)) { res$status <- "FAIL"; res$notes <- "row counts differ"; return(res) }
  supp <- ub_suppressed_columns(names(s), ds, cfg)
  an <- ds$analyte_numeric
  approx <- .chr(an$variables); derived <- vapply(ds$derived_text, function(d) d$target, "")
  prec <- if (!is.null(an$precision_from) && an$precision_from %in% names(s)) as.character(s[[an$precision_from]]) else rep(NA_character_, nrow(s))
  blank_partial <- !identical(cfg$date_blinding$partial_dtc_action, "keep")
  num <- function(x) if (inherits(x, "Date")) as.numeric(x) else if (inherits(x, "POSIXct")) as.numeric(x) else if (inherits(x, "hms")) as.numeric(x) else x
  same <- function(a, b) {
    a <- num(a); b <- num(b)
    if (is.character(a) || is.character(b)) { a[is.na(a)] <- ""; b[is.na(b)] <- ""; return(as.character(a) == as.character(b)) }
    (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & abs(a - b) <= 1e-9 * pmax(1, abs(a)))
  }
  pn <- ds$param_numeric
  p_vars <- .chr(pn$variables); p_text <- .chr(pn$text_vars); d_vars <- .chr(an$derived_variables)
  p_prec <- if (!is.null(pn$precision_from) && pn$precision_from %in% names(s)) as.character(s[[pn$precision_from]]) else NULL
  maxdec <- if (is.null(cfg$numeric_blinding$max_inferred_decimals)) 6L else as.integer(cfg$numeric_blinding$max_inferred_decimals)
  for (c in names(s)) {
    if (c %in% supp) { res$columns[[c]] <- "suppressed"; next }
    a <- s[[c]]; b <- r[[c]]
    eq <- same(a, b)
    if (c %in% c(p_vars, p_text, d_vars)) {
      # re-scaled parameters / derived values: within one unit of the source's last decimal
      if (c %in% p_text) {
        pn2 <- ub_is_plain_number(a) & ub_is_plain_number(b)
        da <- ub_text_decimals(a)
        within <- !eq & pn2 & abs(round(suppressWarnings(as.numeric(a)) * 10^da) - round(suppressWarnings(as.numeric(b)) * 10^da)) <= 1
      } else {
        a <- num(a); b <- num(b)
        d <- if (c %in% p_vars && !is.null(p_prec)) ifelse(ub_is_plain_number(p_prec), ub_text_decimals(p_prec), ub_infer_decimals(a, maxdec))
             else ub_infer_decimals(a, maxdec)
        within <- !eq & !is.na(a) & !is.na(b) & abs(round(a * 10^d) - round(b * 10^d)) <= 1
      }
      within[is.na(within)] <- FALSE
      bad <- sum(!eq & !within)
      res$columns[[c]] <- list(exact = sum(eq), within_last_digit = sum(within), mismatch = bad)
      if (bad) res$status <- "FAIL"
      next
    }
    if (c %in% c(approx, derived)) {
      # no result text: the engine rounds to the source precision -> one unit of the value's own last decimal
      d <- ifelse(ub_is_plain_number(prec), ub_text_decimals(prec),
                  ifelse(ub_is_bql(prec) | !is.numeric(num(a)), 6L, ub_infer_decimals(suppressWarnings(as.numeric(num(a))), maxdec)))
      if (c %in% derived) {
        pn <- ub_is_plain_number(a) & ub_is_plain_number(b)
        d2 <- ifelse(ub_is_plain_number(prec), ub_text_decimals(prec), ub_text_decimals(a))
        within <- !eq & pn & abs(suppressWarnings(as.numeric(a)) - suppressWarnings(as.numeric(b))) <= 1.000001 * 10^(-d2)
      } else {
        within <- !eq & !is.na(a) & !is.na(b) & abs(round(a * 10^d) - round(b * 10^d)) <= 1
      }
      within[is.na(within)] <- FALSE
      bad <- sum(!eq & !within)
      res$columns[[c]] <- list(exact = sum(eq), within_last_digit = sum(within), mismatch = bad)
      if (bad) res$status <- "FAIL"
      next
    }
    if (all(eq)) next
    ia <- which(!eq)
    if (endsWith(toupper(c), "DTC") && blank_partial) {
      pa <- ia[b[ia] == "" & grepl(.ISO_PARTIAL, trimws(a[ia]), perl = TRUE)]
      if (length(pa)) { res$columns[[c]] <- list(partial_dates_blanked = length(pa)); ia <- setdiff(ia, pa) }
      if (!length(ia)) next
    }
    if (is.character(a) && all(toupper(a[ia]) == toupper(b[ia]))) {
      res$columns[[c]] <- list(case_only_differences = length(ia)); if (res$status == "PASS") res$status <- "WARN"
    } else { res$columns[[c]] <- list(mismatch = length(ia)); res$status <- "FAIL" }
  }
  if (!identical(toupper(substr(src$meta$file_label, 1, 40)), toupper(rt$meta$file_label))) { res$notes <- c(res$notes, "dataset label differs"); res$status <- "FAIL" }
  ls <- substr(src$meta$labels, 1, 40); lr <- rt$meta$labels[names(ls)]; lr[is.na(lr)] <- ""
  dl <- names(ls)[ls != lr]
  if (length(dl)) {
    res$notes <- c(res$notes, paste("labels differ for", paste(dl, collapse = ", ")))
    if (any(toupper(ls[dl]) != toupper(lr[dl]))) res$status <- "FAIL"
  }
  res
}
