#' Disclosure QC of blinded datasets
#'
#' Statuses: FAIL blocks release, WARN needs review, INFO is for information.
#' A FAIL can be waived (after a documented review) with `qc_waivers`.
#' @param study ub_study
#' @param keys ub_keys
#' @param blinded named list of list(data, meta) (blinded datasets)
#' @param blind_reports named list of engine reports from the blind step
#' @return data.frame of findings with attribute "status"
#' @export
ub_run_qc <- function(study, keys, blinded, blind_reports = list()) {
  cfg <- study$cfg; qcfg <- cfg$qc; nb <- cfg$numeric_blinding
  low <- nb$multiplier_low; high <- nb$multiplier_high
  F <- data.frame(status = character(), check = character(), dataset = character(), variable = character(),
                  detail = character(), public = character(), stringsAsFactors = FALSE)
  add <- function(status, check, dataset = "", variable = "", detail = "", public = NULL) {
    if (status == "FAIL") {
      tag <- if (nzchar(dataset)) paste0(check, ":", dataset, ".", variable) else check
      if (check %in% study$qc_waivers || tag %in% study$qc_waivers) status <- "WAIVED"
    }
    if (is.null(public)) public <- paste0(check, " (details in the private report)")
    F[nrow(F) + 1L, ] <<- c(status, check, dataset, variable, detail, public)
  }
  if (isTRUE(keys$manifest$seeded))
    add(if (study$allow_seeded_maps) "WARN" else "FAIL", "seeded_maps",
        detail = "Maps were built with a fixed seed; anyone who knows the seed and the data order can regenerate the key. Rebuild without a seed for any real study.")
  if (isTRUE(keys$manifest$weak_entropy))
    add("FAIL", "weak_entropy", detail = "Maps were built without a cryptographic random source (install the 'openssl' package and rebuild).")
  sf <- keys$t$subject_factors
  if (nrow(sf) && nchar(gsub("\\D", "", sub("e.*$", "", sf$FACTOR[1]))) < 10)
    add("FAIL", "coarse_factors", detail = "Stored factors have too few digits; a coarse factor grid makes blinded values factorable.")
  for (nm in names(blind_reports)) for (ck in blind_reports[[nm]]$checks) add(ck[[1]], ck[[2]], nm, "", ck[[3]], ck[[3]])

  t <- keys$terms <- ub_leak_terms(keys)   # grouping values (ARM, COHORTCD '1', ...) are coded, but they are not secrets
  alt <- function(x, bounded = TRUE) {
    x <- unique(x[nzchar(x)]); if (!length(x)) return(NULL)
    x <- x[order(-nchar(x))]
    body <- paste(.rx_escape(x), collapse = "|")
    if (bounded) paste0("(?<![A-Za-z0-9])(?:", body, ")(?![A-Za-z0-9])") else body
  }
  term_re <- alt(t$ORIG_TEXT)
  frag_re <- alt(t$ORIG_TEXT[t$ROLE %in% c("ALIAS", "CODE", "CODE_ALIAS", "TERM") & nchar(t$ORIG_TEXT) >= 3], FALSE)
  s <- keys$t$subjects
  idsub_re <- alt(c(s$USUBJID_ORIG, s$STUDYID_ORIG, unlist(lapply(keys$manifest$studyid_orig, as.character))), FALSE)
  subjid_re <- alt(s$SUBJID_ORIG[nchar(s$SUBJID_ORIG) >= 3])
  site_orig <- keys$t$sites$SITEID_ORIG
  alnum <- function(x) gsub("[^A-Z0-9]", "", toupper(x))
  name_terms <- unique(alnum(t$ORIG_TEXT)); name_terms <- name_terms[nchar(name_terms) >= 3]
  hit_ci <- function(re, x) if (is.null(re)) rep(FALSE, length(x)) else grepl(re, x, perl = TRUE, ignore.case = TRUE)
  hit_cs <- function(re, x) if (is.null(re)) rep(FALSE, length(x)) else grepl(re, x, perl = TRUE)
  date_re <- paste0("(?i)\\b(19|20)\\d{2}-\\d{2}-\\d{2}\\b|\\b\\d{1,2}[ -]?(JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC)[A-Z]*[ -]?(19|20)\\d{2}\\b|",
                    "\\b(JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC)[A-Z]* \\d{1,2},? (19|20)\\d{2}\\b|\\b\\d{1,2}/\\d{1,2}/(19|20)\\d{2}\\b|",
                    "(?<!\\d)(19|20)\\d{2}(0[1-9]|1[0-2])(0[1-9]|[12]\\d|3[01])(\\d{4})?(?!\\d)")

  for (nm in names(blinded)) {
    df <- blinded[[nm]]$data; meta <- blinded[[nm]]$meta; ds <- cfg$datasets[[nm]]
    ren <- keys$renames[[nm]]
    orig_names <- names(df); if (length(ren)) { b2o <- setNames(names(ren), ren); hit <- orig_names %in% names(b2o); orig_names[hit] <- b2o[orig_names[hit]] }
    supp_orig <- ub_suppressed_columns(orig_names, ds, cfg)
    supp_blind <- names(df)[orig_names %in% supp_orig]
    date_cols <- names(ub_classify_columns(df, meta$formats, cfg$date_blinding))
    for (c in setdiff(names(df), UB_RECID_VAR)) {          # UBRECID: random identifier added at release
      if (any(vapply(name_terms, function(x) grepl(x, alnum(c), fixed = TRUE), logical(1))))
        add("FAIL", "identity_in_varname", nm, c, paste0("variable name ", c, " contains an identity fragment"), "a variable name contains an identity fragment")
    }
    labs <- c("(dataset label)" = meta$file_label, meta$labels)
    for (i in seq_along(labs)) {
      l <- labs[[i]]; if (is.na(l) || !nzchar(l)) next
      if (hit_ci(term_re, l)) add("FAIL", "identity_in_label", nm, names(labs)[i], paste0("label '", l, "' contains an identity term"), "a label contains an identity term")
      if (hit_cs(idsub_re, l)) add("FAIL", "id_in_label", nm, names(labs)[i], paste0("label '", l, "' contains an original ID"))
    }
    rd <- ds$row_dates; rd_mask <- NULL
    if (!is.null(rd) && rd$name_var %in% names(df) && rd$value_var %in% names(df)) {
      q <- toupper(trimws(as.character(df[[rd$name_var]])))
      rd_mask <- vapply(q, function(x) any(vapply(toupper(.chr(rd$patterns)), function(p) .glob_match(x, p), logical(1))), logical(1), USE.NAMES = FALSE)
    }
    for (c in setdiff(names(df)[vapply(df, is.character, logical(1))], UB_RECID_VAR)) {
      u <- unique(df[[c]]); u <- u[!is.na(u) & nzchar(trimws(u))]
      if (!length(u)) next
      if (c %in% supp_blind) { add("FAIL", "suppressed_not_blank", nm, c, paste0(c, " should be blank but has ", length(u), " value(s)")); next }
      h <- u[hit_ci(term_re, u)]
      if (length(h)) add("FAIL", "residual_identity_text", nm, c, paste0(length(h), " distinct value(s) still contain an identity term, e.g. '", h[1], "'"),
                         paste0(length(h), " distinct value(s) still contain an identity term"))
      h <- u[hit_ci(frag_re, u) & !hit_ci(term_re, u)]
      if (length(h)) add("WARN", "identity_fragment", nm, c, paste0(length(h), " value(s) contain an alias/code glued to other characters, e.g. '", h[1], "'"),
                         paste0(length(h), " value(s) contain an identity fragment"))
      h <- u[hit_cs(idsub_re, u)]
      if (length(h)) add("FAIL", "residual_subject_id", nm, c, paste0(length(h), " value(s) contain an original STUDYID/USUBJID"),
                         paste0(length(h), " value(s) contain an original study/subject ID"))
      h <- u[hit_cs(subjid_re, u)]
      if (length(h)) add("FAIL", "residual_subjid", nm, c, paste0(length(h), " value(s) contain an original SUBJID, e.g. '", h[1], "'"),
                         paste0(length(h), " value(s) contain an original SUBJID"))
      if ((grepl("SITE", toupper(c)) || startsWith(toupper(c), "INV")) && length(site_orig)) {
        h <- u[trimws(u) %in% site_orig]
        if (length(h)) add("FAIL", "residual_siteid", nm, c, paste0(length(h), " original site value(s)"))
      }
      if (!isFALSE(qcfg$free_text_date_check) && !c %in% date_cols && !endsWith(toupper(c), "DTC")) {
        uu <- u
        if (!is.null(rd_mask) && c == rd$value_var) { uu <- unique(df[[c]][!rd_mask]); uu <- uu[!is.na(uu) & nzchar(trimws(uu))] }
        h <- uu[grepl(date_re, uu, perl = TRUE) & !ub_is_plain_number(uu)]   # '0.2012031' is a number
        if (length(h)) add("FAIL", "free_text_date", nm, c,
                           paste0(length(h), " value(s) look like they contain a calendar date, e.g. '", h[1], "' -- suppress the variable or waive after review"),
                           paste0(length(h), " value(s) look like they contain a calendar date"))
      }
    }
    uc <- ds$id_vars$usubjid
    # actual (mg) / planned (mg/m2) dose = the real body surface area, whatever the unit columns say
    for (dp in list(c("DOSEA", "DOSEP"), c("EXDOSE", "ECDOSE"))) {
      if (!all(dp %in% names(df))) next
      a <- suppressWarnings(as.numeric(df[[dp[1]]])); p <- suppressWarnings(as.numeric(df[[dp[2]]]))
      u <- if (uc %in% names(df)) as.character(df[[uc]]) else rep("", nrow(df))
      ok <- !is.na(a) & !is.na(p) & a > 0 & p > 0
      r <- a[ok] / p[ok]; h <- r >= 1.1 & r <= 3
      ns <- length(unique(u[ok][h]))
      if (ns >= 3) add("FAIL", "dose_ratio_reveals_bsa", nm, paste0(dp[1], "/", dp[2]),
                       paste0(sum(h), " record(s) of ", ns, " subjects have ", dp[1], "/", dp[2], " between 1.1 and 3.0: a dose per m2 next ",
                              "to the dose in mg gives the real body surface area. Blank the planned dose on those records (blank_where) or leave it out (drop_vars)"),
                       paste0(dp[1], "/", dp[2], " looks like a body surface area on records of ", ns, " subjects"))
    }
    if (uc %in% names(df)) { bad <- sum(!df[[uc]] %in% s$USUBJID_BLIND); if (bad) add("FAIL", "unblinded_usubjid", nm, uc, paste0(bad, " row(s) with a USUBJID that is not a blinded ID")) }
    sc <- ds$id_vars$siteid
    if (!is.null(sc) && sc %in% names(df) && !sc %in% supp_blind) {
      v <- setdiff(unique(trimws(df[[sc]])), ""); bad <- setdiff(v, keys$t$sites$SITEID_BLIND)
      if (length(bad)) add("FAIL", "unblinded_siteid", nm, sc, paste0(length(bad), " site value(s) are not blinded site IDs"))
    }
    extra <- if (is.null(nb$subject_extra_decimals)) 1L else as.integer(nb$subject_extra_decimals)
    for (var in intersect(.chr(ds$subject_numeric_vars), names(df))) {
      mx <- suppressWarnings(max(as.integer(sf$SRC_DECIMALS[sf$VARIABLE == var]))) + extra
      if (!is.finite(mx)) next
      v <- df[[var]]; v <- v[!is.na(v)]
      over <- sum(abs(v - round(v, mx)) > 1e-9 * pmax(1, abs(v)))
      if (over) add("FAIL", "excess_precision", nm, var, paste0(over, " value(s) carry more than ", mx, " decimals (full-precision products leak factors)"))
    }
    pn <- ds$param_numeric
    if (!is.null(pn) && !is.null(pn$precision_from) && pn$precision_from %in% names(df)) {
      prec <- as.character(df[[pn$precision_from]])
      for (var in intersect(.chr(pn$variables), names(df))) {
        v <- suppressWarnings(as.numeric(df[[var]]))
        ok <- !is.na(v) & ub_is_plain_number(prec)
        if (!any(ok)) next                                   # no result at all (e.g. PC delivered without results)
        d <- ub_text_decimals(prec[ok])
        over <- sum(abs(v[ok] - round(v[ok], d)) > 1e-9 * pmax(1, abs(v[ok])))
        if (over) add("FAIL", "excess_precision", nm, var, paste0(over, " value(s) carry more decimals than their result text"))
      }
    }
    an <- ds$analyte_numeric
    if (!is.null(an) && an$analyte_var %in% names(df)) {
      code <- as.character(df[[an$analyte_var]])
      prec <- if (!is.null(an$precision_from) && an$precision_from %in% names(df)) as.character(df[[an$precision_from]]) else NULL
      for (var in intersect(.chr(an$variables), names(df))) {
        v <- suppressWarnings(as.numeric(df[[var]]))
        if (!is.null(prec)) {
          ok <- !is.na(v) & ub_is_plain_number(prec)
          if (any(ok)) {                                     # no result at all (e.g. PC delivered without results)
            d <- ub_text_decimals(prec[ok])
            over <- sum(abs(v[ok] - round(v[ok], d)) > 1e-9 * pmax(1, abs(v[ok])))
            if (over) add("FAIL", "excess_precision", nm, var, paste0(over, " value(s) carry more decimals than their result text"))
          }
        }
        gmin <- if (is.null(qcfg$gcd_attack_min_records)) 5 else qcfg$gcd_attack_min_records
        g <- data.frame(u = df[[uc]], a = code, v = v, stringsAsFactors = FALSE)
        g <- g[!is.na(g$v) & g$v > 0, ]
        found <- 0
        for (grp in split(g$v, paste(g$u, g$a, sep = "\r"))) {
          if (length(grp) < gmin) next
          dmax <- max(ub_infer_decimals(grp, 9))
          ints <- round(grp * 10^dmax)
          if (any(ints > 2^52)) next
          G <- Reduce(function(a, b) { while (b > 0) { r <- a %% b; a <- b; b <- r }; a }, ints)
          gs <- sub("0+$", "", format(G, scientific = FALSE))
          if (nchar(gs) < 5) next
          if (any(G / 10^(0:15) >= low & G / 10^(0:15) <= high)) found <- found + 1
        }
        if (found) add("FAIL", "common_factor_attack", nm, var, paste0(found, " subject/analyte group(s) share an exact common factor (values are exact multiples)"))
      }
      for (lv in intersect(.chr(an$lloq_vars), names(df))) {
        tab <- data.frame(u = df[[uc]], a = code, l = suppressWarnings(as.numeric(df[[lv]])), stringsAsFactors = FALSE)
        tab <- tab[!is.na(tab$l), ]
        for (a in unique(tab$a)) {
          x <- tab[tab$a == a, ]
          ns <- length(unique(x$u)); nv <- length(unique(x$l))
          if (ns >= 3 && nv >= max(2, 0.5 * ns))
            add("FAIL", "lloq_varies_by_subject", nm, lv, paste0("LLOQ for one analyte takes ", nv, " values across ", ns, " subjects -- it would reveal per-subject factors"))
        }
      }
    }
  }
  qn <- if (is.null(qcfg$quasi_identifier_dataset)) "ADSL" else qcfg$quasi_identifier_dataset
  if (!is.null(blinded[[qn]])) {
    df <- blinded[[qn]]$data; uc <- cfg$datasets[[qn]]$id_vars$usubjid
    qi <- intersect(.chr(qcfg$quasi_identifiers), names(df)); k <- if (is.null(qcfg$k_threshold)) 5 else qcfg$k_threshold
    if (length(qi)) {
      sub <- df[!duplicated(df[[uc]]), c(uc, qi), drop = FALSE]
      key <- do.call(paste, c(lapply(sub[qi], as.character), sep = "\r"))
      cnt <- as.vector(table(key)[key])
      small <- sub[cnt < k, , drop = FALSE]
      if (nrow(small)) {
        ids <- sort(as.character(small[[uc]]))
        add("WARN", "k_anonymity", qn, paste(qi, collapse = "+"),
            paste0(nrow(small), " subject(s) fall in ", length(unique(key[cnt < k])), " quasi-identifier combination(s) with fewer than ", k,
                   " subjects: ", paste(utils::head(ids, 20), collapse = ", "), if (length(ids) > 20) " ..." else "",
                   ". Consider generalising (e.g. COUNTRY -> REGION)."),
            paste0(nrow(small), " subject(s) in quasi-identifier cells smaller than k=", k))
      } else add("INFO", "k_anonymity", qn, paste(qi, collapse = "+"), paste0("all quasi-identifier cells have >= ", k, " subjects"))
    }
  }
  st <- if (any(F$status == "FAIL")) "FAIL" else if (any(F$status == "WARN")) "WARN" else "PASS"
  attr(F, "status") <- st
  F
}

#' Write QC reports (private JSON/markdown and a shareable public summary)
#' @param F findings from [ub_run_qc()]
#' @param qc_dir output folder (PRIVATE)
#' @export
ub_write_qc_reports <- function(F, qc_dir) {
  dir.create(qc_dir, recursive = TRUE, showWarnings = FALSE)
  st <- attr(F, "status")
  writeLines(jsonlite::toJSON(list(status = st, findings = F), auto_unbox = TRUE, pretty = TRUE, dataframe = "rows"),
             file.path(qc_dir, "qc_report.json"))
  ord <- order(match(F$status, c("FAIL", "WAIVED", "WARN", "INFO")), F$dataset, F$check)
  G <- F[ord, , drop = FALSE]
  cnt <- table(G$status)
  head <- paste0("**Overall: ", st, "** -- ", paste(paste0(names(cnt), ": ", as.integer(cnt)), collapse = ", "))
  tab <- function(field) {
    var <- G$variable; if (field == "public") var[G$check == "identity_in_varname"] <- "(hidden)"
    c("| Status | Check | Dataset | Variable | Detail |", "|---|---|---|---|---|",
      sprintf("| %s | %s | %s | %s | %s |", G$status, G$check, G$dataset, var, gsub("|", "\\|", G[[field]], fixed = TRUE)))
  }
  writeLines(c("# Blinding QC report (PRIVATE -- may quote original values)", "", head, "", tab("detail")), file.path(qc_dir, "qc_report.md"))
  writeLines(c("# Blinding QC summary", "", head, "", tab("public")), file.path(qc_dir, "qc_summary_public.md"))
  invisible(st)
}
