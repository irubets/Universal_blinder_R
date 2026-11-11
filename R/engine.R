#' Blind or unblind one dataset
#'
#' Pure function: no files are read or written. Order of operations (blind): subject keys,
#' suppression, per-subject covariates, per-subject-per-analyte values, LLOQ, result text, PK parameters,
#' exact swaps, grouping values, free text and labels, dates, identifiers, renames; unblind runs the inverse.
#'
#' @param df data.frame (source for "blind", blinded for "unblind")
#' @param meta list(file_label, labels, formats) as returned by [ub_read_dataset()]
#' @param name dataset name as used in the configuration (e.g. "ADNCA")
#' @param cfg configuration
#' @param keys key from [ub_load_keys()] / [ub_build_keys()]
#' @param direction "blind" or "unblind"
#' @return list(data, meta, report)
#' @export
ub_transform <- function(df, meta, name, cfg, keys, direction = c("blind", "unblind")) {
  direction <- match.arg(direction)
  fwd <- direction == "blind"
  ds <- cfg$datasets[[name]]
  if (is.null(ds)) ub_error("No configuration for dataset ", name)
  nb <- cfg$numeric_blinding; dcfg <- cfg$date_blinding
  report <- list(dataset = name, direction = direction, rows = nrow(df), notes = character(), checks = list())
  add_check <- function(status, check, msg) report$checks[[length(report$checks) + 1L]] <<- list(status, check, msg)
  nbl <- function(x) is.na(x) | !nzchar(trimws(as.character(x)))

  # ---- 0. renames back (unblind) ----
  ren <- keys$renames[[name]]
  if (!fwd && length(ren)) {
    back <- setNames(names(ren), ren); back <- back[names(back) %in% names(df)]
    names(df)[match(names(back), names(df))] <- unname(back)
    names(meta$labels)[match(names(back), names(meta$labels))] <- unname(back)
    names(meta$formats)[match(names(back), names(meta$formats))] <- unname(back)
  }
  idv <- ds$id_vars; ucol <- idv$usubjid
  if (!ucol %in% names(df)) ub_error(name, ": ", ucol, " not found")

  # ---- 1. subject ids ----
  u_in <- trimws(as.character(df[[ucol]]))
  if (fwd) {
    unknown <- setdiff(unique(u_in), keys$subj_fwd_key); orig_u <- u_in
  } else {
    unknown <- setdiff(unique(u_in), keys$subj_rev_key)
    orig_u <- keys$subj_rev$USUBJID_ORIG[match(u_in, keys$subj_rev_key)]
  }
  if (length(unknown)) ub_error(name, ": ", length(unknown), " ", ucol, " value(s) not in the subject map (rebuild maps)")

  # ---- 2. suppression ----
  supp <- ub_suppressed_columns(names(df), ds, cfg)
  if (fwd) for (c in supp) df[[c]] <- if (is.character(df[[c]])) rep("", nrow(df)) else {
    x <- df[[c]]; x[] <- NA; x }
  report$suppressed <- supp

  # ---- 3. per-subject covariates ----
  extra <- if (is.null(nb$subject_extra_decimals)) 1L else as.integer(nb$subject_extra_decimals)
  for (var in .chr(ds$subject_numeric_vars)) {
    if (!var %in% names(df) || var %in% supp) next
    v <- suppressWarnings(as.numeric(df[[var]]))
    k <- .k2(orig_u, var)
    f <- unname(keys$subj_factor[k]); d <- unname(keys$subj_dec[k])
    miss <- !is.na(v) & is.na(f)
    if (any(miss)) ub_error(name, ".", var, ": ", sum(miss), " value(s) have no factor in the key (rebuild maps)")
    out <- v
    ok <- !is.na(v)
    out[ok] <- if (fwd) ub_round_half_away(v[ok] * f[ok], d[ok] + extra) else ub_round_half_away(v[ok] / f[ok], d[ok])
    df[[var]] <- out
    if (fwd) {
      ps <- unique(data.frame(u = orig_u[ok], v = v[ok]))
      if (length(unique(ps$u)) >= 3 && length(unique(ps$v)) == 1)
        add_check("FAIL", "constant_scaled_var", paste0(name, ".", var, " has the same value for every subject; per-subject scaling of a study constant reveals each subject's factor"))
    }
  }

  # ---- 4/5/6. analyte values, LLOQ, derived text ----
  an <- ds$analyte_numeric
  if (!is.null(an) && an$analyte_var %in% names(df)) {
    cm <- ub_analyte_key_map(keys, direction)
    ac <- trimws(as.character(df[[an$analyte_var]]))
    akey <- unname(cm[ac]); akey[!nzchar(ac)] <- NA
    ptxt <- if (!is.null(an$precision_from) && an$precision_from %in% names(df)) as.character(df[[an$precision_from]]) else rep(NA_character_, nrow(df))
    maxdec <- if (is.null(nb$max_inferred_decimals)) 6L else as.integer(nb$max_inferred_decimals)
    bql_blank <- !identical(nb$bql_imputed_action, "flag")
    imputed <- character()   # subject/analyte/value of BQL-imputed results, for derived values copied from them
    for (var in .chr(an$variables)) {
      if (!var %in% names(df) || var %in% supp) next
      v <- suppressWarnings(as.numeric(df[[var]]))
      f <- unname(keys$analyte_factor[.k2(orig_u, akey)]); f[is.na(akey)] <- NA
      nz <- !is.na(v) & v != 0
      if (any(nz & is.na(f))) ub_error(name, ".", var, ": ", sum(nz & is.na(f)), " non-missing value(s) have no subject/analyte factor (unknown ", an$analyte_var, " or subject -- rebuild maps or add an alias)")
      # rows without a usable result text share ONE precision (the largest among them)
      nt <- nz & !ub_is_plain_number(ptxt) & !ub_is_bql(ptxt)
      d_col <- if (any(nt)) max(ub_infer_decimals(v[nt], maxdec)) else 0L
      d <- ifelse(ub_is_plain_number(ptxt), ub_text_decimals(ptxt), d_col)
      # value imputed from the LLOQ on a BQL record: one decimal more than the BQL text (+ LLOQ extra on blind)
      bq <- ub_is_bql(ptxt) & nz
      lx0 <- if (is.null(nb$lloq_extra_decimals)) 1L else as.integer(nb$lloq_extra_decimals)
      d[bq] <- ub_text_decimals(ptxt[bq]) + 1L + (if (fwd) lx0 else 0L)
      blank_bq <- fwd && bql_blank && any(bq)
      if (fwd && any(bq) && !bql_blank)
        add_check("FAIL", "bql_imputed_value", paste0(name, ".", var, ": ", sum(bq), " BQL record(s) carry a non-zero value (e.g. LLOQ/2). ",
                  "Next to the blinded LLOQ it reveals each subject's factor; set these values to 0 or missing in the transfer copy, or waive after review"))
      go <- if (blank_bq) nz & !bq else nz
      out <- v
      out[go] <- if (fwd) ub_round_half_away(v[go] * f[go], d[go]) else ub_round_half_away(v[go] / f[go], d[go])
      out[!is.na(v) & v == 0] <- 0
      if (blank_bq) {
        # BQL-imputed value (e.g. LLOQ/2): next to the blinded LLOQ it would reveal the subject's factor --
        # released as missing, together with the values derived from it (the original comes back on retrieval)
        imputed <- c(imputed, .k2(.k2(orig_u[bq], akey[bq]), sprintf("%.17g", v[bq])))
        out[bq] <- NA
        report$notes <- c(report$notes, paste0(var, ": ", sum(bq), " BQL-imputed value(s) released as missing"))
        for (dv in .chr(an$bql_dependent_vars[[var]])) if (dv %in% names(df)) {
          if (is.character(df[[dv]])) df[[dv]][bq] <- "" else df[[dv]][bq] <- NA
        }
      }
      df[[var]] <- out
      if (fwd && any(nz)) {
        g <- unique(data.frame(k = akey[nz], u = orig_u[nz], v = v[nz]))
        for (kk in unique(g$k)) {
          s <- g[g$k == kk, ]
          if (length(unique(s$u)) >= 3 && length(unique(s$v)) == 1)
            add_check("FAIL", "constant_scaled_var", paste0(name, ".", var, ": one analyte has the same value for every subject; per-subject scaling of it reveals each subject's factor"))
        }
      }
    }
    lx <- if (is.null(nb$lloq_extra_decimals)) 1L else as.integer(nb$lloq_extra_decimals)
    for (var in .chr(an$lloq_vars)) {
      if (!var %in% names(df) || var %in% supp) next
      v <- suppressWarnings(as.numeric(df[[var]]))
      g <- unname(keys$lloq_factor[akey]); d <- unname(keys$lloq_dec[akey])
      miss <- !is.na(v) & is.na(g)
      if (any(miss)) ub_error(name, ".", var, ": ", sum(miss), " value(s) have no LLOQ factor (rebuild maps)")
      ok <- !is.na(v)
      out <- v
      out[ok] <- if (fwd) ub_round_half_away(v[ok] * g[ok], d[ok] + lx) else ub_round_half_away(v[ok] / g[ok], d[ok])
      df[[var]] <- out
    }
    # derived results of the same analyte (baseline, change, peak): same factor, one precision per column
    for (var in .chr(an$derived_variables)) {
      if (!var %in% names(df) || var %in% supp) next
      v <- suppressWarnings(as.numeric(df[[var]]))
      d <- if (any(!is.na(v))) max(ub_infer_decimals(v[!is.na(v)], maxdec)) else 0L
      f <- unname(keys$analyte_factor[.k2(orig_u, akey)]); f[is.na(akey)] <- NA
      nz <- !is.na(v) & v != 0
      if (any(nz & is.na(f))) ub_error(name, ".", var, ": ", sum(nz & is.na(f)), " value(s) have no subject/analyte factor (rebuild maps)")
      dep <- if (fwd && length(imputed)) nz & .k2(.k2(orig_u, akey), sprintf("%.17g", v)) %in% imputed else rep(FALSE, length(v))
      go <- nz & !dep
      out <- v
      out[go] <- if (fwd) ub_round_half_away(v[go] * f[go], d) else ub_round_half_away(v[go] / f[go], d)
      if (any(dep)) {                       # e.g. ABASE copied from a BQL-imputed baseline
        out[dep] <- NA
        report$notes <- c(report$notes, paste0(var, ": ", sum(dep), " value(s) derived from BQL-imputed results released as missing"))
        for (dv in .chr(an$bql_dependent_vars[[var]])) if (dv %in% names(df)) {
          if (is.character(df[[dv]])) df[[dv]][dep] <- "" else df[[dv]][dep] <- NA
        }
      }
      df[[var]] <- out
    }
    # LLOQ held as text ('2.5 ng/mL'): one factor per analyte, one extra decimal
    for (var in .chr(an$lloq_text_vars)) {
      if (!var %in% names(df) || var %in% supp || !is.character(df[[var]])) next
      tx <- df[[var]]
      has <- !is.na(tx) & grepl(.LEAD_NUM, tx, perl = TRUE)
      g <- unname(keys$lloq_factor[akey])
      if (any(has & is.na(g))) ub_error(name, ".", var, ": ", sum(has & is.na(g)), " value(s) have no LLOQ factor (rebuild maps)")
      df[[var]] <- ub_scale_lead_number(tx, if (fwd) g else 1 / g, if (fwd) lx else -lx)
    }
    cache <- list()
    for (sp in ds$derived_text) for (c in c(sp$target, sp$bql_ref)) if (c %in% names(df) && is.null(cache[[c]])) cache[[c]] <- as.character(df[[c]])
    for (sp in ds$derived_text) {
      tgt <- sp$target; src <- sp$source; lv <- sp$lloq_var
      if (!tgt %in% names(df) || !src %in% names(df) || tgt %in% supp) next
      rtx <- cache[[sp$bql_ref]]; own <- cache[[tgt]]
      num <- suppressWarnings(as.numeric(df[[src]]))
      new <- own
      bq <- if (is.null(rtx)) rep(FALSE, nrow(df)) else ub_is_bql(rtx)
      if (any(bq)) {
        dec <- ub_text_decimals(rtx[bq])
        lval <- if (!is.null(lv) && lv %in% names(df)) suppressWarnings(as.numeric(df[[lv]][bq])) else rep(NA_real_, sum(bq))
        need <- is.na(lval)
        if (any(need)) {
          gk <- unname(keys$lloq_factor[akey[bq][need]])
          if (any(is.na(gk))) ub_error(name, ".", tgt, ": BQL text without LLOQ factor")
          base <- ub_bql_value(rtx[bq][need])
          lval[need] <- if (fwd) ub_round_half_away(base * gk, dec[need]) else ub_round_half_away(base / gk, dec[need])
        }
        new[bq] <- ub_make_bql_text(lval, dec, ub_bql_prefix(rtx[bq]))   # BQL or BLQ, as written
      }
      nb2 <- !bq & !is.na(num) & ub_is_plain_number(own)
      if (any(nb2)) new[nb2] <- ub_format_fixed(num[nb2], ub_text_decimals(own[nb2]))
      df[[tgt]] <- new
    }
  }

  # ---- 6b. parameters (VS body size, PK parameters): factor chosen per parameter code ----
  pn <- ds$param_numeric
  if (!is.null(pn) && any(c(.chr(pn$variables), .chr(pn$text_vars)) %in% names(df))) {
    if (!pn$param_var %in% names(df)) ub_error(name, ": ", pn$param_var, " is needed to decide how each parameter is blinded")
    codes <- trimws(as.character(df[[pn$param_var]]))
    cl <- ub_param_class(pn, codes, cfg)
    unk <- sort(unique(codes[cl$kind == "unknown" & nzchar(codes)]), method = "radix")
    if (length(unk)) ub_error(name, ".", pn$param_var, ": parameter(s) ", paste(unk, collapse = ", "), " are not classified in ",
                              "param_scaling_tables.", pn$table, " (scale / inverse / unscaled / own_factor) -- add them to the ",
                              "configuration (config_overrides) before blinding")
    mult <- rep(NA_real_, nrow(df))
    sub_k <- cl$kind %in% c("subject", "own")
    if (any(sub_k)) mult[sub_k] <- unname(keys$subj_factor[.k2(orig_u[sub_k], cl$key[sub_k])])
    an_k <- cl$kind %in% c("scale", "inverse")
    if (any(an_k)) {
      av <- pn$analyte_var
      if (is.null(av) || !av %in% names(df)) ub_error(name, ": ", av, " is needed to scale ", pn$param_var, " parameters")
      lut <- ub_analyte_lookup(keys, direction)
      ak <- unname(lut[toupper(trimws(as.character(df[[av]])))])
      f <- unname(keys$analyte_factor[.k2(orig_u, ak)]); f[is.na(ak)] <- NA
      mult[an_k] <- ifelse(cl$kind[an_k] == "inverse", 1 / f[an_k], f[an_k])
    }
    scaled <- cl$kind %in% c("subject", "own", "scale", "inverse")
    if (any(scaled & is.na(mult))) {
      has <- rep(FALSE, nrow(df))
      for (var in intersect(.chr(pn$variables), names(df))) has <- has | !is.na(suppressWarnings(as.numeric(df[[var]])))
      for (var in intersect(.chr(pn$text_vars), names(df))) has <- has | ub_is_plain_number(as.character(df[[var]]))
      n <- sum(scaled & is.na(mult) & has)
      if (n) ub_error(name, ": ", n, " value(s) of scaled parameters have no factor in the key ",
                      "(unknown subject/analyte -- rebuild maps or add an analyte alias)")
    }
    if (!fwd) mult <- 1 / mult
    pt <- if (!is.null(pn$precision_from) && pn$precision_from %in% names(df)) as.character(df[[pn$precision_from]]) else rep(NA_character_, nrow(df))
    maxdec <- if (is.null(nb$max_inferred_decimals)) 6L else as.integer(nb$max_inferred_decimals)
    for (var in .chr(pn$variables)) {
      if (!var %in% names(df) || var %in% supp) next
      v <- suppressWarnings(as.numeric(df[[var]]))
      go <- !is.na(mult) & !is.na(v) & v != 0
      nt <- go & !ub_is_plain_number(pt)
      d_col <- if (any(nt)) max(ub_infer_decimals(v[nt], maxdec)) else 0L
      d <- ifelse(ub_is_plain_number(pt), ub_text_decimals(pt), d_col)
      out <- v
      out[go] <- ub_round_half_away(v[go] * mult[go], d[go])
      df[[var]] <- out
    }
    for (var in .chr(pn$text_vars)) {
      if (!var %in% names(df) || var %in% supp || !is.character(df[[var]])) next
      df[[var]] <- ub_scale_text(df[[var]], mult)
    }
  }

  # ---- 7. exact swaps ----
  for (var in names(ds$exact_vars)) {
    if (!var %in% names(df) || var %in% supp) next
    m <- ub_exact_map(keys, ds$exact_vars[[var]], direction)
    x <- as.character(df[[var]])
    bad <- unique(x[!nbl(x) & !x %in% names(m)])
    if (length(bad)) ub_error(name, ".", var, ": ", length(bad), " value(s) are not in the key for role '",
                              ds$exact_vars[[var]], "' (rebuild maps; a new treatment/analyte appeared)")
    hit <- x %in% names(m)
    x[hit] <- unname(m[x[hit]])
    df[[var]] <- x
  }

  # ---- 7b. grouping values (codebook keys): the whole value is replaced by its code ----
  coded <- character()
  if (isTRUE(keys$manifest$codebook)) {
    cv <- .ub_cb_category_vars(cfg)
    for (c in names(df)) {
      g <- if (toupper(c) %in% names(cv)) cv[[toupper(c)]] else NA_character_
      if (is.na(g) || c %in% supp || !is.character(df[[c]])) next
      m <- ub_category_map(keys, g, direction)
      x <- trimws(df[[c]])
      kp <- .chr(keys$manifest$category_keep[[g]])
      bad <- unique(x[!nbl(x) & !x %in% names(m) & !x %in% kp])
      if (length(bad) && fwd) ub_error(name, ".", c, ": ", length(bad), " ", g, " value(s) are not in the codebook ",
                                       "(a new value appeared -- run again to add it)")
      hit <- !is.na(x) & x %in% names(m)
      x[hit] <- unname(m[x[hit]])
      df[[c]] <- x
      coded <- c(coded, c)
    }
  }

  # ---- 8. free text + labels ----
  subs <- list()
  get_sub <- function(ns) {
    if (is.null(subs[[ns]])) {
      tt <- ub_text_terms(keys, ns)
      rules <- if (fwd) data.frame(find = tt$orig, replace = tt$blind, priority = tt$priority, stringsAsFactors = FALSE)
               else data.frame(find = tt$blind, replace = tt$orig, priority = tt$priority, stringsAsFactors = FALSE)
      subs[[ns]] <<- ub_substituter(rules, ub_preserve_case(keys))
    }
    subs[[ns]]
  }
  for (var in names(ds$text_vars)) {
    if (!var %in% names(df) || var %in% supp || var %in% coded || !is.character(df[[var]])) next
    df[[var]] <- get_sub(ds$text_vars[[var]])(df[[var]])
  }
  sa <- get_sub("any")
  if (length(meta$file_label) && nzchar(meta$file_label)) meta$file_label <- sa(meta$file_label)
  if (length(meta$labels)) meta$labels <- setNames(sa(unname(meta$labels)), names(meta$labels))

  # ---- 9. dates ----
  if (!isFALSE(dcfg$enabled)) {
    kinds <- ub_classify_columns(df, meta$formats, dcfg)
    kinds <- kinds[!names(kinds) %in% supp]
    study_const <- toupper(.chr(dcfg$study_constant_date_vars))
    per_study <- identical(dcfg$mode, "per_study")
    if (is.na(keys$shift[UB_STUDY_KEY])) ub_error("Date-shift key has no study-level offset (rebuild maps)")
    s_study <- unname(keys$shift[UB_STUDY_KEY])
    sgn <- if (fwd) 1L else -1L
    subj_shift <- unname(keys$shift[orig_u])
    if (any(is.na(subj_shift)) && !per_study) ub_error(name, ": ", sum(is.na(subj_shift)), " row(s) have no date shift (rebuild maps)")
    counts <- c(shifted = 0, partial = 0, invalid = 0)
    blank_partial <- fwd && !identical(dcfg$partial_dtc_action, "keep")
    for (col in names(kinds)) {
      kind <- kinds[[col]]
      use_study <- per_study || toupper(col) %in% study_const
      sh <- if (use_study) rep(s_study, nrow(df)) else subj_shift
      if (fwd && !use_study) report <- .check_shared_dates(df[[col]], orig_u, kind, name, col, dcfg, report)
      if (kind == "dtc") {
        r <- ub_shift_dtc(as.character(df[[col]]), sgn * sh)
        val <- r$value
        if (blank_partial) val[r$status == "partial"] <- ""
        df[[col]] <- val
        for (s in names(counts)) counts[s] <- counts[s] + sum(r$status == s)
      } else {
        x <- df[[col]]
        df[[col]] <- ub_shift_values(x, sgn * sh, kind)
        counts["shifted"] <- counts["shifted"] + sum(!is.na(x))
      }
    }
    rd <- ds$row_dates
    if (!is.null(rd) && rd$name_var %in% names(df) && rd$value_var %in% names(df) && !rd$value_var %in% supp) {
      q <- toupper(trimws(as.character(df[[rd$name_var]])))
      isd <- vapply(q, function(x) any(vapply(toupper(.chr(rd$patterns)), function(p) .glob_match(x, p), logical(1))), logical(1), USE.NAMES = FALSE)
      if (any(isd)) {
        sh <- if (per_study) rep(s_study, nrow(df)) else subj_shift
        val <- as.character(df[[rd$value_var]])
        r <- ub_shift_dtc(val[isd], sgn * sh[isd])
        nv <- r$value
        if (blank_partial) nv[r$status == "partial"] <- ""
        val[isd] <- nv
        df[[rd$value_var]] <- val
        for (s in names(counts)) counts[s] <- counts[s] + sum(r$status == s)
      }
    }
    report$date_columns <- kinds
    report$date_counts <- as.list(counts)
    if (counts[["partial"]] > 0 && fwd) {
      if (blank_partial) report$notes <- c(report$notes, paste0(counts[["partial"]], " partial ISO date(s) blanked (cannot be shifted)"))
      else add_check("WARN", "partial_dates_unshifted", paste0(name, ": ", counts[["partial"]], " partial ISO date(s) (year or year-month) were left unshifted and show real calendar information"))
    }
    if (counts[["invalid"]] > 0) add_check("WARN", "unparsed_dtc", paste0(name, ": ", counts[["invalid"]], " --DTC value(s) could not be parsed and were left as is"))
  }

  # ---- 10. identifiers ----
  sc <- idv$siteid
  if (!is.null(sc) && sc %in% names(df) && !sc %in% supp) {
    m <- if (fwd) keys$site_fwd else keys$site_rev
    x <- trimws(as.character(df[[sc]]))
    bad <- unique(x[nzchar(x) & !x %in% names(m)])
    if (length(bad)) ub_error(name, ".", sc, ": ", length(bad), " site value(s) not in the site map (rebuild maps)")
    x[nzchar(x)] <- unname(m[x[nzchar(x)]])
    df[[sc]] <- x
  }
  if (fwd) { i <- match(u_in, keys$subj_fwd_key); lut <- keys$subj_fwd; cols <- c("STUDYID_BLIND", "USUBJID_BLIND", "SUBJID_BLIND") }
  else { i <- match(u_in, keys$subj_rev_key); lut <- keys$subj_rev; cols <- c("STUDYID_ORIG", "USUBJID_ORIG", "SUBJID_ORIG") }
  if (!is.null(idv$studyid) && idv$studyid %in% names(df)) {
    df[[idv$studyid]] <- lut[[cols[1]]][i]
    so <- keys$manifest$studyid_orig[[name]]
    if (!fwd && !is.null(so)) df[[idv$studyid]] <- rep(as.character(so), nrow(df))   # SDTM and ADaM may differ
  }
  if (!is.null(idv$subjid) && idv$subjid %in% names(df)) df[[idv$subjid]] <- lut[[cols[3]]][i]
  df[[ucol]] <- lut[[cols[2]]][i]

  # ---- 11. renames (blind) ----
  if (fwd && length(ren)) {
    present <- ren[names(ren) %in% names(df)]
    missing <- setdiff(names(ren), names(df))
    if (length(missing)) report$notes <- c(report$notes, paste0("rename_vars: ", paste(missing, collapse = ", "), " not present"))
    clash <- setdiff(intersect(unname(present), names(df)), names(present))
    if (length(clash)) ub_error(name, ": rename target(s) ", paste(clash, collapse = ", "), " already exist")
    names(df)[match(names(present), names(df))] <- unname(present)
    names(meta$labels)[match(names(present), names(meta$labels))] <- unname(present)
    names(meta$formats)[match(names(present), names(meta$formats))] <- unname(present)
  }
  report$renamed <- ren
  list(data = df, meta = meta, report = report)
}

.check_shared_dates <- function(x, orig_u, kind, name, col, dcfg, report) {
  v <- if (kind == "dtc") { y <- as.character(x); ifelse(!is.na(y) & nchar(y) >= 10, substr(y, 1, 10), NA) } else .as_sas_day(x, kind)
  ok <- !is.na(v)
  p <- unique(data.frame(u = orig_u[ok], v = v[ok], stringsAsFactors = FALSE))
  n_subj <- length(unique(p$u))
  if (n_subj < 3) return(report)
  if (length(unique(p$v)) == 1) {
    report$checks[[length(report$checks) + 1L]] <- list("FAIL", "constant_shifted_date",
      paste0(name, ".", col, " is the same calendar date for every subject; add it to date_blinding.study_constant_date_vars (or suppress it)"))
    return(report)
  }
  if (max(table(p$u)) > 1) return(report)
  top <- max(table(p$v))
  frac <- top / n_subj
  min_n <- if (is.null(dcfg$shared_date_min_subjects)) 5 else dcfg$shared_date_min_subjects
  thr <- if (is.null(dcfg$shared_date_warn_fraction)) 0.25 else dcfg$shared_date_warn_fraction
  if (top >= min_n && frac >= thr)
    report$checks[[length(report$checks) + 1L]] <- list("WARN", "shared_shifted_date",
      paste0(name, ".", col, ": ", round(100 * frac), "% of subjects share one calendar date (e.g. last-known-alive = cut-off); shifting it per subject reveals the relative offsets of those subjects. Consider suppressing it or waiving after review"))
  report
}
