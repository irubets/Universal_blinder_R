# Blinding key ("maps"): build, save, load. Plain CSV files plus maps_manifest.json
# (see docs/map_format.md).

UB_MAP_FILES <- c(subjects = "replace_subjid.csv", sites = "replace_siteid.csv", terms = "replace_terms.csv",
                  subject_factors = "factors_subject_numeric.csv", analyte_factors = "factors_analyte_numeric.csv",
                  lloq_factors = "factors_lloq.csv", date_shifts = "shift_dates.csv", renames = "rename_vars.csv")
UB_MAP_COLS <- list(
  subjects = c("STUDYID_ORIG", "USUBJID_ORIG", "SUBJID_ORIG", "STUDYID_BLIND", "USUBJID_BLIND", "SUBJID_BLIND"),
  sites = c("SITEID_ORIG", "SITEID_BLIND"),
  terms = c("TYPE", "ROLE", "IDENTITY_KEY", "SEQ", "ORIG_TEXT", "BLIND_TEXT"),
  subject_factors = c("USUBJID_ORIG", "VARIABLE", "FACTOR", "SRC_DECIMALS"),
  analyte_factors = c("USUBJID_ORIG", "ANALYTE_KEY", "FACTOR"),
  lloq_factors = c("ANALYTE_KEY", "FACTOR", "SRC_DECIMALS"),
  date_shifts = c("USUBJID_ORIG", "SHIFT_DAYS"),
  renames = c("DATASET", "VAR_ORIG", "VAR_BLIND"))
UB_STUDY_KEY <- "*STUDY*"

.empty_tbl <- function(cols) as.data.frame(setNames(replicate(length(cols), character(), simplify = FALSE), cols),
                                           stringsAsFactors = FALSE)
.k2 <- function(a, b) paste(a, b, sep = "\r")

.new_keys <- function(t, manifest) {
  for (k in names(UB_MAP_COLS)) if (is.null(t[[k]])) t[[k]] <- .empty_tbl(UB_MAP_COLS[[k]])
  s <- t$subjects; st <- t$sites; sf <- t$subject_factors; af <- t$analyte_factors
  lf <- t$lloq_factors; ds <- t$date_shifts; rn <- t$renames
  keys <- list(
    t = t, manifest = manifest,
    subj_fwd = s[, c("STUDYID_BLIND", "USUBJID_BLIND", "SUBJID_BLIND")], subj_fwd_key = s$USUBJID_ORIG,
    subj_rev = s[, c("STUDYID_ORIG", "USUBJID_ORIG", "SUBJID_ORIG")], subj_rev_key = s$USUBJID_BLIND,
    site_fwd = setNames(st$SITEID_BLIND, st$SITEID_ORIG), site_rev = setNames(st$SITEID_ORIG, st$SITEID_BLIND),
    subj_factor = setNames(as.numeric(sf$FACTOR), .k2(sf$USUBJID_ORIG, sf$VARIABLE)),
    subj_dec = setNames(as.integer(sf$SRC_DECIMALS), .k2(sf$USUBJID_ORIG, sf$VARIABLE)),
    analyte_factor = setNames(as.numeric(af$FACTOR), .k2(af$USUBJID_ORIG, af$ANALYTE_KEY)),
    lloq_factor = setNames(as.numeric(lf$FACTOR), lf$ANALYTE_KEY),
    lloq_dec = setNames(as.integer(lf$SRC_DECIMALS), lf$ANALYTE_KEY),
    shift = setNames(as.integer(ds$SHIFT_DAYS), ds$USUBJID_ORIG),
    renames = split(setNames(rn$VAR_BLIND, rn$VAR_ORIG), rn$DATASET))
  structure(keys, class = "ub_keys")
}

ub_exact_map <- function(keys, role, direction) {
  t <- keys$t$terms
  sel <- switch(role,
    treatment = which(t$TYPE == "TREATMENT" & t$ROLE == "NAME"),
    analyte_code = which(t$TYPE == "ANALYTE" & t$ROLE %in% c("CODE", "CODE_ALIAS")),
    analyte_name = {
      # a name identical to its code (e.g. PCTEST = PCTESTCD = "ABC123") has no NAME row: use the CODE row.
      # NAME rows stay first, so unblinding a code shared by name and code still returns the name.
      nm <- which(t$TYPE == "ANALYTE" & t$ROLE == "NAME")
      c(nm, which(t$TYPE == "ANALYTE" & t$ROLE == "CODE" & !t$ORIG_TEXT %in% t$ORIG_TEXT[nm]))
    },
    ub_error("Unknown exact_vars role '", role, "'"))
  if (direction == "blind") return(setNames(t$BLIND_TEXT[sel], t$ORIG_TEXT[sel]))
  m <- setNames(t$ORIG_TEXT[sel], t$BLIND_TEXT[sel])
  m[!duplicated(names(m))]      # a code shared by several spellings: the first (primary) one
}

# Whole-value map of a grouping variable (codebook keys only)
ub_category_map <- function(keys, group, direction) {
  t <- keys$t$terms
  sel <- t$TYPE == "CATEGORY" & t$ROLE == group
  if (direction == "blind") return(setNames(t$BLIND_TEXT[sel], t$ORIG_TEXT[sel]))
  m <- setNames(t$ORIG_TEXT[sel], t$BLIND_TEXT[sel])
  m[!duplicated(names(m))]
}

# Terms whose original text must not appear in released data (grouping values are not secrets)
ub_leak_terms <- function(keys) { t <- keys$t$terms; t[t$TYPE != "CATEGORY", , drop = FALSE] }

# FALSE for codebook keys: a code is written exactly as in the codebook, whatever the case of the match
ub_preserve_case <- function(keys) if (is.null(keys$manifest$preserve_case)) TRUE else isTRUE(keys$manifest$preserve_case)

ub_analyte_key_map <- function(keys, direction) {
  t <- keys$t$terms
  sel <- t$TYPE == "ANALYTE" & t$ROLE %in% c("CODE", "CODE_ALIAS")
  setNames(t$IDENTITY_KEY[sel], if (direction == "blind") t$ORIG_TEXT[sel] else t$BLIND_TEXT[sel])
}

# UPPER-CASED analyte code / name / alias (orig or blind, per direction) -> IDENTITY_KEY
ub_analyte_lookup <- function(keys, direction) {
  t <- keys$t$terms
  sel <- t$TYPE == "ANALYTE"
  setNames(t$IDENTITY_KEY[sel], toupper(trimws(if (direction == "blind") t$ORIG_TEXT[sel] else t$BLIND_TEXT[sel])))
}

ub_text_terms <- function(keys, namespace) {
  t <- keys$t$terms
  pri <- if (namespace == "analyte") c(PRIVATE = 0, ANALYTE = 1, TREATMENT = 9, CATEGORY = 5) else c(PRIVATE = 0, TREATMENT = 1, ANALYTE = 2, CATEGORY = 5)
  in_text <- .chr(keys$manifest$category_in_text)
  min_len <- if (is.null(keys$manifest$category_in_text_min_length)) 6L else as.integer(keys$manifest$category_in_text_min_length)
  # descriptive grouping values (e.g. an ARM label inside APROFILE) are replaced in text too; short codes never
  keep <- ifelse(t$TYPE == "CATEGORY", t$ROLE %in% in_text & nchar(t$ORIG_TEXT) >= min_len,
                 t$TYPE == "PRIVATE" | namespace == "any" | t$TYPE == toupper(namespace))
  data.frame(orig = t$ORIG_TEXT[keep], blind = t$BLIND_TEXT[keep], priority = unname(pri[t$TYPE[keep]]),
             stringsAsFactors = FALSE)
}

#' Save a key (maps folder)
#' @param keys a `ub_keys` object
#' @param maps_dir destination folder (PRIVATE)
#' @export
ub_save_keys <- function(keys, maps_dir) {
  dir.create(maps_dir, recursive = TRUE, showWarnings = FALSE)
  for (k in names(UB_MAP_FILES)) {
    d <- keys$t[[k]]
    if ("FACTOR" %in% names(d)) d$FACTOR <- sprintf("%.17g", as.numeric(d$FACTOR))
    utils::write.csv(d, file.path(maps_dir, UB_MAP_FILES[[k]]), row.names = FALSE, na = "")
  }
  writeLines(jsonlite::toJSON(keys$manifest, auto_unbox = TRUE, pretty = TRUE),
             file.path(maps_dir, "maps_manifest.json"))
  invisible(maps_dir)
}

#' Load a key (maps folder)
#' @param maps_dir folder holding the key CSVs and maps_manifest.json
#' @export
ub_load_keys <- function(maps_dir) {
  man <- file.path(maps_dir, "maps_manifest.json")
  if (!file.exists(man)) ub_error("No maps_manifest.json in ", maps_dir, " -- build the maps first. Schema-1 maps ",
                                  "(before 2026-09-27) are not accepted: they were built with a published seed.")
  t <- list()
  for (k in names(UB_MAP_FILES)) {
    p <- file.path(maps_dir, UB_MAP_FILES[[k]])
    t[[k]] <- if (file.exists(p)) utils::read.csv(p, colClasses = "character", na.strings = character(),
                                                  check.names = FALSE, encoding = "UTF-8") else .empty_tbl(UB_MAP_COLS[[k]])
  }
  .new_keys(t, .read_json(man))
}

# ---------------------------------------------------------------------------
# randomness: cryptographic when no seed is given
# ---------------------------------------------------------------------------
.ub_rng <- function(seed = NULL, stream = 0L) {
  if (!is.null(seed)) {
    env <- new.env()
    runif_s <- function(n) {
      old <- if (exists(".Random.seed", globalenv())) get(".Random.seed", globalenv()) else NULL
      on.exit(if (!is.null(old)) assign(".Random.seed", old, globalenv()) else rm(".Random.seed", envir = globalenv()))
      if (is.null(env$state)) set.seed(as.integer(seed) + stream, kind = "Mersenne-Twister") else assign(".Random.seed", env$state, globalenv())
      u <- stats::runif(n)
      env$state <- get(".Random.seed", globalenv())
      u
    }
    return(list(u = runif_s, entropy = "seeded"))
  }
  if (requireNamespace("openssl", quietly = TRUE)) {
    u <- function(n) {
      if (n == 0) return(numeric())
      b <- matrix(as.integer(openssl::rand_bytes(7L * n)), nrow = 7L)
      (bitwAnd(b[1, ], 31L) * 2^48 + b[2, ] * 2^40 + b[3, ] * 2^32 + b[4, ] * 2^24 + b[5, ] * 2^16 + b[6, ] * 2^8 + b[7, ]) / 2^53
    }
    return(list(u = u, entropy = "openssl"))
  }
  warning("Package 'openssl' not available: falling back to R's RNG seeded from the clock. ",
          "Install openssl before building maps for a real study.")
  list(u = function(n) stats::runif(n), entropy = "weak")
}

.shuffle <- function(x, rng) if (length(x) < 2) x else x[order(rng$u(length(x)))]
.next_seq <- function(v) { d <- suppressWarnings(as.integer(gsub("\\D", "", v))); if (length(d) && any(!is.na(d))) max(d, na.rm = TRUE) + 1L else 1L }
.fmt_tpl <- function(tpl, seq, studyid_blind = NULL, subjid_blind = NULL) {
  out <- tpl
  m <- regmatches(tpl, regexpr("\\{seq(:0?(\\d+)d)?\\}", tpl))
  if (length(m)) {
    w <- sub("^\\{seq:0?(\\d+)d\\}$", "\\1", m)
    val <- if (grepl("^\\d+$", w)) formatC(seq, width = as.integer(w), flag = "0") else as.character(seq)
    out <- sub(m, val, out, fixed = TRUE)
  }
  if (!is.null(studyid_blind)) out <- gsub("{studyid_blind}", studyid_blind, out, fixed = TRUE)
  if (!is.null(subjid_blind)) out <- gsub("{subjid_blind}", subjid_blind, out, fixed = TRUE)
  out
}
.suffix <- function(j) if (j <= 1) "" else letters[j]

#' Build (or extend) the blinding key from SOURCE datasets
#'
#' @param datasets named list: config dataset name -> list(data, meta) as returned by [ub_read_dataset()]
#' @param cfg configuration ([ub_load_config()] / study$cfg)
#' @param identities study$identities
#' @param rename_vars study$rename_vars
#' @param existing an existing key to extend (entries are never re-drawn)
#' @param seed NULL (cryptographic randomness) or an integer for TESTS ONLY
#' @return list(keys, summary)
#' @export
#' @param codebook NULL, or list(terms, keep) from the codebook (study folders): the term table is then taken from
#'   the codebook as it is (codes such as TRT01/ANL01, replaced without case variants) instead of being generated
ub_build_keys <- function(datasets, cfg, identities = list(), rename_vars = list(), existing = NULL, seed = NULL,
                          codebook = NULL) {
  naming <- cfg$placeholder_naming; nb <- cfg$numeric_blinding; dcfg <- cfg$date_blinding
  low <- as.numeric(nb$multiplier_low); high <- as.numeric(nb$multiplier_high)
  maxdec <- if (is.null(nb$max_inferred_decimals)) 6L else as.integer(nb$max_inferred_decimals)
  rngs <- lapply(0:6, function(i) .ub_rng(seed, i * 1000L))
  names(rngs) <- c("subj", "site", "sf", "af", "lf", "dates", "ids")
  old <- if (is.null(existing)) lapply(UB_MAP_COLS, .empty_tbl) else existing$t
  summary <- list()
  nonblank <- function(x) { x <- as.character(x); x[!is.na(x) & nzchar(trimws(x))] }

  # ---- subjects & sites ----
  subj <- list(); sites <- character(); studyid_orig <- list()
  for (nm in names(datasets)) {
    df <- datasets[[nm]]$data; idv <- cfg$datasets[[nm]]$id_vars
    u <- idv$usubjid
    if (!u %in% names(df)) ub_error(nm, ": id variable ", u, " not found")
    if (!is.null(idv$studyid) && idv$studyid %in% names(df)) {
      sv <- unique(trimws(nonblank(df[[idv$studyid]])))
      if (length(sv) == 1) studyid_orig[[nm]] <- sv
    }
    if (any(is.na(df[[u]]) | !nzchar(trimws(df[[u]])))) ub_error(nm, ": blank ", u)
    for (i in which(!duplicated(df[[u]]))) {
      uid <- as.character(df[[u]][i])
      prev <- if (is.null(subj[[uid]])) c("", "") else subj[[uid]]
      sid <- if (!is.null(idv$studyid) && idv$studyid %in% names(df)) as.character(df[[idv$studyid]][i]) else ""
      sj <- if (!is.null(idv$subjid) && idv$subjid %in% names(df)) as.character(df[[idv$subjid]][i]) else ""
      subj[[uid]] <- c(if (nzchar(sid)) sid else prev[1], if (nzchar(sj)) sj else prev[2])
    }
    if (!is.null(idv$siteid) && idv$siteid %in% names(df)) sites <- union(sites, nonblank(df[[idv$siteid]]))
  }
  new_ids <- .shuffle(sort(as.character(setdiff(names(subj), old$subjects$USUBJID_ORIG)), method = "radix"), rngs$subj)
  seq0 <- if (nrow(old$subjects)) .next_seq(old$subjects$SUBJID_BLIND) else 1L
  sb <- vapply(seq_along(new_ids), function(i) .fmt_tpl(naming$subjid_blind_template, seq0 + i - 1L), "")
  new_s <- data.frame(STUDYID_ORIG = vapply(new_ids, function(u) subj[[u]][1], ""), USUBJID_ORIG = new_ids,
                      SUBJID_ORIG = vapply(new_ids, function(u) subj[[u]][2], ""),
                      STUDYID_BLIND = rep(naming$studyid_blind, length(new_ids)),
                      USUBJID_BLIND = vapply(sb, function(s) .fmt_tpl(naming$usubjid_blind_template, 0, naming$studyid_blind, s), ""),
                      SUBJID_BLIND = sb, stringsAsFactors = FALSE, row.names = NULL)
  subjects <- rbind(old$subjects, new_s)
  summary$subjects <- c(nrow(old$subjects), nrow(new_s))
  new_sites <- .shuffle(sort(as.character(setdiff(sites, old$sites$SITEID_ORIG)), method = "radix"), rngs$site)
  sseq <- if (nrow(old$sites)) .next_seq(old$sites$SITEID_BLIND) else 1L
  sites_df <- rbind(old$sites, data.frame(SITEID_ORIG = new_sites,
    SITEID_BLIND = vapply(seq_along(new_sites), function(i) .fmt_tpl(naming$siteid_blind_template, sseq + i - 1L), ""),
    stringsAsFactors = FALSE))
  summary$sites <- c(nrow(old$sites), length(new_sites))

  # ---- treatments & analytes ----
  treatments <- character(); analytes <- list(); codes_seen <- character()
  for (nm in names(datasets)) {
    df <- datasets[[nm]]$data; ds <- cfg$datasets[[nm]]
    for (var in names(ds$exact_vars)) {
      if (!var %in% names(df)) next
      if (ds$exact_vars[[var]] == "treatment") treatments <- union(treatments, nonblank(df[[var]]))
      if (ds$exact_vars[[var]] == "analyte_code") codes_seen <- union(codes_seen, nonblank(df[[var]]))
    }
    pair <- .chr(ds$analyte_pair)
    if (length(pair) && pair[1] %in% names(df)) {
      codes <- trimws(as.character(df[[pair[1]]]))
      nms <- if (pair[2] %in% names(df)) trimws(as.character(df[[pair[2]]])) else rep("", length(codes))
      keep <- !duplicated(paste(codes, nms, sep = "\r")) & nzchar(codes)
      for (i in which(keep)) {
        lst <- analytes[[codes[i]]]; if (is.null(lst)) lst <- character()
        if (nzchar(nms[i]) && !nms[i] %in% lst) lst <- c(lst, nms[i])
        analytes[[codes[i]]] <- lst
      }
    }
  }
  if (!is.null(codebook)) {
    terms <- codebook$terms
    cb <- terms[terms$TYPE == "ANALYTE", , drop = FALSE]
    analytes <- list()
    for (cd in cb$IDENTITY_KEY[cb$ROLE == "CODE"]) analytes[[cd]] <- character()
    for (i in which(cb$ROLE == "NAME")) analytes[[cb$IDENTITY_KEY[i]]] <- c(analytes[[cb$IDENTITY_KEY[i]]], cb$ORIG_TEXT[i])
    code_alias <- setNames(cb$IDENTITY_KEY[cb$ROLE == "CODE_ALIAS"], cb$ORIG_TEXT[cb$ROLE == "CODE_ALIAS"])
    identities$analyte_aliases <- split(cb$ORIG_TEXT[cb$ROLE == "ALIAS"], cb$IDENTITY_KEY[cb$ROLE == "ALIAS"])
    terms_old <- old$terms[0, , drop = FALSE]
    summary$terms <- c(0L, nrow(terms))
  } else {
    lt <- .ub_legacy_terms(old, naming, identities, treatments, analytes, codes_seen, rngs)
    terms <- lt$terms; analytes <- lt$analytes; code_alias <- lt$code_alias; terms_old <- lt$terms_old
    summary$terms <- c(nrow(terms_old), nrow(terms) - nrow(terms_old))
  }

  # ---- numeric factors ----
  sf_old <- old$subject_factors
  decs <- list()
  for (nm in names(datasets)) {
    df <- datasets[[nm]]$data; ds <- cfg$datasets[[nm]]; u <- ds$id_vars$usubjid
    for (var in .chr(ds$subject_numeric_vars)) {
      if (!var %in% names(df)) next
      v <- suppressWarnings(as.numeric(df[[var]]))
      ok <- !is.na(v)
      if (!any(ok)) next
      kk <- .k2(as.character(df[[u]][ok]), var)
      dd <- ub_infer_decimals(v[ok], maxdec)
      agg <- tapply(dd, kk, max)
      for (k in names(agg)) decs[[k]] <- max(c(decs[[k]], agg[[k]]))
    }
  }
  for (nm in names(datasets)) {
    df <- datasets[[nm]]$data; ds <- cfg$datasets[[nm]]; u <- ds$id_vars$usubjid
    pn <- ds$param_numeric
    if (is.null(pn) || !pn$param_var %in% names(df)) next
    cl <- ub_param_class(pn, df[[pn$param_var]], cfg)
    for (var in .chr(pn$variables)) {
      if (!var %in% names(df)) next
      v <- suppressWarnings(as.numeric(df[[var]]))
      ok <- !is.na(v) & cl$kind %in% c("subject", "own")
      if (!any(ok)) next
      kk <- .k2(as.character(df[[u]][ok]), cl$key[ok])
      agg <- tapply(ub_infer_decimals(v[ok], maxdec), kk, max)
      for (k in names(agg)) decs[[k]] <- max(c(decs[[k]], agg[[k]]))
    }
  }
  kn <- sort(as.character(setdiff(names(decs), .k2(sf_old$USUBJID_ORIG, sf_old$VARIABLE))), method = "radix")
  parts <- strsplit(kn, "\r", fixed = TRUE)
  new_sf <- data.frame(USUBJID_ORIG = vapply(parts, `[`, "", 1), VARIABLE = vapply(parts, `[`, "", 2),
                       FACTOR = low + (high - low) * rngs$sf$u(length(kn)),
                       SRC_DECIMALS = as.character(unlist(decs[kn])), stringsAsFactors = FALSE)
  sf_old$FACTOR <- as.numeric(sf_old$FACTOR)
  subject_factors <- rbind(sf_old, new_sf)
  summary$subject_factors <- c(nrow(sf_old), nrow(new_sf))

  af_old <- old$analyte_factors; lf_old <- old$lloq_factors
  code_to_key <- c(setNames(names(analytes), names(analytes)), code_alias)
  pairs <- character(); lloq_dec <- list()
  by_name <- character()   # UPPER code / name / alias -> analyte key
  for (code in names(analytes)) {
    x <- c(code, analytes[[code]], .chr(identities$analyte_aliases[[code]]))
    by_name[toupper(trimws(x))] <- code
  }
  for (al in names(code_alias)) by_name[toupper(trimws(al))] <- code_alias[[al]]
  ot <- terms_old[terms_old$TYPE == "ANALYTE", , drop = FALSE]
  for (i in seq_len(nrow(ot))) { k <- toupper(trimws(ot$ORIG_TEXT[i])); if (is.na(by_name[k])) by_name[k] <- ot$IDENTITY_KEY[i] }
  for (nm in names(datasets)) {
    df <- datasets[[nm]]$data; ds <- cfg$datasets[[nm]]; pn <- ds$param_numeric
    if (is.null(pn) || !pn$param_var %in% names(df) || is.null(pn$analyte_var)) next
    cl <- ub_param_class(pn, df[[pn$param_var]], cfg)
    need <- cl$kind %in% c("scale", "inverse")
    if (!any(need)) next
    if (!pn$analyte_var %in% names(df)) ub_error(nm, ": ", pn$analyte_var, " is needed to scale ", pn$param_var, " parameters")
    k <- unname(by_name[toupper(trimws(as.character(df[[pn$analyte_var]])))])
    unk <- unique(as.character(df[[pn$analyte_var]])[need & is.na(k)])
    if (length(unk)) ub_error(.ub_unknown_analyte_message(nm, pn$analyte_var, sort(unk, method = "radix"), analytes, !is.null(codebook)))
    u <- as.character(df[[ds$id_vars$usubjid]])
    pairs <- union(pairs, unique(.k2(u[need], k[need])))
  }
  for (nm in names(datasets)) {
    df <- datasets[[nm]]$data; ds <- cfg$datasets[[nm]]; an <- ds$analyte_numeric
    if (is.null(an) || !an$analyte_var %in% names(df)) next
    u <- as.character(df[[ds$id_vars$usubjid]])
    k <- unname(code_to_key[trimws(as.character(df[[an$analyte_var]]))])
    pairs <- union(pairs, unique(.k2(u[!is.na(k)], k[!is.na(k)])))
    for (lv in .chr(an$lloq_vars)) {
      if (!lv %in% names(df)) next
      v <- suppressWarnings(as.numeric(df[[lv]])); ok <- !is.na(v) & !is.na(k)
      if (!any(ok)) next
      agg <- tapply(ub_infer_decimals(v[ok], maxdec), k[ok], max)
      for (a in names(agg)) lloq_dec[[a]] <- max(c(lloq_dec[[a]], agg[[a]]))
    }
    for (lv in .chr(an$lloq_text_vars)) {
      if (!lv %in% names(df)) next
      tx <- as.character(df[[lv]])
      for (i in which(!is.na(k) & !is.na(tx))) {
        r <- regexpr(.LEAD_NUM, tx[i], perl = TRUE)
        if (r > 0) lloq_dec[[k[i]]] <- max(c(lloq_dec[[k[i]]], 0L, ub_text_decimals(regmatches(tx[i], r))))
      }
    }
    for (sp in ds$derived_text) {
      if (!sp$bql_ref %in% names(df)) next
      tx <- as.character(df[[sp$bql_ref]]); b <- !is.na(k) & .ub_is_bql(tx)
      for (i in which(b & !duplicated(k))) if (is.null(lloq_dec[[k[i]]])) lloq_dec[[k[i]]] <- ub_text_decimals(tx[i])
    }
  }
  pn <- sort(as.character(setdiff(pairs, .k2(af_old$USUBJID_ORIG, af_old$ANALYTE_KEY))), method = "radix")
  pp <- strsplit(pn, "\r", fixed = TRUE)
  new_af <- data.frame(USUBJID_ORIG = vapply(pp, `[`, "", 1), ANALYTE_KEY = vapply(pp, `[`, "", 2),
                       FACTOR = low + (high - low) * rngs$af$u(length(pn)), stringsAsFactors = FALSE)
  af_old$FACTOR <- as.numeric(af_old$FACTOR)
  analyte_factors <- rbind(af_old, new_af)
  ln <- sort(as.character(setdiff(names(lloq_dec), lf_old$ANALYTE_KEY)), method = "radix")
  new_lf <- data.frame(ANALYTE_KEY = ln, FACTOR = low + (high - low) * rngs$lf$u(length(ln)),
                       SRC_DECIMALS = as.character(unlist(lloq_dec[ln])), stringsAsFactors = FALSE)
  lf_old$FACTOR <- as.numeric(lf_old$FACTOR)
  lloq_factors <- rbind(lf_old, new_lf)
  summary$analyte_factors <- c(nrow(af_old), nrow(new_af)); summary$lloq_factors <- c(nrow(lf_old), nrow(new_lf))

  # ---- date shifts ----
  dmin <- as.integer(dcfg$shift_days_min); dmax <- as.integer(dcfg$shift_days_max)
  if (dmin < 1 || dmax < dmin) ub_error("date_blinding: need 1 <= shift_days_min <= shift_days_max")
  need <- c(if (!UB_STUDY_KEY %in% old$date_shifts$USUBJID_ORIG) UB_STUDY_KEY,
            sort(as.character(setdiff(names(subj), old$date_shifts$USUBJID_ORIG)), method = "radix"))
  if (length(need)) {
    u1 <- rngs$dates$u(length(need)); u2 <- rngs$dates$u(length(need))
    mag <- dmin + floor(u1 * (dmax - dmin + 1))
    sh <- ifelse(u2 < 0.5, mag, -mag)
  } else sh <- integer()
  date_shifts <- rbind(old$date_shifts, data.frame(USUBJID_ORIG = need, SHIFT_DAYS = as.character(as.integer(sh)),
                                                   stringsAsFactors = FALSE))
  summary$date_shifts <- c(nrow(old$date_shifts), length(need))

  # ---- renames ----
  rn <- .empty_tbl(UB_MAP_COLS$renames)
  for (dsn in sort(as.character(names(rename_vars)), method = "radix")) {
    m <- rename_vars[[dsn]]
    for (a in sort(as.character(names(m)), method = "radix")) rn[nrow(rn) + 1L, ] <- c(dsn, a, m[[a]])
  }
  for (dsn in unique(rn$DATASET)) {
    b <- rn$VAR_BLIND[rn$DATASET == dsn]
    if (anyDuplicated(toupper(b))) ub_error("rename_vars[", dsn, "]: duplicate target names")
    bad <- b[nchar(b) > 8 | !grepl("^[A-Za-z]", b)]
    if (length(bad)) ub_error("rename_vars[", dsn, "]: invalid XPT V5 names ", paste(bad, collapse = ", "))
  }

  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  manifest <- if (is.null(existing)) list() else existing$manifest
  entropy <- rngs$subj$entropy
  manifest$schema_version <- 2L
  manifest$builder <- paste("universalblinder (R)", as.character(utils::packageVersion("universalblinder")))
  manifest$updated_utc <- now
  if (is.null(manifest$created_utc)) manifest$created_utc <- now
  manifest$multiplier_range <- c(low, high)
  manifest$date_shift_days <- c(dmin, dmax)
  manifest$seeded <- isTRUE(manifest$seeded) || !is.null(seed)
  manifest$weak_entropy <- isTRUE(manifest$weak_entropy) || identical(entropy, "weak")
  cbk <- c("codebook", "preserve_case", "category_keep", "category_in_text", "category_in_text_min_length")
  manifest[cbk] <- NULL
  if (!is.null(codebook)) {
    grp <- .ub_cb_groups(cfg)
    manifest$codebook <- TRUE
    manifest$preserve_case <- FALSE
    manifest$category_keep <- if (length(codebook$keep)) lapply(codebook$keep, I) else structure(list(), names = character())
    manifest$category_in_text <- I(names(grp)[vapply(grp, function(g) g$in_text, logical(1))])
    manifest$category_in_text_min_length <- as.integer(.ub_cb_settings(cfg)$category_in_text_min_length)
  }
  if (length(studyid_orig)) {
    so <- if (is.null(manifest$studyid_orig)) list() else manifest$studyid_orig
    for (nm in names(studyid_orig)) so[[nm]] <- studyid_orig[[nm]]
    manifest$studyid_orig <- so
  }
  keys <- .new_keys(list(subjects = subjects, sites = sites_df, terms = terms, subject_factors = subject_factors,
                         analyte_factors = analyte_factors, lloq_factors = lloq_factors, date_shifts = date_shifts,
                         renames = rn), manifest)
  list(keys = keys, summary = summary)
}

# Error for analyte values (e.g. PP.PPCAT) that match no code, name or alias -- says what to add
# (mirrors python maps._unknown_analyte_message; written to the PRIVATE step log on purpose).
.ub_unknown_analyte_message <- function(name, var, unknown, analytes, codebook = FALSE) {
  if (codebook) return(paste0(name, ".", var, ": ", length(unknown), " value(s) match no analyte code, name or other spelling: ",
                              paste0("'", trimws(unknown), "'", collapse = ", "), ". They are needed to scale the PK parameters: ",
                              "give each one the code of its analyte (ANL..) in the codebook instead of '-'"))
  guess <- function(v) {
    u <- toupper(trimws(v)); hits <- character()
    for (cd in names(analytes)) for (x in c(cd, analytes[[cd]]))
      if (nzchar(u) && (grepl(u, toupper(x), fixed = TRUE) || grepl(toupper(x), u, fixed = TRUE))) hits <- c(hits, cd)
    if (length(unique(hits)) == 1) hits[1] else NA_character_
  }
  cds <- sort(names(analytes), method = "radix")
  known <- paste(vapply(cds, function(cd) paste0(cd, " = ", if (length(analytes[[cd]])) paste(analytes[[cd]], collapse = ", ") else cd), ""), collapse = "; ")
  g <- vapply(unknown, guess, ""); g[is.na(g)] <- "<CODE>"
  ex <- paste(vapply(unique(g), function(cd) paste0(cd, " = ", if (sum(g == cd) > 1) paste0("c(", paste0('"', trimws(unknown[g == cd]), '"', collapse = ", "), ")")
                                                   else paste0('"', trimws(unknown[g == cd]), '"')), ""), collapse = ", ")
  paste0(name, ".", var, ": ", length(unknown), " value(s) match no analyte code, name or alias: ",
         paste0("'", trimws(unknown), "'", collapse = ", "), ". Known analytes (code = name): ", known,
         ". Add each value as an alias of its analyte code in the study settings, e.g. analyte_aliases = list(", ex,
         ") (see USERGUIDE, \"Analyte names that differ between datasets\")")
}

# Term table with generated tokens (study files without a codebook)
.ub_legacy_terms <- function(old, naming, identities, treatments, analytes, codes_seen, rngs) {
  code_alias <- unlist(identities$analyte_code_aliases)
  if (is.null(code_alias)) code_alias <- setNames(character(), character())
  for (al in names(code_alias)) {
    moved <- analytes[[al]]; analytes[[al]] <- NULL
    tg <- code_alias[[al]]
    if (is.null(analytes[[tg]])) ub_error("analyte_code_aliases: target code '", tg, "' was not found in the data")
    analytes[[tg]] <- union(analytes[[tg]], moved)
  }
  for (c in codes_seen) if (is.null(analytes[[c]]) && !c %in% names(code_alias))
    ub_error("Analyte code '", c, "' has no name pairing and no alias -- add it to identities.analyte_code_aliases")
  all_names <- unlist(lapply(names(analytes), function(c) setNames(analytes[[c]], rep(c, length(analytes[[c]])))))
  if (length(all_names)) {
    up <- toupper(all_names)
    dup <- up[duplicated(up)]
    for (d in dup) if (length(unique(names(all_names)[up == d])) > 1) ub_error("Analyte name '", d, "' is used by two codes")
  }

  terms_old <- old$terms
  trows <- .empty_tbl(UB_MAP_COLS$terms)
  add <- function(type, role, key, seq, orig, blind) {
    trows[nrow(trows) + 1L, ] <<- c(type, role, key, as.character(seq), orig, blind)
  }
  have <- function(type, role, key, orig) any(terms_old$TYPE == type & terms_old$ROLE == role &
                                                terms_old$IDENTITY_KEY == key & terms_old$ORIG_TEXT == orig)
  seq_of <- function(type, key) {
    s <- c(terms_old$SEQ[terms_old$TYPE == type & terms_old$IDENTITY_KEY == key],
           trows$SEQ[trows$TYPE == type & trows$IDENTITY_KEY == key])
    if (length(s)) as.integer(s[1]) else NA_integer_
  }
  next_seq <- function(type) {
    s <- as.integer(c(terms_old$SEQ[terms_old$TYPE == type], trows$SEQ[trows$TYPE == type]))
    if (length(s)) max(s) + 1L else 1L
  }
  for (tn in .shuffle(sort(as.character(treatments), method = "radix"), rngs$ids)) {
    sq <- seq_of("TREATMENT", tn); if (is.na(sq)) sq <- next_seq("TREATMENT")
    if (!have("TREATMENT", "NAME", tn, tn)) add("TREATMENT", "NAME", tn, sq, tn, .fmt_tpl(naming$treat_blind_template, sq))
    als <- identities$treatment_aliases[[tn]]
    for (j in seq_along(als)) if (!have("TREATMENT", "ALIAS", tn, als[j]))
      add("TREATMENT", "ALIAS", tn, sq, als[j], paste0(.fmt_tpl(naming$treat_alias_blind_template, sq), .suffix(j)))
  }
  for (k in names(identities$treatment_aliases)) if (!k %in% treatments && !k %in% terms_old$IDENTITY_KEY)
    ub_error("treatment_aliases: '", k, "' is not a treatment value found in the data")
  for (code in .shuffle(sort(as.character(names(analytes)), method = "radix"), rngs$ids)) {
    sq <- seq_of("ANALYTE", code); if (is.na(sq)) sq <- next_seq("ANALYTE")
    if (!have("ANALYTE", "CODE", code, code)) add("ANALYTE", "CODE", code, sq, code, .fmt_tpl(naming$analyte_code_blind_template, sq))
    j <- sum(terms_old$TYPE == "ANALYTE" & terms_old$ROLE == "NAME" & terms_old$IDENTITY_KEY == code)
    for (nmv in analytes[[code]]) {
      if (toupper(nmv) == toupper(code) || have("ANALYTE", "NAME", code, nmv)) next
      j <- j + 1L
      add("ANALYTE", "NAME", code, sq, nmv, paste0(.fmt_tpl(naming$analyte_name_blind_template, sq), .suffix(j)))
    }
    als <- identities$analyte_aliases[[code]]
    for (j in seq_along(als)) if (!have("ANALYTE", "ALIAS", code, als[j]))
      add("ANALYTE", "ALIAS", code, sq, als[j], paste0(.fmt_tpl(naming$analyte_alias_blind_template, sq), .suffix(j)))
    cal <- sort(as.character(names(code_alias)[code_alias == code]), method = "radix")
    for (j in seq_along(cal)) if (!have("ANALYTE", "CODE_ALIAS", code, cal[j]))
      add("ANALYTE", "CODE_ALIAS", code, sq, cal[j], paste0(.fmt_tpl(naming$analyte_code_alias_blind_template, sq), LETTERS[j]))
  }
  for (k in names(identities$analyte_aliases)) if (is.null(analytes[[k]]))
    ub_error("analyte_aliases: '", k, "' is not an analyte code found in the data")
  for (tm in identities$private_terms) if (!have("PRIVATE", "TERM", tm, tm)) {
    sq <- next_seq("PRIVATE"); add("PRIVATE", "TERM", tm, sq, tm, .fmt_tpl(naming$private_term_blind_template, sq))
  }
  terms <- rbind(terms_old, trows)
  if (anyDuplicated(terms$BLIND_TEXT)) ub_error("Duplicate blind tokens generated")

  list(terms = terms, analytes = analytes, code_alias = code_alias, terms_old = terms_old)
}
