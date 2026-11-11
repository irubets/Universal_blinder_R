# AI layer, part 2: the private value set, masking, and the leak check (spec Section 7.3).
# Every known private value of a study folder is collected from the codebook, the key (private/maps), the
# settings (blind_options.json) and the identity columns of the datasets in input/. Masking replaces each one
# with a typed placeholder (<SUBJECT_3>, <TREATMENT_1>, ...); the leak check finds any that are left.

.ub_ai_cache <- new.env(parent = emptyenv())

# kind -> placeholder prefix; kinds not listed use their own name in upper case (codebook groups: ARM, REGIMEN...)
.UB_KIND_PREFIX <- c(subject = "SUBJECT", site = "SITE", study = "STUDY", treatment = "TREATMENT", analyte = "ANALYTE",
                     term = "TERM", date = "DATE")
.UB_KIND_ORDER <- c("subject", "site", "study", "treatment", "analyte")
# a value found under several kinds keeps the most specific one; codebook groups (arm, ...) come before "term"
.ub_kind_rank <- function(k) { r <- match(k, .UB_KIND_ORDER); r[is.na(r)] <- length(.UB_KIND_ORDER) + 1L
  r[k == "term"] <- length(.UB_KIND_ORDER) + 2L; r[k == "date"] <- length(.UB_KIND_ORDER) + 3L; r }

.ub_ai_read_options <- function(folder) {
  p <- file.path(folder, "blind_options.json")
  if (!file.exists(p)) return(list())
  tryCatch(.read_json(p), error = function(e) list())
}

.ub_ai_signature <- function(folder) {
  fs <- c(list.files(file.path(folder, "input"), full.names = TRUE),
          list.files(file.path(folder, "private", "maps"), full.names = TRUE),
          file.path(folder, "private", "codebook.csv"), file.path(folder, "blind_options.json"))
  fs <- fs[file.exists(fs)]
  i <- file.info(fs)
  paste(fs, as.numeric(i$mtime), i$size, collapse = "|")
}

# map a kind name coming from another module (publish_check) onto ours
.ub_ai_norm_kind <- function(k) {
  k <- tolower(k)
  ifelse(grepl("subj|usubjid", k), "subject",
  ifelse(grepl("site|invid", k), "site",
  ifelse(grepl("study", k), "study",
  ifelse(grepl("treat|trt", k), "treatment",
  ifelse(grepl("analyte|anl", k), "analyte",
  ifelse(grepl("date", k), "date", "term"))))))
}

.ub_ai_profile <- function(name, cfg, aliases) {
  if (!is.null(aliases[[name]])) return(as.character(aliases[[name]]))
  if (!is.null(cfg$datasets[[name]])) return(name)
  p <- ub_pattern_profile(name, cfg)
  if (is.null(p)) NA_character_ else p
}

.ub_ai_cfg <- function(folder, opts) {
  cfg <- tryCatch({
    cp <- opts[["config", exact = TRUE]]
    if (!is.null(cp) && !grepl("^([A-Za-z]:|/|\\\\)", cp)) cp <- file.path(folder, cp)
    ub_load_config(cp)
  }, error = function(e) ub_load_config())
  if (!is.null(opts$config_overrides)) cfg <- .merge_list(cfg, opts$config_overrides)
  cfg
}

# distinct values of the identity columns of every dataset in input/ (before a key exists this is the only source)
.ub_ai_input_values <- function(folder, opts) {
  dir <- file.path(folder, "input")
  fs <- list.files(dir, pattern = "\\.(xpt|sas7bdat)$", ignore.case = TRUE, full.names = TRUE)
  if (!length(fs)) return(data.frame(kind = character(), value = character(), stringsAsFactors = FALSE))
  cfg <- .ub_ai_cfg(folder, opts)
  aliases <- opts$dataset_aliases
  out <- list()
  add <- function(kind, x) {
    x <- trimws(as.character(x)); x <- unique(x[!is.na(x) & nzchar(x)])
    if (length(x)) out[[length(out) + 1L]] <<- data.frame(kind = kind, value = x, stringsAsFactors = FALSE)
  }
  for (f in fs) {
    name <- toupper(tools::file_path_sans_ext(basename(f)))
    prof <- .ub_ai_profile(name, cfg, aliases)
    df <- tryCatch(ub_read_dataset(f)$data, error = function(e) NULL)
    if (is.null(df)) next
    spec <- if (is.na(prof)) NULL else cfg$datasets[[prof]]
    idv <- spec$id_vars
    cols <- names(df)
    pick <- function(v) if (!is.null(v) && v %in% cols) df[[v]] else NULL
    # standard identity columns, whatever the profile says
    add("subject", c(pick(idv$usubjid), pick(idv$subjid), pick("USUBJID"), pick("SUBJID"), pick("RSUBJID")))
    add("site", c(pick(idv$siteid), pick("SITEID"), pick("INVID")))
    add("study", c(pick(idv$studyid), pick("STUDYID")))
    for (v in names(spec$exact_vars)) if (v %in% cols) {
      role <- as.character(spec$exact_vars[[v]])
      add(if (role == "treatment") "treatment" else if (startsWith(role, "analyte")) "analyte" else "term", df[[v]])
    }
    av <- spec$analyte_numeric$analyte_var
    if (!is.null(av) && av %in% cols) add("analyte", df[[av]])
    # text_vars (TRT01P, APROFILE, PCTPTREF...) are not taken whole: only the names inside them are secret, and
    # those come from the exact columns above; whole values such as "PERIOD 1 DAY 1" stay in released data
  }
  if (!length(out)) return(data.frame(kind = character(), value = character(), stringsAsFactors = FALSE))
  do.call(rbind, out)
}

.ub_ai_key_values <- function(maps_dir) {
  rd <- function(f) {
    p <- file.path(maps_dir, f)
    if (!file.exists(p)) return(NULL)
    tryCatch(utils::read.csv(p, colClasses = "character", na.strings = character(), check.names = FALSE,
                             encoding = "UTF-8"), error = function(e) NULL)
  }
  out <- list()
  add <- function(kind, x) {
    x <- trimws(as.character(x)); x <- unique(x[!is.na(x) & nzchar(x)])
    if (length(x)) out[[length(out) + 1L]] <<- data.frame(kind = kind, value = x, stringsAsFactors = FALSE)
  }
  s <- rd("replace_subjid.csv")
  if (!is.null(s)) { add("subject", c(s$USUBJID_ORIG, s$SUBJID_ORIG)); add("study", s$STUDYID_ORIG) }
  st <- rd("replace_siteid.csv")
  if (!is.null(st)) add("site", st$SITEID_ORIG)
  t <- rd("replace_terms.csv")
  if (!is.null(t) && nrow(t)) {
    k <- ifelse(t$TYPE == "TREATMENT", "treatment", ifelse(t$TYPE == "ANALYTE", "analyte",
         ifelse(t$TYPE == "CATEGORY", tolower(t$ROLE), "term")))
    for (kk in unique(k)) add(kk, t$ORIG_TEXT[k == kk])
  }
  # original dates, if a key ever records them (columns named *_ORIG holding dates)
  for (f in list.files(maps_dir, pattern = "date", ignore.case = TRUE)) {
    d <- rd(f)
    if (is.null(d)) next
    for (c in grep("ORIG", names(d), value = TRUE))
      if (any(grepl("^\\d{4}-\\d{2}-\\d{2}", d[[c]]))) add("date", d[[c]][grepl("^\\d{4}-\\d{2}-\\d{2}", d[[c]])])
  }
  if (!length(out)) return(NULL)
  do.call(rbind, out)
}

.ub_ai_codebook_values <- function(path) {
  cb <- .ub_read_codebook(path)
  if (!nrow(cb)) return(NULL)
  cb <- cb[nzchar(cb$ACTUAL_VALUE) & cb$BLINDED_VALUE != "-", , drop = FALSE]   # '-' = left as it is: not a secret
  if (!nrow(cb)) return(NULL)
  kind <- ifelse(cb$TYPE == "TREATMENT", "treatment", ifelse(cb$TYPE == "ANALYTE", "analyte",
          ifelse(cb$TYPE %in% c("TEXT", "PRIVATE", ""), "term", tolower(cb$TYPE))))
  data.frame(kind = kind, value = cb$ACTUAL_VALUE, stringsAsFactors = FALSE)
}

.ub_ai_option_values <- function(opts) {
  id <- opts$identities
  out <- list()
  add <- function(kind, x) {
    x <- trimws(unlist(lapply(x, as.character))); x <- unique(x[!is.na(x) & nzchar(x)])
    if (length(x)) out[[length(out) + 1L]] <<- data.frame(kind = kind, value = x, stringsAsFactors = FALSE)
  }
  add("treatment", c(names(id$treatment_aliases), unlist(id$treatment_aliases)))
  add("analyte", c(names(id$analyte_aliases), unlist(id$analyte_aliases),
                   names(id$analyte_code_aliases), unlist(id$analyte_code_aliases)))
  add("term", id$private_terms)
  pp <- opts$prepare_pc
  if (!is.null(pp)) {
    add("analyte", c(pp$pctestcd, pp$pctest, names(pp$analytes), unlist(pp$analytes)))
  }
  if (!length(out)) return(NULL)
  do.call(rbind, out)
}

# values too short to mask safely are dropped ("1" would hit every number); subject IDs with separators stay
.ub_ai_keep_value <- function(kind, value) {
  n <- nchar(value)
  ok <- n >= 3 | (kind == "subject" & n >= 2 & grepl("[-/_. ]", value))
  ok & !toupper(value) %in% c("N/A", "NONE", "NULL", "YES", "NO", "Y", "N", "NA", "")
}

#' Every known private value of a study folder, with its placeholder
#'
#' Sources: the codebook (treatment, analyte and grouping originals, other spellings), the key in `private/maps`
#' (subject, site and study IDs, private terms, original dates if present), the settings in `blind_options.json`
#' (aliases, private terms, Prepare PC analytes) and the distinct values of the identity columns of the datasets in
#' `input/` (USUBJID, SUBJID, SITEID, INVID, STUDYID, the configuration's treatment/analyte columns). The result is
#' cached until one of these files changes.
#' @param folder the Universal Blinder folder of a study
#' @return data.frame(kind, value, placeholder); PRIVATE -- never send or print it
#' @export
ub_ai_private_values <- function(folder) {
  folder <- normalizePath(folder, mustWork = FALSE)
  sig <- .ub_ai_signature(folder)
  hit <- .ub_ai_cache[[folder]]
  if (!is.null(hit) && identical(hit$sig, sig)) return(hit$values)
  opts <- .ub_ai_read_options(folder)
  parts <- list(.ub_ai_key_values(file.path(folder, "private", "maps")),
                .ub_ai_codebook_values(file.path(folder, "private", "codebook.csv")),
                .ub_ai_option_values(opts),
                tryCatch(.ub_ai_input_values(folder, opts), error = function(e) NULL))
  if (exists(".ub_private_terms", mode = "function") && dir.exists(file.path(folder, "private", "maps"))) {
    pt <- tryCatch(get(".ub_private_terms", mode = "function")(file.path(folder, "private", "maps"), character()),
                   error = function(e) NULL)
    if (is.data.frame(pt) && nrow(pt) && all(c("kind", "term") %in% names(pt)))
      parts[[length(parts) + 1L]] <- data.frame(kind = .ub_ai_norm_kind(pt$kind), value = as.character(pt$term),
                                                stringsAsFactors = FALSE)
  }
  v <- do.call(rbind, Filter(Negate(is.null), parts))
  if (is.null(v) || !nrow(v)) {
    v <- data.frame(kind = character(), value = character(), placeholder = character(), stringsAsFactors = FALSE)
  } else {
    v$value <- trimws(v$value)
    v <- v[.ub_ai_keep_value(v$kind, v$value), , drop = FALSE]
    rank <- .ub_kind_rank(v$kind)
    v <- v[order(rank, v$kind), , drop = FALSE]
    v <- v[!duplicated(toupper(v$value)), , drop = FALSE]
    rank <- .ub_kind_rank(v$kind)
    v <- v[order(rank, v$kind, toupper(v$value)), , drop = FALSE]
    pref <- ifelse(v$kind %in% names(.UB_KIND_PREFIX), .UB_KIND_PREFIX[v$kind], toupper(gsub("[^A-Za-z0-9]", "", v$kind)))
    seqn <- stats::ave(seq_along(v$kind), v$kind, FUN = seq_along)
    v$placeholder <- ifelse(v$kind == "date", "<DATE>", paste0("<", pref, "_", seqn, ">"))
    rownames(v) <- NULL
  }
  .ub_ai_cache[[folder]] <- list(sig = sig, values = v)
  v
}

# ---------------------------------------------------------------------------------------------------------------
# matcher (shared by masking and the leak check)
# ---------------------------------------------------------------------------------------------------------------
.ub_rx_escape <- function(x) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)

.ub_value_rx <- function(v) {
  first <- substr(v, 1, 1); last <- substr(v, nchar(v), nchar(v))
  pre <- ifelse(grepl("[A-Za-z0-9]", first), "(?<![A-Za-z0-9])", "")
  pre <- paste0(pre, ifelse(grepl("[0-9]", first), "(?<![0-9][.,])", ""))
  post <- ifelse(grepl("[A-Za-z0-9]", last), "(?![A-Za-z0-9])", "")
  post <- paste0(post, ifelse(grepl("[0-9]", last), "(?![.,][0-9])", ""))
  paste0(pre, .ub_rx_escape(v), post)
}

# all non-overlapping hits of VALUES in TEXT: data.frame(start, length, row) (row of VALUES), leftmost-longest
.ub_find_values <- function(text, values, chunk = 150L) {
  empty <- data.frame(start = integer(), length = integer(), row = integer())
  if (is.null(values) || !nrow(values) || !nzchar(text)) return(empty)
  keep <- .ub_ai_keep_value(values$kind, values$value) & values$kind != "date" | values$kind == "date" & nchar(values$value) >= 6
  idx <- which(keep)
  idx <- idx[order(-nchar(values$value[idx]))]
  key <- toupper(values$value)
  hits <- list()
  for (s in seq(1, length(idx), by = chunk)) {
    ii <- idx[s:min(length(idx), s + chunk - 1L)]
    if (!length(ii)) next
    rx <- paste0("(?i)(?:", paste(.ub_value_rx(values$value[ii]), collapse = "|"), ")")
    m <- gregexpr(rx, text, perl = TRUE)[[1]]
    if (m[1] == -1) next
    len <- attr(m, "match.length")
    txt <- toupper(substring(text, m, m + len - 1L))
    row <- ii[match(txt, key[ii])]
    hits[[length(hits) + 1L]] <- data.frame(start = as.integer(m), length = as.integer(len), row = row)
  }
  if (!length(hits)) return(empty)
  h <- do.call(rbind, hits)
  h <- h[!is.na(h$row), , drop = FALSE]
  h <- h[order(h$start, -h$length), , drop = FALSE]
  keep <- logical(nrow(h)); last_end <- 0L
  for (i in seq_len(nrow(h))) if (h$start[i] > last_end) { keep[i] <- TRUE; last_end <- h$start[i] + h$length[i] - 1L }
  h <- h[keep, , drop = FALSE]
  rownames(h) <- NULL
  h
}

.UB_MONTHS <- "JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC"
.UB_DATE_RX <- paste0("(?i)(?<![0-9])(?:\\d{4}-\\d{2}-\\d{2}(?:T\\d{2}:\\d{2}(?::\\d{2}(?:\\.\\d+)?)?)?",
                      "|\\d{1,2}(?:", .UB_MONTHS, ")\\d{4}(?::\\d{2}:\\d{2}(?::\\d{2})?)?",
                      "|\\d{1,2}[ -](?:", .UB_MONTHS, ")[a-z]*[ -]\\d{4})(?![0-9])")

#' Replace every private value in a text by its placeholder
#'
#' Longest values first, ignoring case; a value is only replaced as a whole word (so `101` is not replaced inside
#' `1101` or `10.101`). Values shorter than 3 characters are skipped, except subject IDs with separators.
#' @param text character vector
#' @param values [ub_ai_private_values()] (kind, value, placeholder)
#' @param dates also replace anything that looks like a date (ISO 8601, `01JAN2024`) by `<DATE>`
#' @return the masked text
#' @export
ub_ai_mask <- function(text, values, dates = TRUE) {
  if (!length(text)) return(text)
  vapply(text, function(t) {
    if (is.na(t) || !nzchar(t)) return(t)
    h <- .ub_find_values(t, values)
    if (nrow(h)) {
      for (i in rev(seq_len(nrow(h))))     # right to left, so earlier positions stay valid
        t <- paste0(substr(t, 1, h$start[i] - 1L), values$placeholder[h$row[i]],
                    substr(t, h$start[i] + h$length[i], nchar(t)))
    }
    if (dates) t <- gsub(.UB_DATE_RX, "<DATE>", t, perl = TRUE)
    t
  }, "", USE.NAMES = FALSE)
}

#' Find private values in a text (the final check before a Claude request)
#' @param text one string (the full prompt)
#' @param values [ub_ai_private_values()]
#' @return data.frame(position, line, kind, placeholder), one row per hit; never the value itself
#' @export
ub_ai_leak_check <- function(text, values) {
  text <- paste(text, collapse = "\n")
  h <- .ub_find_values(text, values)
  if (!nrow(h)) return(data.frame(position = integer(), line = integer(), kind = character(), placeholder = character(),
                                  stringsAsFactors = FALSE))
  nl <- gregexpr("\n", text, fixed = TRUE)[[1]]
  nl <- if (nl[1] == -1) integer() else as.integer(nl)
  data.frame(position = h$start, line = findInterval(h$start, nl) + 1L, kind = values$kind[h$row],
             placeholder = values$placeholder[h$row], stringsAsFactors = FALSE)
}
