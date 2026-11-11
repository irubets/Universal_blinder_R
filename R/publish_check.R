# Pre-publication leak scan: every file under a folder (normally the repository) is searched for values that
# exist in one or more PRIVATE key (maps) folders -- original treatment/analyte names, aliases, private terms,
# study IDs, subject IDs, renamed variable names -- plus optional extra terms. Data files that are not labelled
# SYNTHETIC, key-like files, R/Python session leftovers and logs are flagged too. Run it before every push.

.UB_PUB_TEXT_EXT <- c("md", "txt", "py", "r", "sas", "json", "csv", "tsv", "html", "htm", "yml", "yaml", "toml", "cfg",
                      "ini", "log", "rd", "rmd", "qmd", "xml", "bat", "cmd", "ps1", "sh", "lst")
.UB_PUB_DATA_EXT <- c("xpt", "sas7bdat", "rds", "rda", "rdata", "xlsx", "xls", "parquet", "feather")
.UB_PUB_FORBIDDEN <- c(".rhistory", ".rdata", "replace_subjid.csv", "replace_terms.csv", "replace_siteid.csv",
                       "factors_subject_numeric.csv", "factors_analyte_numeric.csv", "factors_lloq.csv",
                       "shift_dates.csv", "maps_manifest.json", "replace_treat_analyte.csv", "rename_vars.csv",
                       "factors_adsl_numeric.csv", "factors_conc_numeric.csv", "codebook.csv", "aliases.csv")
.UB_PUB_SKIP_DIRS <- c(".git", "__pycache__", ".pytest_cache", ".Rproj.user", "node_modules", ".venv", "venv")
.UB_PUB_MIN_LEN <- 3L

.ub_pub_csv <- function(p) {
  if (!file.exists(p)) return(NULL)
  tryCatch(utils::read.csv(p, colClasses = "character", na.strings = character(), check.names = FALSE,
                           fileEncoding = "UTF-8-BOM"),
           error = function(e) NULL, warning = function(w)
             tryCatch(utils::read.csv(p, colClasses = "character", na.strings = character(), check.names = FALSE,
                                      fileEncoding = "latin1"), error = function(e) NULL))
}

#' Private terms of one or more key (maps) folders
#'
#' Collects every value that must never appear in published files: original treatment, analyte, grouping and
#' private-term texts (`replace_terms.csv`, R/Python format, and the older `replace_treat_analyte.csv`), original
#' study and subject IDs (`replace_subjid.csv`), renamed variable names (`rename_vars.csv`), and the actual values
#' of the codebook (`codebook.csv` in the maps folder or its parent, rows coded `-` excluded). Terms shorter than
#' 3 characters are ignored. Used by [ub_publish_check()] and reusable for masking.
#'
#' @param maps_dirs character vector of maps folders (missing folders are skipped with a warning)
#' @param extra_terms optional character vector of extra terms, or the path of a text file with one term per line
#' @return `data.frame(kind, term)`, one row per distinct term (case-insensitive), longest first. PRIVATE.
#' @keywords internal
#' @export
.ub_private_terms <- function(maps_dirs, extra_terms = NULL) {
  kinds <- character(); terms <- character()
  put <- function(kind, x) {
    x <- trimws(as.character(x))
    x <- x[!is.na(x) & nchar(x) >= .UB_PUB_MIN_LEN]
    if (length(x)) { kinds <<- c(kinds, rep(kind, length(x))); terms <<- c(terms, x) }
  }
  for (d in as.character(maps_dirs)) {
    if (is.na(d) || !dir.exists(d)) { warning("maps folder not found: ", d, call. = FALSE); next }
    t <- .ub_pub_csv(file.path(d, "replace_terms.csv"))
    if (!is.null(t) && "ORIG_TEXT" %in% names(t)) {
      ty <- if ("TYPE" %in% names(t)) tolower(trimws(t$TYPE)) else rep("term", nrow(t))
      ty[!nzchar(ty)] <- "term"
      for (k in unique(ty)) put(k, t$ORIG_TEXT[ty == k])
    }
    t <- .ub_pub_csv(file.path(d, "replace_treat_analyte.csv"))
    if (!is.null(t)) for (c in intersect(c("MATCH_TEXT", "CANONICAL_VALUE", "PCTESTCD_ORIG", "PCTEST_ORIG"), names(t)))
      put("term", t[[c]])
    t <- .ub_pub_csv(file.path(d, "replace_subjid.csv"))
    if (!is.null(t)) {
      if ("STUDYID_ORIG" %in% names(t)) put("studyid", t$STUDYID_ORIG)
      if ("USUBJID_ORIG" %in% names(t)) put("usubjid", t$USUBJID_ORIG)
      if ("SUBJID_ORIG" %in% names(t)) put("subjid", t$SUBJID_ORIG)
    }
    t <- .ub_pub_csv(file.path(d, "rename_vars.csv"))
    if (!is.null(t) && "VAR_ORIG" %in% names(t)) {
      keep <- if ("VAR_BLIND" %in% names(t)) toupper(t$VAR_ORIG) != toupper(t$VAR_BLIND) else TRUE
      put("variable_name", t$VAR_ORIG[keep])
    }
    for (cb in unique(c(file.path(d, "codebook.csv"), file.path(dirname(normalizePath(d, mustWork = FALSE)), "codebook.csv")))) {
      t <- .ub_pub_csv(cb)
      if (is.null(t)) next
      names(t) <- toupper(trimws(names(t)))
      if (!"ACTUAL_VALUE" %in% names(t)) next
      bv <- if ("BLINDED_VALUE" %in% names(t)) trimws(t$BLINDED_VALUE) else rep("", nrow(t))
      put("codebook", t$ACTUAL_VALUE[bv != "-"])
    }
  }
  if (!is.null(extra_terms) && length(extra_terms)) {
    x <- as.character(extra_terms)
    if (length(x) == 1 && file.exists(x) && !dir.exists(x)) x <- readLines(x, warn = FALSE, encoding = "UTF-8")
    put("extra", x)
  }
  out <- data.frame(kind = kinds, term = terms, stringsAsFactors = FALSE)
  out <- out[!duplicated(toupper(out$term)), , drop = FALSE]
  out <- out[order(-nchar(out$term), out$term), , drop = FALSE]
  rownames(out) <- NULL
  out
}

.ub_pub_escape <- function(x) gsub("([.\\\\^$|?*+()\\[\\]{}])", "\\\\\\1", x, perl = TRUE)

# compiled-once list of perl patterns (chunked: one huge alternation can exceed the PCRE size limit)
.ub_pub_patterns <- function(terms, chunk = 400L) {
  if (!length(terms)) return(character())
  g <- split(terms, ceiling(seq_along(terms) / chunk))
  vapply(g, function(t) paste0("(?i)(?<![A-Za-z0-9])(?:", paste(.ub_pub_escape(t), collapse = "|"), ")(?![A-Za-z0-9])"), "")
}

# first matching term of each element of x (NA = none)
.ub_pub_first_hit <- function(x, pats, lookup) {
  hit <- rep(NA_character_, length(x))
  for (p in pats) {
    todo <- which(is.na(hit))
    if (!length(todo)) break
    m <- regexpr(p, x[todo], perl = TRUE)
    w <- which(m > 0)
    if (length(w)) hit[todo[w]] <- lookup[toupper(regmatches(x[todo], m))]
  }
  hit
}

.ub_pub_is_fixture_key <- function(folder) {
  man <- file.path(folder, "maps_manifest.json")
  subj <- file.path(folder, "replace_subjid.csv")
  tryCatch({
    if (!isTRUE(jsonlite::fromJSON(man)$synthetic_fixture)) return(FALSE)
    t <- utils::read.csv(subj, colClasses = "character", na.strings = character())
    nrow(t) > 0 && all(t$STUDYID_ORIG == "SYN-PK-001")
  }, error = function(e) FALSE, warning = function(w) FALSE)
}

.ub_pub_files <- function(repo) {
  out <- character()
  walk <- function(d, rel) {
    ents <- list.files(d, all.files = TRUE, no.. = TRUE)
    isdir <- dir.exists(file.path(d, ents))
    for (f in ents[!isdir]) out <<- c(out, if (nzchar(rel)) paste0(rel, "/", f) else f)
    for (s in ents[isdir]) if (!s %in% .UB_PUB_SKIP_DIRS) walk(file.path(d, s), if (nzchar(rel)) paste0(rel, "/", s) else s)
  }
  walk(repo, "")
  out
}

#' Scan a folder for private values before publishing it
#'
#' @param repo the folder to scan (normally a copy of the files to publish)
#' @param maps_dirs one or more PRIVATE key (maps) folders whose original values must not appear
#' @param extra_terms optional extra terms (character vector, or a text file with one term per line)
#' @param quiet `TRUE` = print nothing
#' @return invisibly, `data.frame(file, line, kind, term)` of findings (no rows = clean). `kind` is the kind of
#'   private term found (treatment, analyte, category, private, studyid, usubjid, subjid, variable_name, codebook,
#'   term, extra) or `key_file`, `log_file`, `unlabelled_data`. `term` is the private value found (PRIVATE; the
#'   printed summary shows only its kind and length). `line` is NA for whole-file findings and data files.
#' @export
ub_publish_check <- function(repo, maps_dirs, extra_terms = NULL, quiet = FALSE) {
  say <- function(...) if (!quiet) cat(paste0(...), "\n", sep = "")
  repo <- normalizePath(repo, mustWork = TRUE)
  tt <- .ub_private_terms(maps_dirs, extra_terms)
  if (!nrow(tt)) say("WARNING: no private terms loaded -- check the maps folder(s)")
  pats <- .ub_pub_patterns(tt$term)
  lookup <- stats::setNames(tt$term, toupper(tt$term))
  kind_of <- stats::setNames(tt$kind, tt$term)
  rows <- list()
  add <- function(file, line, kind, term) rows[[length(rows) + 1L]] <<-
    data.frame(file = file, line = as.integer(line), kind = kind, term = term, stringsAsFactors = FALSE)

  for (rel in .ub_pub_files(repo)) {
    path <- file.path(repo, rel)
    low <- tolower(basename(rel))
    ext <- tolower(tools::file_ext(low))
    if (startsWith(low, ".") && !grepl(".", substring(low, 2), fixed = TRUE)) ext <- ""   # .Rhistory, .gitignore
    if (low %in% .UB_PUB_FORBIDDEN) {
      if (low %in% c(".rhistory", ".rdata") || !.ub_pub_is_fixture_key(dirname(path))) add(rel, NA, "key_file", "")
      next
    }
    if (ext == "log") add(rel, NA, "log_file", "")
    if (ext %in% .UB_PUB_DATA_EXT) {
      df <- NULL; label <- ""
      if (ext %in% c("xpt", "sas7bdat")) {
        df <- tryCatch(if (ext == "xpt") haven::read_xpt(path) else haven::read_sas(path), error = function(e) NULL)
        if (!is.null(df)) { label <- attr(df, "label"); if (is.null(label)) label <- "" }
      }
      if (!startsWith(toupper(label), "SYNTHETIC")) { add(rel, NA, "unlabelled_data", ""); next }
      if (length(pats)) {
        labs <- vapply(df, function(v) { l <- attr(v, "label"); if (is.null(l)) "" else as.character(l) }, "")
        vals <- unique(unlist(lapply(df, function(v) unique(as.character(v))), use.names = FALSE))
        blob <- c(vals[!is.na(vals)], labs, names(df))
        h <- .ub_pub_first_hit(blob, pats, lookup)
        h <- h[!is.na(h)]
        if (length(h)) add(rel, NA, kind_of[[h[1]]], h[1])        # one finding per data file
      }
      next
    }
    if (ext %in% .UB_PUB_TEXT_EXT || ext == "") {
      if (!length(pats)) next
      txt <- tryCatch(suppressWarnings(readLines(path, warn = FALSE, encoding = "UTF-8", skipNul = TRUE)),
                      error = function(e) character())
      if (!length(txt)) next
      txt <- iconv(txt, "UTF-8", "UTF-8", sub = "")
      txt[is.na(txt)] <- ""
      h <- .ub_pub_first_hit(txt, pats, lookup)
      for (i in which(!is.na(h))) add(rel, i, kind_of[[h[i]]], h[i])
    }
  }
  out <- if (length(rows)) do.call(rbind, rows) else
    data.frame(file = character(), line = integer(), kind = character(), term = character(), stringsAsFactors = FALSE)
  if (nrow(out)) {
    say("publish-check: ", nrow(out), " finding(s) -- do NOT publish until resolved")
    what <- ifelse(out$kind == "key_file", "key/session file must not be published",
            ifelse(out$kind == "log_file", "log file (may contain paths and values)",
            ifelse(out$kind == "unlabelled_data", "data file not labelled SYNTHETIC",
                   paste0("private ", out$kind, " (length ", nchar(out$term), ")"))))
    say(paste0("  ", out$file, ifelse(is.na(out$line), "", paste0(" line ", out$line)), ": ", what, collapse = "\n"))
  } else {
    say("publish-check: clean (", nrow(tt), " private terms checked)")
  }
  invisible(out)
}
