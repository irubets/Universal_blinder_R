# Record identifiers, release post-processing and local retrieval -- mirrors python/records.py.
#
# Every released record carries UBRECID, a random identifier unique across the study. Released rows are
# re-sorted by the blinded USUBJID (stable), and a private crosswalk (UBRECID -> source row) is archived
# per release under private/crosswalk/<release id>/ in the same format as the Python package, so a
# release made by either package can be retrieved by either.

UB_RECID_VAR <- "UBRECID"
.UB_RECID_ALPHABET <- strsplit("ABCDEFGHJKLMNPQRSTUVWXYZ23456789", "")[[1]]
.UB_EQUIV <- list(c("SUBJID", "USUBJID"), c("ATEST", "PCTEST"), c("PARAMCD", "PCTESTCD"), c("AVISIT", "VISIT"))

.ub_md5 <- function(path) unname(tools::md5sum(path))

.ub_recids <- function(n, used) {
  out <- character()
  while (length(out) < n) {
    k <- (n - length(out)) * 2L
    idx <- if (requireNamespace("openssl", quietly = TRUE)) as.integer(openssl::rand_bytes(8L * k)) %% 32L
           else floor(stats::runif(8L * k) * 32)
    cand <- vapply(split(.UB_RECID_ALPHABET[idx + 1L], rep(seq_len(k), each = 8L)), paste, "", collapse = "")
    cand <- unique(cand[!cand %in% used$ids])
    cand <- cand[seq_len(min(length(cand), n - length(out)))]
    used$ids <- c(used$ids, cand); out <- c(out, cand)
  }
  out
}

.ub_release_id <- function() {
  t <- Sys.time()
  paste0(format(t, "%Y%m%dT%H%M%S", tz = "UTC"), sprintf("%06d", as.integer((as.numeric(t) %% 1) * 1e6)), "Z")
}

.ub_norm <- function(x) { x <- toupper(trimws(as.character(x))); x[is.na(x)] <- ""; x }

.ub_blank_mask <- function(src, rule) {
  m <- rep(TRUE, nrow(src))
  for (v in names(rule$when)) {
    if (!v %in% names(src)) return(rep(FALSE, nrow(src)))
    m <- m & .ub_norm(src[[v]]) %in% .ub_norm(unlist(rule$when[[v]]))
  }
  m
}

.ub_rule_applies <- function(rule, name) {
  ds <- if (is.null(rule$datasets)) "*" else unlist(rule$datasets)
  any(vapply(ds, function(d) .glob_match(toupper(name), toupper(d)), logical(1)))
}

.UB_WHOLE_NUMBER_VARS <- c("DOSEP")

# Apply blank_where / drop_vars, round DOSEP, add UBRECID, re-sort by blinded USUBJID. `bl` is list(data, meta) in SOURCE order.
ub_postprocess <- function(name, bl, src, ds_cfg, drop_vars, blank_where, recids) {
  df <- bl$data; meta <- bl$meta
  if (nrow(df) != nrow(src)) ub_error(name, ": blinded and source row counts differ")
  if (UB_RECID_VAR %in% names(df)) ub_error(name, ": the source already has a variable ", UB_RECID_VAR)
  blanked <- list()
  for (rule in blank_where) {
    if (!.ub_rule_applies(rule, name)) next
    m <- .ub_blank_mask(src, rule)
    if (!any(m)) next
    for (v in unlist(rule$vars)) if (v %in% names(df)) {
      if (is.character(df[[v]])) df[[v]][m] <- "" else df[[v]][m] <- NA
      blanked[[v]] <- (if (is.null(blanked[[v]])) 0L else blanked[[v]]) + sum(m)
    }
  }
  # planned dose = a dose category: released as a whole number (half up); the source keeps its value
  for (v in intersect(.UB_WHOLE_NUMBER_VARS, names(df))) if (is.numeric(df[[v]])) df[[v]] <- floor(df[[v]] + 0.5)
  pats <- toupper(c(unlist(drop_vars[["*"]]), unlist(drop_vars[[name]])))
  ids <- toupper(.chr(ds_cfg$id_vars))
  dropped <- names(df)[vapply(names(df), function(c) !toupper(c) %in% ids &&
                                any(vapply(pats, function(p) .glob_match(toupper(c), p), logical(1))), logical(1))]
  df <- df[, setdiff(names(df), dropped), drop = FALSE]
  meta$labels <- meta$labels[setdiff(names(meta$labels), dropped)]
  meta$formats <- meta$formats[setdiff(names(meta$formats), dropped)]
  df <- cbind(stats::setNames(data.frame(recids, stringsAsFactors = FALSE), UB_RECID_VAR), df)
  meta$labels <- c(stats::setNames("Blinded record identifier", UB_RECID_VAR), meta$labels)
  ord <- seq_len(nrow(df))
  uc <- ds_cfg$id_vars$usubjid
  if (!is.null(uc) && uc %in% names(df)) ord <- order(df[[uc]], method = "radix")   # stable
  df <- df[ord, , drop = FALSE]; rownames(df) <- NULL
  xw <- data.frame(UBRECID = df[[UB_RECID_VAR]], SRC_ROW = ord, stringsAsFactors = FALSE)
  list(data = df, meta = meta, xw = xw, dropped = dropped, blanked = blanked)
}

.ub_crosswalk_root <- function(study) file.path(dirname(study$paths$maps_dir), "crosswalk")

.ub_write_crosswalk <- function(study, rid, tables, info) {
  d <- file.path(.ub_crosswalk_root(study), rid)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  for (nm in names(tables)) utils::write.csv(tables[[nm]], file.path(d, paste0(tolower(nm), ".csv")), row.names = FALSE)
  writeLines(jsonlite::toJSON(list(release_id = rid, datasets = info), auto_unbox = TRUE, pretty = TRUE),
             file.path(d, "crosswalk.json"))
  d
}

.ub_releases <- function(root) {
  if (!dir.exists(root)) return(character())
  d <- list.dirs(root, full.names = FALSE, recursive = FALSE)
  sort(d[file.exists(file.path(root, d, "crosswalk.json"))], decreasing = TRUE, method = "radix")
}

.ub_load_crosswalk <- function(root, rid) {
  info <- .read_json(file.path(root, rid, "crosswalk.json"))$datasets
  tabs <- lapply(stats::setNames(names(info), names(info)), function(nm)
    utils::read.csv(file.path(root, rid, paste0(tolower(nm), ".csv")), colClasses = c("character", "integer")))
  list(info = info, xw = tabs)
}

# SAS dates / datetimes / times as ISO text for people reading the retrieved records (as in Python)
.ub_readable <- function(df) {
  for (c in names(df)) {
    x <- df[[c]]
    if (inherits(x, "Date")) df[[c]] <- ifelse(is.na(x), "", format(x, "%Y-%m-%d"))
    else if (inherits(x, "POSIXct")) df[[c]] <- ifelse(is.na(x), "", format(x, "%Y-%m-%dT%H:%M", tz = "UTC"))
    else if (inherits(x, "hms") || inherits(x, "difftime")) {
      s <- as.numeric(x); df[[c]] <- ifelse(is.na(s), "", sprintf("%02d:%02d", as.integer(s %/% 3600), as.integer(s %% 3600 %/% 60)))
    }
  }
  df
}

#' Read a findings CSV
#'
#' One row per finding. Columns: DATASET (optional), UBRECID (optional; several separated by spaces,
#' commas or semicolons), any blinded variable to filter on (SUBJID, APROFILE, ATEST, AVISIT, ...;
#' several values separated by ";"), FINDING (free text carried to the output). Blank cells are ignored.
#' @param path CSV file
#' @return list of named character vectors
#' @export
ub_read_findings <- function(path) {
  f <- utils::read.csv(path, colClasses = "character", na.strings = character(), check.names = FALSE,
                       encoding = "UTF-8", fileEncoding = "UTF-8-BOM")
  names(f) <- toupper(trimws(names(f)))
  lapply(seq_len(nrow(f)), function(i) { r <- unlist(f[i, , drop = TRUE]); r[nzchar(trimws(r))] })
}

#' Original records behind the findings of a review of the blinded data
#'
#' Filters the BLINDED datasets with what each finding names and returns the ORIGINAL rows of the
#' matching records through the crosswalk. SUBJID/USUBJID, ATEST/PCTEST, PARAMCD/PCTESTCD and
#' AVISIT/VISIT stand in for each other. Record identifiers of older releases are found too.
#' @param study a `ub_study`
#' @param findings list of named character vectors (see [ub_read_findings()])
#' @param out_dir optional folder for <dataset>.csv (PRIVATE: real data)
#' @return named list of data.frames (FINDING_NO, FINDING, UBRECID, RELEASE + original variables)
#' @export
ub_retrieve <- function(study, findings, out_dir = NULL) {
  root <- .ub_crosswalk_root(study)
  rids <- .ub_releases(root)
  if (!length(rids)) ub_error("no crosswalk found -- run the blind step first")
  keys <- ub_load_keys(study$paths$maps_dir)
  s <- keys$t$subjects
  sj2u <- stats::setNames(.ub_norm(s$USUBJID_BLIND), .ub_norm(s$SUBJID_BLIND))
  u2sj <- stats::setNames(names(sj2u), sj2u)
  cws <- list(); cw <- function(rid) { if (is.null(cws[[rid]])) cws[[rid]] <<- .ub_load_crosswalk(root, rid); cws[[rid]] }
  bls <- list(); bl <- function(nm) { if (is.null(bls[[nm]])) bls[[nm]] <<- ub_read_dataset(ub_blinded_path(study, nm))$data; bls[[nm]] }
  srcs <- list()
  src <- function(nm, info) {
    if (is.null(srcs[[nm]])) {
      p <- ub_source_path(study, nm)
      if (.ub_md5(p) != info[[nm]]$source_md5) ub_error(nm, ": the source file changed since the release -- retrieve against the source that was blinded")
      srcs[[nm]] <<- .ub_readable(ub_read_dataset(p)$data)
    }
    srcs[[nm]]
  }
  out <- list()
  for (no in seq_along(findings)) {
    f <- findings[[no]]; names(f) <- toupper(names(f))
    text <- if ("FINDING" %in% names(f)) f[["FINDING"]] else ""
    nms <- if ("DATASET" %in% names(f)) toupper(trimws(f[["DATASET"]])) else names(study$datasets)
    ids <- if (UB_RECID_VAR %in% names(f)) { x <- toupper(unlist(strsplit(trimws(f[[UB_RECID_VAR]]), "[[:space:],;]+"))); x[nzchar(x)] } else character()
    filt <- f[setdiff(names(f), c("DATASET", "FINDING", UB_RECID_VAR))]
    for (nm in nms) {
      if (is.null(study$datasets[[nm]])) ub_error("finding ", no, ": unknown dataset ", nm)
      hits <- data.frame(rid = character(), id = character(), row = integer(), stringsAsFactors = FALSE)
      if (length(ids)) {
        left <- ids
        for (rid in rids) {
          x <- cw(rid)$xw[[nm]]; if (is.null(x)) next
          sel <- x[x$UBRECID %in% left, , drop = FALSE]
          if (nrow(sel)) hits <- rbind(hits, data.frame(rid = rid, id = sel$UBRECID, row = sel$SRC_ROW, stringsAsFactors = FALSE))
          left <- setdiff(left, sel$UBRECID); if (!length(left)) break
        }
      }
      if (length(filt) || !length(ids)) {
        b <- bl(nm); m <- rep(TRUE, nrow(b)); usable <- TRUE
        for (col in names(filt)) {
          want <- .ub_norm(strsplit(filt[[col]], "\\s*;\\s*")[[1]])
          cc <- if (col %in% names(b)) col else {
            alt <- unlist(lapply(.UB_EQUIV, function(p) if (col %in% p) setdiff(p, col) else NULL))
            alt <- alt[alt %in% names(b)]; if (length(alt)) alt[1] else NA_character_ }
          if (is.na(cc)) { usable <- FALSE; break }
          if (col %in% c("SUBJID", "USUBJID") && cc != col) {
            tr <- if (col == "SUBJID") sj2u else u2sj
            want <- ifelse(want %in% names(tr), unname(tr[want]), want)
          }
          m <- m & .ub_norm(b[[cc]]) %in% want
        }
        if (!usable) next
        if (length(ids)) {
          hits <- hits[hits$id %in% b[[UB_RECID_VAR]][m], , drop = FALSE]
        } else {
          x <- cw(rids[1])$xw[[nm]]
          r <- b[[UB_RECID_VAR]][m]
          hits <- data.frame(rid = rep(rids[1], length(r)), id = r, row = x$SRC_ROW[match(r, x$UBRECID)], stringsAsFactors = FALSE)
        }
      }
      if (!nrow(hits)) next
      rows <- lapply(seq_len(nrow(hits)), function(i) {
        s0 <- src(nm, cw(hits$rid[i])$info)
        cbind(data.frame(FINDING_NO = no, FINDING = text, UBRECID = hits$id[i], RELEASE = hits$rid[i], stringsAsFactors = FALSE),
              s0[hits$row[i], , drop = FALSE], row.names = NULL)
      })
      out[[nm]] <- rbind(out[[nm]], do.call(rbind, rows))
    }
  }
  if (!is.null(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    for (nm in names(out)) utils::write.csv(out[[nm]], file.path(out_dir, paste0(tolower(nm), ".csv")), row.names = FALSE, na = "")
  }
  out
}
