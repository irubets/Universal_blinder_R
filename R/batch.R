#' Staged batch execution
#'
#' Steps: build_maps -> blind -> qc -> unblind -> verify -> release. Each step can
#' run alone, as a range or all together; a state file (<log_dir>/batch_state.json)
#' records status and timing, and a step refuses to run when a prerequisite is
#' missing, failed or stale. Designed for scripts AND for Shiny: nothing is
#' printed, progress is reported through `on_progress(step, message)`.
#'
#' @param study a `ub_study` ([ub_load_study()]) or a path to study.json
#' @param steps character vector of steps (default: the study's batch steps)
#' @param from,to optional first / last step
#' @param on_progress function(step, message) called at start and end of each step
#' @return data.frame with one row per executed step
#' @export
ub_batch_run <- function(study, steps = NULL, from = NULL, to = NULL, on_progress = NULL) {
  if (is.character(study)) study <- ub_load_study(study)
  seq <- intersect(study$steps, UB_STEPS)
  if (!is.null(steps)) {
    bad <- setdiff(steps, UB_STEPS); if (length(bad)) ub_error("Unknown step(s): ", paste(bad, collapse = ", "))
    seq <- UB_STEPS[UB_STEPS %in% steps]
  }
  if (!is.null(from)) seq <- seq[match(from, seq):length(seq)]
  if (!is.null(to)) seq <- seq[seq_len(match(to, seq))]
  out <- list()
  for (s in seq) {
    r <- ub_batch_step(study, s, on_progress = on_progress)
    out[[length(out) + 1L]] <- r
    if (!r$status %in% c("PASS", "WARN", "DONE")) break
  }
  do.call(rbind, lapply(out, as.data.frame, stringsAsFactors = FALSE))
}

.state_path <- function(study) file.path(study$paths$log_dir, "batch_state.json")
.load_state <- function(study) {
  p <- .state_path(study)
  if (file.exists(p)) .read_json(p) else list(steps = list())
}
.save_state <- function(study, st) {
  dir.create(study$paths$log_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(jsonlite::toJSON(st, auto_unbox = TRUE, pretty = TRUE, null = "null"), .state_path(study))
}
.PREREQ <- list(build_maps = character(), blind = "build_maps", qc = "blind", unblind = "blind",
                verify = "unblind", release = c("qc", "verify"))
.is_stale <- function(st, step) {
  me <- st$steps[[step]]; if (is.null(me)) return(FALSE)
  any(vapply(.PREREQ[[step]], function(p) { ps <- st$steps[[p]]; !is.null(ps) && ps$finished > me$finished }, logical(1)))
}

#' Status of every batch step
#' @param study a `ub_study` or a path to study.json
#' @export
ub_batch_status <- function(study) {
  if (is.character(study)) study <- ub_load_study(study)
  st <- .load_state(study)
  data.frame(step = UB_STEPS,
             status = vapply(UB_STEPS, function(s) if (is.null(st$steps[[s]])) "not run" else st$steps[[s]]$status, ""),
             finished = vapply(UB_STEPS, function(s) if (is.null(st$steps[[s]])) "" else st$steps[[s]]$finished, ""),
             stale = vapply(UB_STEPS, function(s) .is_stale(st, s), logical(1)), row.names = NULL, stringsAsFactors = FALSE)
}

.now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6", tz = "UTC")

#' Run one batch step
#' @param study a `ub_study` or a path to study.json
#' @param step one of build_maps, blind, qc, unblind, verify, release
#' @param on_progress optional function(step, message)
#' @export
ub_batch_step <- function(study, step, on_progress = NULL) {
  if (is.character(study)) study <- ub_load_study(study)
  if (!step %in% UB_STEPS) ub_error("Unknown step ", step)
  st <- .load_state(study)
  for (p in .PREREQ[[step]]) {
    ps <- st$steps[[p]]
    if (is.null(ps)) ub_error("Step '", step, "' needs '", p, "' to have run first")
    if (!ps$status %in% c("PASS", "WARN", "DONE")) ub_error("Step '", step, "' needs '", p, "' to succeed (it is ", ps$status, ")")
    if (.is_stale(st, p)) ub_error("Step '", p, "' is stale (an upstream step re-ran after it); re-run '", p, "' first")
  }
  dir.create(study$paths$log_dir, recursive = TRUE, showWarnings = FALSE)
  log_path <- file.path(study$paths$log_dir, sprintf("%02d_%s.log", match(step, UB_STEPS), step))
  lines <- character()
  log <- function(...) lines <<- c(lines, paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), paste0(...)))
  prog <- if (is.function(on_progress)) on_progress else function(step, msg) NULL
  started <- .now(); prog(step, "started"); log("step ", step, " started")
  res <- tryCatch(.STEPS[[step]](study, st, log),
                  ub_error = function(e) list(status = "ERROR", detail = conditionMessage(e)),
                  error = function(e) list(status = "ERROR", detail = paste0(class(e)[1], ": ", conditionMessage(e))))
  if (res$status == "ERROR") log("ERROR ", res$detail)
  writeLines(lines, log_path)
  rec <- list(status = res$status, started = started, finished = .now(),
              detail = if (is.character(res$detail)) res$detail else jsonlite::toJSON(res$detail, auto_unbox = TRUE), log = log_path)
  st <- .load_state(study); st$steps[[step]] <- rec; .save_state(study, st)
  prog(step, res$status)
  c(list(step = step), rec)
}

.read_sources <- function(study, log) {
  out <- list()
  for (nm in names(study$datasets)) { out[[nm]] <- ub_read_dataset(ub_source_path(study, nm)); log("read ", nm, " (", nrow(out[[nm]]$data), " rows)") }
  out
}

.STEPS <- list(
  build_maps = function(study, st, log) {
    md <- study$paths$maps_dir
    existing <- if (file.exists(file.path(md, "maps_manifest.json"))) ub_load_keys(md) else NULL
    if (!is.null(study$seed)) log("WARNING seed given in study file: maps will be REPRODUCIBLE -- for tests only")
    srcs <- .read_sources(study, log)
    cb <- if (is.null(study$codebook)) NULL else .ub_update_codebook(study$codebook, srcs, study$cfg, study$identities, study$seed, log)
    b <- ub_build_keys(srcs, study$cfg, study$identities, study$rename_vars, existing, study$seed, codebook = cb)
    ub_save_keys(b$keys, md)
    for (k in names(b$summary)) log(sprintf("%-16s %d existing + %d new", k, b$summary[[k]][1], b$summary[[k]][2]))
    list(status = "DONE", detail = b$summary)
  },
  blind = function(study, st, log) {
    keys <- ub_load_keys(study$paths$maps_dir); reps <- list(); xws <- list(); info <- list()
    used <- new.env(); used$ids <- character()
    rid <- .ub_release_id()
    for (nm in names(study$datasets)) {
      src <- ub_read_dataset(ub_source_path(study, nm))
      r <- ub_transform(src$data, src$meta, nm, study$cfg, keys, "blind")
      pp <- ub_postprocess(nm, r, src$data, study$cfg$datasets[[nm]], study$drop_vars, study$blank_where,
                           .ub_recids(nrow(r$data), used))
      rep <- r$report
      # source-side checks about variables that are not released no longer apply
      keep <- vapply(rep$checks, function(ck) !any(vapply(pp$dropped, function(d)
        grepl(paste0("\\b", nm, "\\.", d, "\\b"), ck[[3]], perl = TRUE), logical(1))), logical(1))
      rep$checks <- rep$checks[keep]; rep$dropped <- pp$dropped; rep$blanked <- pp$blanked
      ub_write_dataset(pp$data, ub_blinded_path(study, nm), pp$meta, name = nm)
      xws[[nm]] <- pp$xw
      info[[nm]] <- list(source_file = basename(ub_source_path(study, nm)), source_md5 = .ub_md5(ub_source_path(study, nm)),
                         blinded_file = basename(ub_blinded_path(study, nm)), blinded_md5 = .ub_md5(ub_blinded_path(study, nm)),
                         rows = nrow(pp$data))
      reps[[nm]] <- rep
      log("blinded ", nm, " -> ", ub_blinded_path(study, nm), " (dropped: ", paste(c(pp$dropped, "-")[1:max(1, length(pp$dropped))], collapse = " "), ")")
    }
    if ("LABPC" %in% names(study$datasets)) {          # Workflow 3: the lab file in its own layout, linked to this PC
      lw <- .ub_lab_write_layout(study, xws[["PC"]])
      info$LABPC <- c(info$LABPC, lw$info)
      log("lab file in its own layout -> ", lw$info$layout_file)
    }
    d <- .ub_write_crosswalk(study, rid, xws, info)
    if ("LABPC" %in% names(study$datasets)) utils::write.csv(lw$rows, file.path(d, "labpc_labrows.csv"), row.names = FALSE)
    log("crosswalk (record id -> source row) for release ", rid, ": ", d)
    dir.create(study$paths$qc_dir, recursive = TRUE, showWarnings = FALSE)
    writeLines(jsonlite::toJSON(reps, auto_unbox = TRUE, pretty = TRUE, null = "null"), file.path(study$paths$qc_dir, "blind_report.json"))
    list(status = "DONE", detail = c(list(release_id = rid), lapply(reps, function(r) r$rows)))
  },
  qc = function(study, st, log) {
    keys <- ub_load_keys(study$paths$maps_dir)
    bl <- lapply(setNames(names(study$datasets), names(study$datasets)), function(nm) ub_read_dataset(ub_blinded_path(study, nm)))
    rp <- file.path(study$paths$qc_dir, "blind_report.json")
    reps <- if (file.exists(rp)) .read_json(rp) else list()
    F <- ub_run_qc(study, keys, bl, reps)
    ub_write_qc_reports(F, study$paths$qc_dir)
    for (i in which(F$status %in% c("FAIL", "WARN", "WAIVED"))) log(F$status[i], " ", F$check[i], " ", F$dataset[i], ".", F$variable[i], ": ", F$detail[i])
    log("QC status: ", attr(F, "status"))
    list(status = attr(F, "status"), detail = list(fail = sum(F$status == "FAIL"), warn = sum(F$status == "WARN")))
  },
  unblind = function(study, st, log) {
    # local retrieval of every record: the ORIGINAL rows in release order, with their UBRECID
    root <- .ub_crosswalk_root(study); rid <- .ub_releases(root)[1]
    cw <- .ub_load_crosswalk(root, rid)
    for (nm in names(study$datasets)) {
      src <- ub_read_dataset(ub_source_path(study, nm)); x <- cw$xw[[nm]]
      out <- cbind(data.frame(UBRECID = x$UBRECID, stringsAsFactors = FALSE), src$data[x$SRC_ROW, , drop = FALSE], row.names = NULL)
      meta <- src$meta; meta$labels <- c(UBRECID = "Blinded record identifier", meta$labels)
      ub_write_dataset(out, ub_unblinded_path(study, nm), meta, name = nm)
      log("retrieved ", nm, " -> ", ub_unblinded_path(study, nm))
    }
    list(status = "DONE", detail = list(release_id = rid))
  },
  verify = function(study, st, log) {
    # prove the record mapping: 1:1 crosswalk, re-blinding each source record gives exactly the released
    # record with the same UBRECID, and the retrieved record is exactly the source record
    keys <- ub_load_keys(study$paths$maps_dir)
    root <- .ub_crosswalk_root(study); rid <- .ub_releases(root)[1]
    cw <- .ub_load_crosswalk(root, rid)
    res <- list(); worst <- "PASS"; seen <- character()
    for (nm in names(study$datasets)) {
      notes <- character()
      src <- ub_read_dataset(ub_source_path(study, nm))
      bl <- ub_read_dataset(ub_blinded_path(study, nm))$data
      rt <- ub_read_dataset(ub_unblinded_path(study, nm))$data
      x <- cw$xw[[nm]]; n <- nrow(src$data); ids <- x$UBRECID
      if (!identical(sort(x$SRC_ROW), seq_len(n))) notes <- c(notes, "crosswalk rows are not a 1:1 map of the source rows")
      if (anyDuplicated(ids) || any(ids %in% seen)) notes <- c(notes, "record identifiers are not unique across the study")
      seen <- c(seen, ids)
      if (.ub_md5(ub_blinded_path(study, nm)) != cw$info[[nm]]$blinded_md5) notes <- c(notes, "the blinded file changed after the blind step")
      if (nrow(bl) != n || !all(as.vector(bl$UBRECID) == ids)) notes <- c(notes, "blinded records are not in crosswalk order")
      if (!length(notes)) {
        r <- ub_transform(src$data, src$meta, nm, study$cfg, keys, "blind")
        sid <- character(n); sid[x$SRC_ROW] <- ids
        pp <- ub_postprocess(nm, r, src$data, study$cfg$datasets[[nm]], study$drop_vars, study$blank_where, sid)
        if (!identical(names(pp$data), names(bl))) notes <- c(notes, "released variables differ from a re-run of the blinding")
        else {
          bad <- names(bl)[!vapply(names(bl), function(c) .ub_same_col(pp$data[[c]], bl[[c]]), logical(1))]
          if (length(bad)) notes <- c(notes, paste("released values differ from a re-run of the blinding in", paste(bad, collapse = ", ")))
        }
        back <- src$data[x$SRC_ROW, , drop = FALSE]
        bad <- names(back)[!vapply(names(back), function(c) c %in% names(rt) && .ub_same_col(back[[c]], rt[[c]]), logical(1))]
        if (length(bad) || !all(as.vector(rt$UBRECID) == ids)) notes <- c(notes, paste("retrieved records differ from the source in", paste(bad, collapse = ", ")))
      }
      if (nm == "LABPC" && !length(notes)) notes <- .ub_lab_verify(study, bl, cw)
      status <- if (length(notes)) "FAIL" else "PASS"
      res[[nm]] <- list(dataset = nm, status = status, release_id = rid, rows = n, notes = notes)
      log(nm, ": ", status, " ", paste(notes, collapse = "; "))
      if (status == "FAIL") worst <- "FAIL"
    }
    writeLines(jsonlite::toJSON(res, auto_unbox = TRUE, pretty = TRUE), file.path(study$paths$qc_dir, "verify_report.json"))
    list(status = worst, detail = lapply(res, function(r) r$status))
  },
  release = function(study, st, log) {
    qs <- st$steps$qc$status
    if (!qs %in% c("PASS", "WARN")) ub_error("QC status is ", qs, "; resolve or waive every FAIL before release")
    rel <- study$paths$release_dir; dir.create(rel, recursive = TRUE, showWarnings = FALSE)
    rid <- .ub_releases(.ub_crosswalk_root(study))[1]
    man <- c("# Release manifest", "", paste("Created:", .now()), paste("Release:", rid), paste("QC status:", qs), "",
             paste0("Every record carries ", UB_RECID_VAR, ", a random identifier. Quote it (or the blinded SUBJID/USUBJID, ",
                    "APROFILE, ATEST/PCTESTCD, AVISIT/VISIT) when reporting a finding."), "", "| File | SHA-256 |", "|---|---|")
    for (nm in names(study$datasets)) {
      src <- if (nm == "LABPC") .ub_lab_layout_path(study) else ub_blinded_path(study, nm); dst <- file.path(rel, basename(src))
      file.copy(src, dst, overwrite = TRUE)
      h <- if (requireNamespace("openssl", quietly = TRUE)) as.character(openssl::sha256(file(dst))) else unname(tools::md5sum(dst))
      man <- c(man, sprintf("| %s | %s |", basename(dst), h)); log("released ", dst)
    }
    if ("LABPC" %in% names(study$datasets)) man <- c(man, "", .ub_lab_manifest_line(study))
    file.copy(file.path(study$paths$qc_dir, "qc_summary_public.md"), file.path(rel, "qc_summary.md"), overwrite = TRUE)
    writeLines(man, file.path(rel, "RELEASE_MANIFEST.md"))
    list(status = "DONE", detail = rel)
  }
)

# column equality after an XPT round trip: numbers to 1e-9 relative, text without trailing blanks
.ub_same_col <- function(a, b) {
  num <- function(x) if (inherits(x, c("Date", "POSIXct", "hms", "difftime"))) as.numeric(x) else x
  a <- num(a); b <- num(b)
  if (is.character(a) || is.character(b)) {
    a <- sub("\\s+$", "", ifelse(is.na(a), "", as.character(a))); b <- sub("\\s+$", "", ifelse(is.na(b), "", as.character(b)))
    return(identical(a, b))
  }
  a <- as.numeric(a); b <- as.numeric(b)
  length(a) == length(b) && all((is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & abs(a - b) <= 1e-9 * pmax(1, abs(a))))
}
