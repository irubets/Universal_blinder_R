# Study setup: the Universal Blinder folder UB_<StudyNumber> inside an existing study folder, path cleaning,
# scanning the study for datasets, copying them to input, and the study settings file blind_options.json.

.UB_OPTION_KEYS <- c("dataset_aliases", "identities", "rename_vars", "extra_suppress_vars", "qc_waivers",
                     "config_overrides", "drop_vars", "blank_where")
.UB_DATA_EXT <- c("xpt", "sas7bdat")
.UB_LAB_EXT <- c("csv", "xlsx", "xls", "txt")

#' Clean a folder path typed or pasted by the user
#'
#' Trims spaces, removes one pair of surrounding quotes (`"`, `'` or typographic quotes), turns `/` into `\`
#' (Windows), and drops a trailing separator (but keeps `C:\`).
#' @param x a character string
#' @return the cleaned path ("" for empty input)
#' @export
ub_clean_path <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) return("")
  x <- trimws(x[1])
  x <- sub("^file:///", "", x)
  pairs <- list(c('"', '"'), c("'", "'"), c("\u201c", "\u201d"), c("\u2018", "\u2019"), c("\u201d", "\u201d"))
  for (p in pairs) {
    if (nchar(x) >= 2 && startsWith(x, p[1]) && endsWith(x, p[2])) {
      x <- trimws(substr(x, 2, nchar(x) - 1))
      break
    }
  }
  if (.Platform$OS.type == "windows") x <- gsub("/", "\\\\", x)
  while (nchar(x) > 1 && grepl("[\\\\/]$", x) && !grepl("^[A-Za-z]:[\\\\/]$", x) && !grepl("^[\\\\/]{2}$", x))
    x <- substr(x, 1, nchar(x) - 1)
  x
}

#' Is this folder a Universal Blinder study folder?
#' @param path a folder
#' @return TRUE when it holds `blind_options.json`
#' @export
ub_is_ub_folder <- function(path) {
  nzchar(path) && file.exists(file.path(path, "blind_options.json"))
}

.ub_sanitise_name <- function(x) {
  x <- gsub("[<>:\"/\\\\|?*[:cntrl:]]", "-", trimws(x))
  x <- gsub("\\s+", "_", x)
  sub("[. ]+$", "", x)
}

.ub_list_files <- function(dir, ext, exclude_ub = TRUE) {
  if (!dir.exists(dir)) return(character())
  f <- list.files(dir, pattern = paste0("\\.(", paste(ext, collapse = "|"), ")$"), recursive = TRUE,
                  full.names = TRUE, ignore.case = TRUE)
  if (exclude_ub) {
    rel <- substring(normalizePath(f, winslash = "/", mustWork = FALSE),
                     nchar(normalizePath(dir, winslash = "/", mustWork = FALSE)) + 2)
    f <- f[!grepl("(^|/)UB_[^/]*/", rel)]
  }
  f
}

#' Guess the study number from STUDYID in the study's datasets
#' @param study_dir the study folder
#' @return the first non-blank STUDYID found, or the folder name
#' @export
ub_guess_study_number <- function(study_dir) {
  for (f in utils::head(.ub_list_files(study_dir, .UB_DATA_EXT), 20)) {
    d <- tryCatch(if (grepl("\\.xpt$", f, ignore.case = TRUE)) haven::read_xpt(f, n_max = 5)
                  else haven::read_sas(f, n_max = 5), error = function(e) NULL)
    if (!is.null(d) && "STUDYID" %in% toupper(names(d))) {
      v <- trimws(as.character(d[[which(toupper(names(d)) == "STUDYID")[1]]]))
      v <- v[!is.na(v) & nzchar(v)]
      if (length(v)) return(.ub_sanitise_name(v[1]))
    }
  }
  .ub_sanitise_name(basename(normalizePath(study_dir, mustWork = FALSE)))
}

#' Path of the template study folder shipped with the package
#' @export
ub_template_path <- function() system.file("template_folder", package = "universalblinder")

#' Create (or open) the Universal Blinder folder of a study
#'
#' Creates `<study_dir>/UB_<study_number>` with `input`, `output`, `private` and the template files
#' (`blind_options.json`, `blind.R`, `prepare_pc.R`, `retrieve.R`). Existing files are never overwritten.
#' @param study_dir the existing study folder
#' @param study_number the study number used in the folder name
#' @return list(path, created (logical), files_added)
#' @export
ub_setup_study <- function(study_dir, study_number) {
  study_dir <- ub_clean_path(study_dir)
  if (!dir.exists(study_dir)) ub_error("The study folder does not exist: ", study_dir)
  sn <- .ub_sanitise_name(study_number)
  if (!nzchar(sn)) ub_error("Give a study number.")
  ub <- file.path(study_dir, paste0("UB_", sn))
  created <- !dir.exists(ub)
  for (d in c("input", "output", "private")) dir.create(file.path(ub, d), recursive = TRUE, showWarnings = FALSE)
  tpl <- ub_template_path()
  added <- character()
  if (nzchar(tpl)) {
    for (f in list.files(tpl, recursive = TRUE, all.files = FALSE)) {
      to <- file.path(ub, f)
      if (!file.exists(to)) {
        dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
        file.copy(file.path(tpl, f), to)
        added <- c(added, f)
      }
    }
  }
  if (!file.exists(file.path(ub, "blind_options.json"))) ub_write_options(ub, ub_default_options(), backup = FALSE)
  list(path = normalizePath(ub, mustWork = FALSE), created = created, files_added = added)
}

.ub_profiles <- function() {
  cfg <- tryCatch(ub_load_config(ub_default_config_path()), error = function(e) NULL)
  p <- if (is.null(cfg)) character() else names(cfg$datasets)
  toupper(p[!startsWith(p, "_")])
}

.ub_profile_of <- function(file, aliases = list(), profiles = .ub_profiles()) {
  nm <- toupper(tools::file_path_sans_ext(basename(file)))
  al <- aliases[toupper(names(aliases)) == nm]
  if (length(al)) return(toupper(al[[1]]))
  if (nm %in% profiles) return(nm)
  if (grepl("^SUPP", nm)) return("SUPPQUAL")
  NA_character_
}

#' List the datasets and data files found under a study folder
#'
#' @param study_dir the study folder (the `UB_*` folders are skipped)
#' @param ub_dir optional UB folder; files already in its `input` with the same size and date are marked
#' @return data.frame(path, relative, file, type, size_kb, modified, profile, role, preselect, already_copied)
#' @export
ub_scan_study <- function(study_dir, ub_dir = NULL) {
  study_dir <- ub_clean_path(study_dir)
  f <- c(.ub_list_files(study_dir, .UB_DATA_EXT), .ub_list_files(study_dir, .UB_LAB_EXT))
  f <- f[!grepl("^readme", basename(f), ignore.case = TRUE)]
  if (!length(f)) return(data.frame(path = character(), relative = character(), file = character(), type = character(),
                                    size_kb = numeric(), modified = character(), profile = character(), role = character(),
                                    preselect = logical(), already_copied = logical(), stringsAsFactors = FALSE))
  info <- file.info(f)
  root <- normalizePath(study_dir, winslash = "/", mustWork = FALSE)
  rel <- substring(normalizePath(f, winslash = "/", mustWork = FALSE), nchar(root) + 2)
  ext <- tolower(tools::file_ext(f))
  profiles <- .ub_profiles()
  prof <- ifelse(ext %in% .UB_DATA_EXT, vapply(f, .ub_profile_of, "", profiles = profiles), NA_character_)
  role <- unname(ifelse(ext %in% .UB_LAB_EXT, "results file?",
          ifelse(is.na(prof), "no profile", .ub_role_of(prof))))
  copied <- rep(FALSE, length(f))
  if (!is.null(ub_dir) && dir.exists(file.path(ub_dir, "input"))) {
    tgt <- file.path(ub_dir, "input", basename(f))
    ti <- file.info(tgt)
    copied <- file.exists(tgt) & ti$size == info$size & abs(as.numeric(ti$mtime) - as.numeric(info$mtime)) < 2
  }
  out <- data.frame(path = f, relative = rel, file = basename(f), type = ext,
                    size_kb = round(info$size / 1024, 1), modified = format(info$mtime, "%Y-%m-%d %H:%M"),
                    profile = prof, role = role, preselect = role %in% .UB_PRESELECT_ROLES,
                    already_copied = copied, stringsAsFactors = FALSE)
  rownames(out) <- NULL
  rank <- match(out$role, .UB_ROLE_ORDER); rank[is.na(rank)] <- length(.UB_ROLE_ORDER) + 1L
  out[order(rank, out$relative), ]
}

# Role of a dataset profile in the scan of the Study tab.
#  target: the concentrations being blinded (PC, ADNCA/ADPC)
#  exposure: needed to learn the treatment names (EX, EC, ADEX, ADEC)
#  linked: datasets whose subjects, analytes (PPCAT/ATEST = PCTEST), reference dates (PPRFTDTC = PCRFTDTC),
#          specimen and visits match the targets and are blinded with the same key, so they stay traceable
#  subject-level: ADSL/DM, blinded consistently but rich in quasi-identifiers -- include only when needed
.UB_ROLES <- c(PC = "target", ADNCA = "target", ADPC = "target",
               EX = "exposure", EC = "exposure", ADEX = "exposure", ADEC = "exposure",
               PP = "linked: PK parameters", ADPP = "linked: PK parameters", ADPD = "linked: PD",
               ADSL = "subject-level (optional)", DM = "subject-level (optional)",
               SUPPQUAL = "supplemental")
.UB_ROLE_ORDER <- c("target", "exposure", "linked: PK parameters", "linked: PD", "subject-level (optional)",
                    "supplemental", "other", "results file?", "no profile")
.UB_PRESELECT_ROLES <- c("target", "exposure", "linked: PK parameters", "linked: PD")
.ub_role_of <- function(prof) {
  r <- unname(.UB_ROLES[toupper(prof)])
  r[is.na(r)] <- "other"
  r
}

#' Copy files into the input folder of a UB study folder
#' @param ub_dir the UB folder
#' @param files full paths
#' @param overwrite replace a file of the same name in input
#' @return data.frame(file, status) with status copied / already there / exists (not overwritten) / failed
#' @export
ub_copy_to_input <- function(ub_dir, files, overwrite = FALSE) {
  inp <- file.path(ub_dir, "input")
  dir.create(inp, recursive = TRUE, showWarnings = FALSE)
  st <- vapply(files, function(f) {
    to <- file.path(inp, basename(f))
    if (file.exists(to)) {
      a <- file.info(f); b <- file.info(to)
      if (a$size == b$size && abs(as.numeric(a$mtime) - as.numeric(b$mtime)) < 2) return("already there")
      if (!overwrite) return("exists (not overwritten)")
    }
    if (isTRUE(file.copy(f, to, overwrite = TRUE, copy.date = TRUE))) "copied" else "failed"
  }, "")
  data.frame(file = basename(files), status = unname(st), stringsAsFactors = FALSE)
}

#' The default study settings (the template's blind_options.json)
#' @export
ub_default_options <- function() {
  p <- file.path(ub_template_path(), "blind_options.json")
  if (nzchar(ub_template_path()) && file.exists(p)) return(jsonlite::fromJSON(p, simplifyVector = FALSE))
  list(dataset_aliases = setNames(list(), character()), identities = list(
         treatment_aliases = setNames(list(), character()), analyte_aliases = setNames(list(), character()),
         analyte_code_aliases = setNames(list(), character()), private_terms = list()),
       rename_vars = setNames(list(), character()), extra_suppress_vars = setNames(list(), character()),
       qc_waivers = list(), qc_waiver_notes = setNames(list(), character()),
       config_overrides = list(datasets = list(
         ADNCA = list(exact_vars = list(PCTESTCD = "analyte_code", PCTEST = "analyte_name")),
         ADPC = list(exact_vars = list(PCTESTCD = "analyte_code", PCTEST = "analyte_name")))),
       drop_vars = setNames(list(), character()),
       blank_where = list(
         list(datasets = list("AD*"), vars = list("DOSEP"), when = list(DOSEPU = list("mg/m2"))),
         list(datasets = list("EC"), vars = list("ECDOSE"), when = list(ECDOSU = list("mg/m2"))),
         list(datasets = list("EX"), vars = list("EXDOSE"), when = list(EXDOSU = list("mg/m2")))),
       prepare_pc = setNames(list(), character()))
}

#' Read a study's settings (blind_options.json)
#' @param ub_dir the UB folder
#' @return a list (the defaults when the file does not exist)
#' @export
ub_read_options <- function(ub_dir) {
  p <- file.path(ub_dir, "blind_options.json")
  if (!file.exists(p)) return(ub_default_options())
  o <- tryCatch(jsonlite::fromJSON(p, simplifyVector = FALSE),
                error = function(e) ub_error("blind_options.json is not valid JSON: ", conditionMessage(e)))
  o
}

#' Check study settings
#' @param opts a settings list
#' @return character vector of problems (empty when fine)
#' @export
ub_validate_options <- function(opts) {
  p <- character()
  known <- c(.UB_OPTION_KEYS, "qc_waiver_notes", "prepare_pc")
  extra <- setdiff(names(opts), c(known, grep("^_", names(opts), value = TRUE)))
  if (length(extra)) p <- c(p, paste0("unknown setting(s): ", paste(extra, collapse = ", ")))
  w <- unlist(opts$qc_waivers)
  if (length(w)) {
    bad <- w[!grepl("^[A-Za-z_]+:[A-Za-z0-9_*]+\\.[A-Za-z0-9_*]+$", w)]
    if (length(bad)) p <- c(p, paste0("qc_waivers must look like <check>:<DATASET>.<VARIABLE>: ", paste(bad, collapse = ", ")))
    notes <- opts$qc_waiver_notes
    for (x in w) {
      n <- notes[[x]]
      if (is.null(n) || !nzchar(trimws(n$reason %||% "")) || !nzchar(trimws(n$reviewer %||% "")))
        p <- c(p, paste0("QC waiver '", x, "' needs a reason and a reviewer"))
    }
  }
  for (k in c("dataset_aliases", "rename_vars", "extra_suppress_vars", "drop_vars", "config_overrides"))
    if (!is.null(opts[[k]]) && length(opts[[k]]) && is.null(names(opts[[k]])))
      p <- c(p, paste0(k, " must be a set of named entries"))
  p
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Write a study's settings (blind_options.json), keeping a backup of the previous version
#' @param ub_dir the UB folder
#' @param opts the settings list
#' @param backup copy the previous file to private/settings_backup first
#' @export
ub_write_options <- function(ub_dir, opts, backup = TRUE) {
  p <- file.path(ub_dir, "blind_options.json")
  if (backup && file.exists(p)) {
    b <- file.path(ub_dir, "private", "settings_backup")
    dir.create(b, recursive = TRUE, showWarnings = FALSE)
    file.copy(p, file.path(b, paste0("blind_options_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json")))
  }
  writeLines(jsonlite::toJSON(opts, auto_unbox = TRUE, pretty = TRUE, null = "null"), p, useBytes = TRUE)
  invisible(p)
}

#' The part of the study settings the blinding engine uses
#' @param opts settings from [ub_read_options()]
#' @export
ub_options_for_engine <- function(opts) {
  o <- opts[intersect(names(opts), .UB_OPTION_KEYS)]
  o$qc_waivers <- as.character(unlist(o$qc_waivers))
  o
}

# ---- recent studies (per user, outside any study) ----------------------------------------------
.ub_recent_file <- function() file.path(tools::R_user_dir("universalblinder", "config"), "recent_studies.json")

#' Recently opened UB study folders
#' @export
ub_recent_studies <- function() {
  f <- .ub_recent_file()
  if (!file.exists(f)) return(character())
  x <- tryCatch(unlist(jsonlite::fromJSON(f)), error = function(e) character())
  x[dir.exists(x)]
}

#' Remember a UB study folder in the recent list
#' @param ub_dir the UB folder
#' @export
ub_add_recent_study <- function(ub_dir) {
  f <- .ub_recent_file()
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  x <- unique(c(normalizePath(ub_dir, mustWork = FALSE), ub_recent_studies()))
  writeLines(jsonlite::toJSON(utils::head(x, 15)), f)
  invisible(x)
}

#' Status of a UB study folder (for the Study and Workflow tabs)
#' @param ub_dir the UB folder
#' @return list(datasets (data.frame), pc (from ub_check_pc_results or NULL), key_present, codebook_open,
#'   codebook_answer_needed, codebook_review (from [ub_codebook_review_status()]), qc, released)
#' @export
ub_study_status <- function(ub_dir) {
  opts <- tryCatch(ub_read_options(ub_dir), error = function(e) list())
  inp <- file.path(ub_dir, "input")
  f <- .ub_list_files(inp, .UB_DATA_EXT, exclude_ub = FALSE)
  ds <- data.frame(file = basename(f), profile = vapply(f, .ub_profile_of, "", aliases = opts$dataset_aliases %||% list()),
                   stringsAsFactors = FALSE)
  rownames(ds) <- NULL
  pc <- if (exists("ub_check_pc_results", mode = "function")) tryCatch(ub_check_pc_results(ub_dir), error = function(e) NULL) else NULL
  cb <- tryCatch(ub_codebook_read(ub_dir), error = function(e) NULL)
  qc <- NA_character_
  qs <- file.path(ub_dir, "output", "qc_summary.md")
  if (file.exists(qs)) {
    t <- paste(readLines(qs, warn = FALSE), collapse = "\n")
    m <- regmatches(t, regexec("Overall: (PASS|WARN|FAIL)", t))[[1]]
    if (length(m) == 2) qc <- m[2]
  }
  list(datasets = ds, pc = pc, lab_files = ub_lab_files(ub_dir),
       key_present = dir.exists(file.path(ub_dir, "private", "maps")) && length(list.files(file.path(ub_dir, "private", "maps"))) > 0,
       codebook_rows = if (is.null(cb)) 0L else nrow(cb),
       codebook_open = if (is.null(cb)) 0L else sum(cb$.status != "ok"),
       codebook_answer_needed = if (is.null(cb)) 0L else sum(cb$.status == "red"),
       codebook_review = tryCatch(ub_codebook_review_status(ub_dir), error = function(e) list(status = "none")),
       qc = qc,
       released = length(list.files(file.path(ub_dir, "output"), pattern = "\\.xpt$", ignore.case = TRUE)) > 0)
}

# settings as "path = value" lines: named lists are followed, a list of single values gives one line per value,
# any other unnamed list (e.g. blank_where rules) gives one line per element
.ub_setting_leaves <- function(x, path = character()) {
  if (is.list(x) && length(x) && !is.null(names(x)) && all(nzchar(names(x))))
    return(unlist(lapply(names(x), function(k) .ub_setting_leaves(x[[k]], c(path, k))), use.names = FALSE))
  p <- paste(path, collapse = "/")
  if (is.list(x) && !length(x)) return(character())
  if (is.list(x) && all(vapply(x, function(e) !is.list(e) && length(e) == 1, logical(1))))
    return(paste0(p, " = ", vapply(x, as.character, "")))
  if (is.list(x)) return(paste0(p, " = ", vapply(x, function(e) as.character(jsonlite::toJSON(e, auto_unbox = TRUE)), "")))
  paste0(p, " = ", if (length(x) == 1) as.character(x) else as.character(jsonlite::toJSON(x)))
}

#' Settings of a study that differ from the defaults
#'
#' Compares `blind_options.json` with the default settings of a new study. These are the choices that stay with
#' the study folder (they are not in `private`), and that make the pre-flight skip suggestions already applied.
#' @param ub_dir the UB folder
#' @return data.frame(setting, change = "added"/"removed", item)
#' @export
ub_settings_changes <- function(ub_dir) {
  o <- ub_read_options(ub_dir); d <- ub_default_options()
  keys <- function(x) names(x)[!startsWith(names(x), "_")]
  lv <- function(x) unique(unlist(lapply(keys(x), function(k) .ub_setting_leaves(x[[k]], k)), use.names = FALSE))
  lo <- lv(o); ld <- lv(d)
  add <- setdiff(lo, ld); rem <- setdiff(ld, lo)
  items <- c(add, rem)
  data.frame(setting = sub("[/ ].*$", "", items), change = rep(c("added", "removed"), c(length(add), length(rem))),
             item = items, stringsAsFactors = FALSE)
}

#' Reset a study's settings to the defaults
#'
#' Writes the default `blind_options.json`; the previous version is kept in `private/settings_backup`.
#' The datasets, the key and the codebook are not touched.
#' @param ub_dir the UB folder
#' @export
ub_reset_options <- function(ub_dir) {
  ub_write_options(ub_dir, ub_default_options(), backup = TRUE)
}
