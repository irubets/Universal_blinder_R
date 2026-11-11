#' Universal Blinder configuration and study files
#'
#' The configuration (inst/config/blinding_config.json) is generic and holds no
#' study values. The study file (study.json) is PRIVATE: it names the source
#' datasets, the folders (maps = the unblinding key) and any aliases.
#' @name ub_config
#' @importFrom stats setNames
NULL

UB_STEPS <- c("build_maps", "blind", "qc", "unblind", "verify", "release")

ub_error <- function(...) {
  stop(structure(class = c("ub_error", "error", "condition"),
                 list(message = paste0(...), call = sys.call(-1))))
}

#' Path of the packaged default configuration
#' @export
ub_default_config_path <- function() {
  system.file("config", "blinding_config.json", package = "universalblinder", mustWork = TRUE)
}

.read_json <- function(path) {
  jsonlite::fromJSON(path, simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
}

.merge_list <- function(base, override) {
  if (is.null(override)) return(base)
  for (k in names(override)) {
    if (is.list(override[[k]]) && !is.null(names(override[[k]])) && is.list(base[[k]])) {
      base[[k]] <- .merge_list(base[[k]], override[[k]])
    } else {
      base[k] <- list(override[[k]])
    }
  }
  base
}

#' Load a blinding configuration (schema 2)
#' @param path JSON file; NULL for the packaged default.
#' @export
ub_load_config <- function(path = NULL) {
  cfg <- .read_json(if (is.null(path)) ub_default_config_path() else path)
  if (!identical(as.integer(cfg$schema_version), 2L)) ub_error("Config schema_version must be 2")
  cfg
}

.chr <- function(x) if (is.null(x)) character() else vapply(x, as.character, "")

#' Load a private study file
#'
#' Relative paths in the study file are resolved against the study file's folder.
#' @param path path to study.json
#' @return an object of class `ub_study`
#' @export
ub_load_study <- function(path) {
  ub_study_from_list(.read_json(path), dirname(normalizePath(path, mustWork = TRUE)), normalizePath(path))
}

#' Build a study from the contents of a study file
#'
#' The in-memory equivalent of [ub_load_study()], used by [ub_blind_folder()].
#' @param raw a list with the study-file entries (paths, datasets, identities, ...)
#' @param root folder that relative paths resolve against
#' @param path optional path recorded in the study object
#' @return an object of class `ub_study`
#' @export
ub_study_from_list <- function(raw, root, path = NULL) {
  root <- normalizePath(root, mustWork = FALSE)
  cfg_path <- raw[["config", exact = TRUE]]   # exact: `$` would partially match config_overrides
  if (!is.null(cfg_path) && !grepl("^([A-Za-z]:|/|\\\\)", cfg_path)) cfg_path <- file.path(root, cfg_path)
  cfg <- ub_load_config(cfg_path)
  if (!is.null(raw$config_overrides)) cfg <- .merge_list(cfg, raw$config_overrides)
  for (alias in names(raw$dataset_aliases)) {
    target <- raw$dataset_aliases[[alias]]
    if (is.null(cfg$datasets[[target]])) ub_error("dataset_aliases: '", target, "' is not a dataset in the config")
    cfg$datasets[[alias]] <- cfg$datasets[[target]]
  }
  # dataset_patterns: e.g. SUPP* -> SUPPQUAL for a name without a profile of its own
  for (nm in names(raw$datasets)) {
    prof <- ub_pattern_profile(nm, cfg)
    if (is.null(cfg$datasets[[nm]]) && !is.null(prof)) cfg$datasets[[nm]] <- cfg$datasets[[prof]]
  }
  paths <- list(source_dir = "source", maps_dir = "private/maps", blinded_dir = "blinded",
                unblinded_dir = "private/unblinded", qc_dir = "private/qc", release_dir = "release",
                log_dir = "logs")
  for (k in names(raw$paths)) paths[[k]] <- raw$paths[[k]]
  paths <- lapply(paths, function(p) {
    if (grepl("^([A-Za-z]:|/|\\\\)", p)) normalizePath(p, mustWork = FALSE)
    else normalizePath(file.path(root, p), mustWork = FALSE)
  })
  datasets <- lapply(raw$datasets, as.character)
  for (nm in names(datasets)) {
    if (is.null(cfg$datasets[[nm]])) ub_error("Dataset '", nm, "' has no rules in the config; add it or use dataset_aliases")
  }
  for (nm in names(raw$extra_suppress_vars)) {
    if (nm == "*") {   # every dataset
      cfg$common_suppress_vars <- c(.chr(cfg$common_suppress_vars), .chr(raw$extra_suppress_vars[[nm]]))
      next
    }
    cfg$datasets[[nm]]$suppress_vars <- c(.chr(cfg$datasets[[nm]]$suppress_vars), .chr(raw$extra_suppress_vars[[nm]]))
  }
  ident <- raw$identities
  steps <- if (!is.null(raw$batch$steps)) .chr(raw$batch$steps) else UB_STEPS
  structure(list(
    path = if (is.null(path)) file.path(root, "(in memory)") else path, root = root, raw = raw, cfg = cfg, paths = paths, datasets = datasets,
    output_ext = sub("^\\.", "", if (is.null(raw$output_format)) "xpt" else raw$output_format),
    identities = list(
      treatment_aliases = lapply(ident$treatment_aliases, .chr),
      analyte_aliases = lapply(ident$analyte_aliases, .chr),
      analyte_code_aliases = lapply(ident$analyte_code_aliases, as.character),
      private_terms = .chr(ident$private_terms)),
    rename_vars = lapply(raw$rename_vars, function(m) lapply(m, as.character)),
    seed = raw$seed, allow_seeded_maps = isTRUE(raw$allow_seeded_maps),
    qc_waivers = .chr(raw$qc_waivers), steps = steps,
    drop_vars = stats::setNames(lapply(raw$drop_vars, .chr), ifelse(names(raw$drop_vars) == "*", "*", toupper(names(raw$drop_vars)))),
    blank_where = if (is.null(raw$blank_where)) list() else raw$blank_where,
    codebook = if (is.null(raw$codebook)) NULL else if (grepl("^([A-Za-z]:|/|\\\\)", raw$codebook)) raw$codebook
               else file.path(root, raw$codebook)), class = "ub_study")
}

#' Paths of a study's source, blinded and unblinded datasets
#' @param study a `ub_study`
#' @param name dataset name (e.g. "ADNCA")
#' @export
ub_source_path <- function(study, name) file.path(study$paths$source_dir, study$datasets[[name]])
#' @rdname ub_source_path
#' @export
ub_out_name <- function(study, name) paste0(tolower(tools::file_path_sans_ext(basename(study$datasets[[name]]))), ".", study$output_ext)
#' @rdname ub_source_path
#' @export
ub_blinded_path <- function(study, name) file.path(study$paths$blinded_dir, ub_out_name(study, name))
#' @rdname ub_source_path
#' @export
ub_unblinded_path <- function(study, name) file.path(study$paths$unblinded_dir, ub_out_name(study, name))

.glob_match <- function(x, pattern) grepl(utils::glob2rx(pattern), x)

# Profile a dataset NAME gets from cfg$dataset_patterns (NULL when no pattern matches)
ub_pattern_profile <- function(name, cfg) {
  pats <- cfg$dataset_patterns
  for (p in names(pats)) {
    if (startsWith(p, "_")) next
    prof <- as.character(pats[[p]])
    if (.glob_match(toupper(name), toupper(p)) && !is.null(cfg$datasets[[prof]])) return(prof)
  }
  NULL
}

# Classify parameter codes of a param_numeric block (mirrors python config.param_class).
# Returns list(kind = character, key = character): kind in subject / scale / inverse / own / keep / unknown.
ub_param_class <- function(pn, codes, cfg) {
  c <- toupper(trimws(as.character(codes))); c[is.na(c)] <- ""
  kind <- rep(NA_character_, length(c)); key <- rep(NA_character_, length(c))
  sp <- pn$subject_params
  if (length(sp)) {
    spn <- toupper(names(sp)); hit <- match(c, spn)
    kind[!is.na(hit)] <- "subject"; key[!is.na(hit)] <- vapply(sp[hit[!is.na(hit)]], as.character, "")
  }
  tbl <- if (!is.null(pn$table)) cfg$param_scaling_tables[[pn$table]] else NULL
  for (pair in list(c("scale", "scale"), c("inverse", "inverse"), c("keep", "unscaled"), c("own", "own_factor"))) {
    m <- is.na(kind) & c %in% toupper(.chr(tbl[[pair[2]]]))
    kind[m] <- pair[1]
    if (pair[1] == "own") key[m] <- paste0("PARAM:", c[m])
  }
  kind[is.na(kind)] <- if (identical(pn$unlisted, "keep")) "keep" else "unknown"
  list(kind = kind, key = key)
}

ub_suppressed_columns <- function(columns, dspec, cfg) {
  pats <- toupper(c(.chr(cfg$common_suppress_vars), .chr(dspec$suppress_vars)))
  id_cols <- c(toupper(.chr(dspec$id_vars)), toupper(.chr(dspec$keep_vars)))
  out <- character()
  for (c in columns) {
    cu <- toupper(c)
    if (cu %in% id_cols) next
    if (any(vapply(pats, function(p) .glob_match(cu, p), logical(1)))) out <- c(out, c)
  }
  out
}
