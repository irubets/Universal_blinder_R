# Pre-flight: before the first run, look at the datasets in a study folder's input/ and list what the
# blinding profiles will not cover, and the known study-specific traps (placebo doses, rare demographics,
# BSA-based doses, results text, PC without results, ...). Only reads; writes nothing.
#
# Every column of the result except sample_values holds names and counts only. sample_values holds up to
# five REAL values from the data (PRIVATE): strip it before anything is sent outside the computer.

.UB_PF_ID <- c("STUDYID", "USUBJID", "SUBJID", "SITEID")
.UB_PF_ANALYTE <- c("PCTESTCD", "PCTEST", "PARAMCD", "PARAM", "ATEST", "PPCAT")
.UB_PF_TRT <- c("EXTRT", "ECTRT", "TRTP", "TRTA", "TRT01P", "TRT01A", "TRT02P", "TRT02A", "TRT03P", "TRT03A", "ARM",
                "ACTARM", "ARMCD", "ACTARMCD", "ATRT", "TREAT", "SATRT", "COHORT", "COHORTCD", "INVNAM", "INVID")
.UB_PF_DATE <- paste0("\\b\\d{1,2}[ -]?(JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC)[A-Z]*[ -]?\\d{2,4}\\b",
                      "|\\b(19|20)\\d\\d-\\d\\d-\\d\\d\\b")
.UB_PF_EXPOSURE <- c("EX", "EC", "ADEX", "ADEC")
.UB_PF_TARGETS <- c("PC", "ADNCA", "ADPC")
.UB_PF_NUMARM <- "^(TRT([0-9]{2})?[PA]N|TRTSEQ[PA]N|ARMN|ACTARMN|ARMCDN|COHORTN|ATRTN|TREATN|TRTN|SATRTN)$"
.UB_PF_RESULT_VARS <- c("PCORRES", "PCSTRESC", "ARESC", "AVALC")

# every string in a (nested) profile, upper-cased: names and values
.ub_pf_strings <- function(o) {
  out <- character()
  if (is.list(o)) {
    if (!is.null(names(o))) out <- names(o)
    for (v in o) out <- c(out, .ub_pf_strings(v))
  } else if (is.character(o)) {
    out <- o
  }
  unique(toupper(out[!is.na(out) & nzchar(out)]))
}

.ub_pf_glob_any <- function(x, pats) {
  if (!length(pats)) return(rep(FALSE, length(x)))
  rx <- utils::glob2rx(toupper(pats))
  vapply(toupper(x), function(c) any(vapply(rx, function(p) grepl(p, c), logical(1))), logical(1), USE.NAMES = FALSE)
}

.ub_pf_samples <- function(x, n = 5) {
  x <- trimws(as.character(x))
  x <- unique(x[!is.na(x) & nzchar(x)])
  paste(utils::head(sort(x), n), collapse = " | ")
}

.ub_pf_json <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE))

.ub_pf_upper_names <- function(x) {
  if (is.null(x) || !length(x)) return(list())
  names(x) <- ifelse(names(x) == "*", "*", toupper(names(x)))
  x
}

.ub_pf_files <- function(input_dir) {
  if (!dir.exists(input_dir)) return(character())
  fs <- sort(list.files(input_dir))
  fs <- fs[!dir.exists(file.path(input_dir, fs))]
  stem <- tools::file_path_sans_ext(fs)
  fs[tolower(tools::file_ext(fs)) %in% c("xpt", "sas7bdat") & !grepl(".", stem, fixed = TRUE)]
}

#' Pre-flight check of a study folder before the first blinding run
#'
#' Reads every dataset in `FOLDER/input` and lists the columns the blinding profiles will not cover and the
#' known study-specific traps, each with a suggested setting. Writes nothing.
#'
#' @param folder the study folder (with `input/`)
#' @param options the study options (parsed `blind_options.json`: `dataset_aliases`, `config_overrides`,
#'   `drop_vars`, `extra_suppress_vars`, `blank_where`, `config`), or `NULL`. Suggestions already applied
#'   there are not repeated.
#' @return `data.frame(severity, code, dataset, variable, message, suggestion, sample_values)`. `severity` is
#'   "red" (will stop the release or leak), "amber" (check it) or "info". Only `sample_values` holds data values
#'   (up to five real values, PRIVATE); the other columns hold dataset/variable names and counts only.
#' @export
ub_preflight <- function(folder, options = NULL) {
  if (is.null(options)) options <- list()
  rows <- list()
  add <- function(severity, code, dataset, variable, message, suggestion = "", sample_values = "") {
    rows[[length(rows) + 1L]] <<- data.frame(severity = severity, code = code, dataset = dataset, variable = variable,
                                             message = message, suggestion = suggestion, sample_values = sample_values,
                                             stringsAsFactors = FALSE)
  }
  cfg_path <- options[["config", exact = TRUE]]
  if (!is.null(cfg_path) && !grepl("^([A-Za-z]:|/|\\\\)", cfg_path)) cfg_path <- file.path(folder, cfg_path)
  cfg <- ub_load_config(cfg_path)
  if (!is.null(options$config_overrides)) cfg <- .merge_list(cfg, options$config_overrides)
  aliases <- .ub_pf_upper_names(options$dataset_aliases)
  drop <- .ub_pf_upper_names(options$drop_vars)
  extra <- .ub_pf_upper_names(options$extra_suppress_vars)
  blank <- if (is.null(options$blank_where)) list() else options$blank_where

  input_dir <- file.path(folder, "input")
  files <- .ub_pf_files(input_dir)
  names_u <- toupper(tools::file_path_sans_ext(files))
  if (!length(files)) {
    add("red", "no_datasets", "", "", "There are no .xpt or .sas7bdat files in the input folder.",
        "Copy the datasets to blind into the input folder.")
    return(do.call(rbind, rows))
  }
  if (!length(intersect(names_u, .UB_PF_EXPOSURE)))
    add("amber", "no_exposure", "", "",
        "No exposure dataset (EX, EC, ADEX or ADEC) in the input folder: drug names in treatment text will not be learned.",
        "Copy an exposure dataset (ex.xpt, ec.xpt or adex.xpt) into the input folder, if the study has one.")
  lab <- tryCatch(ub_lab_files(folder), error = function(e) character())
  pcr <- tryCatch(ub_check_pc_results(folder), error = function(e) NULL)
  if (!is.null(pcr) && identical(pcr$status, "lab_file_not_merged") && !.ub_lab_release_on(options))
    add("info", "workflow3_possible", "PC", "PCORRES",
        paste0("PC has no concentrations and a lab results file is in input (", paste(pcr$lab_files, collapse = ", "),
               "). Put the results into PC (Prepare PC, Workflow 2); if they can't be matched to PC, release the lab ",
               "file blinded next to PC instead (Workflow 3)."),
        "Prepare PC tab: put the results into PC (Workflow 2), or turn on 'release the lab file blinded instead' (Workflow 3).",
        paste(pcr$lab_files, collapse = ", "))

  for (i in seq_along(files)) {
    n <- names_u[i]
    prof_name <- if (!is.null(cfg$datasets[[n]])) n else if (!is.null(aliases[[n]])) toupper(as.character(aliases[[n]]))
                 else ub_pattern_profile(n, cfg)
    p <- if (is.null(prof_name)) NULL else cfg$datasets[[prof_name]]
    if (is.null(p)) {
      add("red", "no_profile", n, "",
          paste0("Dataset ", n, " has no blinding profile, so the run will stop."),
          paste0("Rename the file to a supported name (", paste(sort(names(cfg$datasets)), collapse = ", "),
                 "), or add dataset_aliases, e.g. ", .ub_pf_json(stats::setNames(list("ADNCA"), n)),
                 ", or leave the file out."))
      next
    }
    df <- tryCatch(ub_read_dataset(file.path(input_dir, files[i]))$data, error = function(e) e)
    if (inherits(df, "error")) {
      add("red", "unreadable", n, "", paste0("Dataset ", n, " cannot be read."),
          "Check the file (the error message is in sample_values).", conditionMessage(df))
      next
    }
    names(df) <- toupper(names(df))
    cols <- names(df)
    kind <- if (prof_name %in% .UB_PF_TARGETS || n %in% .UB_PF_TARGETS) "target" else "helper"
    tsev <- if (kind == "target") "amber" else "info"
    sup <- c(.chr(cfg$common_suppress_vars), .chr(p$suppress_vars), .chr(extra[["*"]]), .chr(extra[[n]]))
    dropped_pats <- c(.chr(drop[["*"]]), .chr(drop[[n]]))
    dropped <- function(v) .ub_pf_glob_any(v, dropped_pats)
    suppressed <- function(v) .ub_pf_glob_any(v, sup)
    covered_pats <- c(.ub_pf_strings(p), toupper(sup), toupper(dropped_pats))
    if (!is.null(p$param_numeric)) covered_pats <- c(covered_pats, "PARAMCD")   # PP/ADPP: PK parameter codes
    covered <- function(v) .ub_pf_glob_any(v, covered_pats)
    # a blank_where rule counts only when its condition variables exist in this dataset (otherwise it never applies)
    blanked <- function(v) any(vapply(blank, function(r) .ub_rule_applies(r, n) && v %in% toupper(.chr(r$vars)) &&
                                        all(toupper(names(r$when)) %in% toupper(cols)), logical(1)))
    handled <- character()
    is_adam_conc <- prof_name %in% c("ADNCA", "ADPC") || n %in% c("ADNCA", "ADPC")
    over <- function(x) paste0("config_overrides: ", .ub_pf_json(list(datasets = stats::setNames(list(x), n))))

    # ---- ADNCA/ADPC carrying the SDTM analyte columns but no ATEST ----
    if (is_adam_conc && !"ATEST" %in% cols) {
      pv <- intersect(c("PCTESTCD", "PCTEST"), cols)
      pv <- pv[!covered(pv)]
      if (length(pv)) {
        ev <- stats::setNames(as.list(ifelse(pv == "PCTESTCD", "analyte_code", "analyte_name")), pv)
        add("red", "pctest_without_atest", n, paste(pv, collapse = ", "),
            paste0(n, " has ", paste(pv, collapse = " and "), " but no ATEST; the profile codes only PARAMCD and ATEST, ",
                   "so the analyte names would stay readable (QC residual_identity_text)."),
            over(list(exact_vars = ev)), .ub_pf_samples(unlist(df[pv])))
        handled <- c(handled, pv)
      }
    }

    # ---- ADNCA/ADPC treatment text outside the profile ----
    if (is_adam_conc) {
      want <- c(EXTRT = "treatment", TRTP = "treatment", TRTA = "treatment", APROFILE = "any", PARCAT1 = "any")
      tv <- p$text_vars
      for (v in intersect(names(want), cols)) {
        role <- if (is.null(tv[[v]])) "" else as.character(tv[[v]])
        if (role %in% c(want[[v]], "any") || dropped(v) || suppressed(v)) next
        red <- v %in% c("EXTRT", "TRTP", "TRTA") && !covered(v)
        add(if (red) "red" else "amber", "adam_treatment_text", n, v,
            if (red) paste0(v, " is not in the ", n, " profile, so treatment names in it would stay readable.")
            else paste0(v, " is coded only for analyte names; it often also names a treatment or placebo."),
            paste0(over(list(text_vars = stats::setNames(list(want[[v]]), v))),
                   if (want[[v]] == "treatment") " (and keep an exposure dataset in the input folder)" else ""),
            .ub_pf_samples(df[[v]]))
        handled <- c(handled, v)
      }
    }

    # ---- identifying columns the profile does not cover ----
    gaps <- cols[(cols %in% c(.UB_PF_ID, .UB_PF_ANALYTE, .UB_PF_TRT)) & !covered(cols) & !cols %in% handled]
    for (v in gaps) {
      if (v %in% .UB_PF_ID) {
        key <- tolower(v)
        code <- if (prof_name == "PC" && v == "SUBJID") "pc_subjid" else "id_not_covered"
        sug <- over(list(id_vars = stats::setNames(list(v), key)))
      } else if (v %in% .UB_PF_ANALYTE) {
        code <- "analyte_not_covered"
        sug <- if (v %in% c("PCTESTCD", "PARAMCD")) over(list(exact_vars = stats::setNames(list("analyte_code"), v)))
               else if (v %in% c("PCTEST", "ATEST")) over(list(exact_vars = stats::setNames(list("analyte_name"), v)))
               else over(list(text_vars = stats::setNames(list("analyte"), v)))
      } else {
        code <- "treatment_not_covered"
        sug <- paste0(over(list(text_vars = stats::setNames(list("treatment"), v))), ", or drop_vars ",
                      .ub_pf_json(stats::setNames(list(list(v)), n)))
      }
      add("red", code, n, v, paste0(v, " identifies a subject, analyte or treatment but the ", n,
                                    " profile does not cover it: it would be a QC FAIL."), sug, .ub_pf_samples(df[[v]]))
    }

    # ---- analyte code without its name / name equal to its code ----
    pair <- .chr(p$analyte_pair)
    if (length(pair) == 2 && pair[1] %in% cols && !pair[2] %in% cols && is.null(p$param_numeric))
      add("amber", "analyte_code_without_name", n, pair[1],
          paste0(pair[1], " has no ", pair[2], " next to it. If ", pair[1], " is another code of a PC analyte, it must get ",
                 "the SAME code: confirm its CHECK row in the codebook."),
          "At codebook review, give this code the same ANL code as the PC analyte.", .ub_pf_samples(df[[pair[1]]], 3))
    if (length(pair) == 2 && all(pair %in% cols)) {
      a <- trimws(as.character(df[[pair[1]]])); b <- trimws(as.character(df[[pair[2]]]))
      same <- unique(a[nzchar(a) & toupper(a) == toupper(b)])
      if (length(same))
        add("info", "name_equals_code", n, paste(pair, collapse = ", "),
            paste0(pair[2], " equals ", pair[1], " for ", length(same), " analyte(s)."),
            "Nothing to do with a current package; if blinding stops with 'not in the key for role analyte_name', update the package.",
            .ub_pf_samples(same))
    }

    # ---- results text ----
    for (v in intersect(.UB_PF_RESULT_VARS, cols)) {
      if (!is.character(df[[v]]) || dropped(v)) next
      x <- trimws(df[[v]]); x <- x[nzchar(x)]
      if (!length(x)) next
      bql <- grepl("^(BQL|BLQ)<\\(", x, ignore.case = TRUE)
      num <- grepl(.UB_PPC_NUMBER, x, perl = TRUE)
      blq <- grepl("^BLQ", x[bql], ignore.case = TRUE)
      if (any(blq))
        add("info", "blq_spelling", n, v, paste0(sum(blq), " value(s) use the spelling BLQ<(x) (", sum(bql) - sum(blq),
                                                  " use BQL<(x))."),
            "Both spellings are blinded with the LLOQ factor; after the run, check that the LLOQ in these values changed.",
            .ub_pf_samples(x[bql][blq]))
      odd <- x[!bql & !num]
      if (length(odd))
        add("amber", "result_text_unrecognised", n, v,
            paste0(length(odd), " value(s) (", length(unique(odd)), " distinct) are neither numbers nor BQL<(x) / BLQ<(x); ",
                   "they are not rescaled and are released as they are."),
            "If a value hides a concentration or LLOQ (e.g. '<0.5'), correct it before blinding; otherwise check it in QC.",
            .ub_pf_samples(odd))
    }

    # ---- PC delivered without results (Workflow 2) ----
    if (prof_name == "PC") {
      rv <- intersect(c("PCORRES", "PCSTRESC", "PCSTRESN"), cols)
      filled <- vapply(rv, function(v) { x <- df[[v]]; if (is.character(x)) any(nzchar(trimws(x))) else any(!is.na(x)) }, logical(1))
      if (!any(filled))
        add("red", "pc_without_results", n, "PCORRES, PCSTRESC, PCSTRESN",
            paste0("PC has the sample records but no results (", if (length(rv)) "PCORRES/PCSTRESC/PCSTRESN are empty"
                   else "no result columns", "). ", length(lab), " results file(s) are in the input folder."),
            "Workflow 2: copy the lab's results file (CSV or Excel) into the input folder and fill PC from it first (ub_prepare_pc).",
            paste(lab, collapse = " | "))
    }

    # ---- ADNCA with PCORRES but no ARESC: precision of AVAL ----
    an <- p$analyte_numeric
    if (!is.null(an) && identical(as.character(an$precision_from), "ARESC") && !"ARESC" %in% cols && "PCORRES" %in% cols)
      add("amber", "precision_from_pcorres", n, "PCORRES",
          paste0(n, " has PCORRES but no ARESC: AVAL would get one shared precision and differ from the blinded PC by rounding."),
          over(list(analyte_numeric = list(precision_from = "PCORRES"))))

    # ---- DOSEA / DOSEP: a planned dose per m2 next to the actual dose in mg gives the real body surface area ----
    # A DOSEP level whose median DOSEA/DOSEP is between 1.1 and 3 is a dose per m2 (whatever the unit columns say).
    # DOSEA is strongly suggested (it gives the BSA even without DOSEP, to anyone who knows the protocol dose);
    # DOSEP is a separate row, so either or both can be highlighted and applied.
    if (all(c("DOSEA", "DOSEP") %in% cols) && !dropped("DOSEA") && !suppressed("DOSEA")) {
      a <- suppressWarnings(as.numeric(df$DOSEA)); pl <- suppressWarnings(as.numeric(df$DOSEP))
      ok <- !is.na(a) & !is.na(pl) & a > 0 & pl > 0
      lv <- numeric()
      if (any(ok)) {
        med <- tapply(a[ok] / pl[ok], pl[ok], stats::median)
        lv <- as.numeric(names(med)[med >= 1.1 & med <= 3])
      }
      hit <- ok & pl %in% lv
      ns <- if ("USUBJID" %in% cols) length(unique(df$USUBJID[hit])) else sum(hit)
      if (length(lv) && ns >= 3) {
        p_gone <- dropped("DOSEP") || suppressed("DOSEP") || blanked("DOSEP")
        add(if (p_gone) "amber" else "red", "dose_reveals_bsa", n, "DOSEA",
            paste0("STRONGLY RECOMMENDED: leave DOSEA out. For ", ns, " subject(s), DOSEP is a dose per m2 (",
                   length(lv), " level(s), see sample_values): DOSEA divided by DOSEP, or by the protocol dose, gives ",
                   "each subject's real body surface area.",
                   if (!p_gone) " With DOSEA and DOSEP both released, QC fails (dose_ratio_reveals_bsa)."),
            paste0("drop_vars ", .ub_pf_json(stats::setNames(list(list("DOSEA")), n)), "."), .ub_pf_samples(lv))
        if (!p_gone)
          add("amber", "dose_reveals_bsa", n, "DOSEP",
              paste0("Optional, in addition to DOSEA: leave DOSEP out. Leaving out only DOSEP stops the QC failure, ",
                     "but DOSEA still gives the body surface area to anyone who knows the protocol dose."),
              paste0("drop_vars ", .ub_pf_json(stats::setNames(list(list("DOSEP")), n)), "."), .ub_pf_samples(lv))
        handled <- c(handled, "DOSEA", "DOSEP")
      }
    }

    # ---- doses and numeric arm codes that can reveal placebo ----
    for (v in cols) {
      x <- df[[v]]
      if (!is.numeric(x) || dropped(v) || suppressed(v)) next
      if (grepl("DOS", v) && !grepl("(U|FRQ|FRM|TXT)$", v)) {
        if (any(!is.na(x) & x == 0))
          add(tsev, "dose_zero", n, v,
              paste0(v, " is 0 on ", sum(!is.na(x) & x == 0), " record(s) (", length(unique(df$USUBJID[!is.na(x) & x == 0])),
                     " subject(s)); a dose of 0 shows who had placebo."),
              paste0("drop_vars ", .ub_pf_json(stats::setNames(list(list(v)), n)), " (or remove the placebo records)."),
              .ub_pf_samples(x))
        unit <- setdiff(intersect(c(paste0(v, "U"), sub("E$", "U", v), sub("DOSE", "DOSU", v)), cols), v)
        uval <- if (length(unit)) unlist(df[unit]) else character()
        lab_ <- attr(x, "label"); lab_ <- if (is.null(lab_)) "" else lab_
        if (v %in% handled) {
          # DOSEA/DOSEP already reported above (dose_reveals_bsa)
        } else if ((any(grepl("M\\^?2|M\\*\\*2", toupper(uval))) || grepl("M\\^?2", toupper(lab_))) && !blanked(v))
          add(tsev, "dose_per_bsa", n, v,
              paste0(v, " is a dose per body surface area (mg/m2): if a dose in mg is also released, the two together give the real BSA."),
              paste0("drop_vars ", .ub_pf_json(stats::setNames(list(list(v)), n)), ", or blank_where for the records with a mg dose."),
              .ub_pf_samples(uval))
        else if (v == "DOSEP" && !length(unit) && !blanked(v))
          add("info", "dose_unit_unknown", n, v,
              "DOSEP (planned dose) has no unit variable; if it is in mg/m2 it reveals the body surface area.",
              paste0("If it is mg/m2: drop_vars ", .ub_pf_json(stats::setNames(list(list(v)), n)), "."))
      }
      if (grepl(.UB_PF_NUMARM, v))
        add(tsev, "numeric_arm_code", n, v,
            paste0(v, " is a numeric treatment/arm code (", length(unique(x[!is.na(x)])), " distinct values); ",
                   "an odd code can show the placebo group."),
            paste0("drop_vars ", .ub_pf_json(stats::setNames(list(list(v)), n)), "."), .ub_pf_samples(x))
    }

    # ---- rare demographics ----
    for (v in intersect(c("RACE", "ETHNIC"), cols)) {
      if (dropped(v) || suppressed(v)) next
      add(tsev, "rare_demographics", n, v,
          paste0(v, " has ", length(unique(df[[v]][nzchar(trimws(as.character(df[[v]])))])),
                 " distinct values; rare categories can identify a subject."),
          paste0("drop_vars ", .ub_pf_json(stats::setNames(list(list(v)), n)), "."), .ub_pf_samples(df[[v]]))
    }

    # ---- text columns quoting calendar dates ----
    for (v in cols) {
      if (!is.character(df[[v]]) || suppressed(v) || dropped(v) || grepl("(DTC|DTM|DT)$", v)) next
      hit <- grepl(.UB_PF_DATE, df[[v]], ignore.case = TRUE, perl = TRUE)
      if (any(hit))
        add("amber", "free_text_date", n, v,
            paste0(v, " is text containing calendar dates on ", sum(hit), " record(s): expect QC free_text_date."),
            paste0("extra_suppress_vars ", .ub_pf_json(stats::setNames(list(list(v)), n)),
                   " (blanked; the originals come back on retrieval)."), .ub_pf_samples(df[[v]][hit]))
    }
  }
  out <- if (length(rows)) do.call(rbind, rows) else
    data.frame(severity = character(), code = character(), dataset = character(), variable = character(),
               message = character(), suggestion = character(), sample_values = character(), stringsAsFactors = FALSE)
  ord <- order(match(out$severity, c("red", "amber", "info")), seq_len(nrow(out)))
  out <- out[ord, , drop = FALSE]
  rownames(out) <- NULL
  out
}

# A pre-flight suggestion as settings to apply: data.frame(path, value) with one row per leaf setting
# (e.g. path "drop_vars/ADNCA", value '["RACE"]'), or NULL when the suggestion is not a single setting:
# plain advice, a condition ("If it is mg/m2: ..."), or a choice ("..., or blank_where ...", "(or remove ...)").
.ub_pf_setting <- function(s) {
  s <- if (length(s) && !is.na(s[1])) trimws(as.character(s[1])) else ""
  m <- regmatches(s, regexec("^(config_overrides|drop_vars|extra_suppress_vars):?\\s*\\{", s))[[1]]
  if (length(m) != 2) return(NULL)
  start <- nchar(m[1])
  ch <- strsplit(substring(s, start), "")[[1]]
  depth <- cumsum((ch == "{") - (ch == "}"))
  end <- which(depth == 0)[1]
  if (is.na(end)) return(NULL)
  rest <- substring(s, start + end)
  if (grepl("\\bor\\b", rest, ignore.case = TRUE)) return(NULL)
  val <- tryCatch(jsonlite::fromJSON(substring(s, start, start + end - 1L), simplifyVector = FALSE),
                  error = function(e) NULL)
  if (is.null(val)) return(NULL)
  flat <- function(x, p) {
    if (is.list(x) && length(x) && !is.null(names(x)) && all(nzchar(names(x))))
      return(do.call(rbind, lapply(names(x), function(k) flat(x[[k]], c(p, k)))))
    data.frame(path = paste(p, collapse = "/"), value = as.character(jsonlite::toJSON(x, auto_unbox = TRUE)),
               stringsAsFactors = FALSE)
  }
  flat(val, m[2])
}

#' Apply pre-flight suggestions to a study's settings
#'
#' Each suggestion that is a single setting (`drop_vars`, `extra_suppress_vars`, `config_overrides`) is written to
#' `blind_options.json`, with a backup of the previous version. List settings such as `drop_vars` are merged with
#' what is there. The datasets in `input` are never changed. Suggestions that offer a choice or a condition, or
#' that are plain advice, are not applied: their positions are returned in `choose`.
#' @param folder the UB study folder
#' @param suggestion the `suggestion` texts of the selected [ub_preflight()] rows
#' @return list(applied = number of suggestions applied, choose = positions not applied, problems = character())
#' @export
ub_preflight_apply <- function(folder, suggestion) {
  sp <- lapply(suggestion, .ub_pf_setting)
  ok <- !vapply(sp, is.null, logical(1))
  choose <- which(!ok)
  if (!any(ok)) return(list(applied = 0L, choose = choose, problems = character()))
  d <- do.call(rbind, sp[ok])
  r <- ub_ai_apply_settings(folder, d$path, d$value)
  list(applied = if (length(r$problems)) 0L else sum(ok), choose = choose, problems = r$problems)
}
