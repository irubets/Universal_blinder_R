# AI layer, part 4: tasks. Each task builds its prompt from parts read by the functions below; the class of a
# part is fixed here by its source (spec Section 7.2), then the gate (llm_gate.R) decides what is sent.

#' The AI tasks
#' @return data.frame(task, label, claude_allowed, description)
#' @export
ub_ai_tasks <- function() {
  data.frame(
    task = c("workflow", "triage", "codebook", "settings", "review", "unexpected"),
    label = c("Ask about the workflow", "Triage pre-flight / QC", "Suggest codebook decisions", "Suggest settings",
              "Review blinded output", "Check unexpected inputs"),
    claude_allowed = c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE),
    description = c(
      "Questions about the two workflows and the app. Sends the user guide and the study status (masked).",
      "Explains pre-flight and QC findings and what to change. Claude gets masked findings, column names and the settings structure; the local model also gets the unmasked details and sample values.",
      "Proposes codes for the codebook rows that need a decision. Needs the real names, so only the local model can do it.",
      "Proposes changes to blind_options.json. Claude gets masked findings and column names; the local model also gets sample values.",
      "Reviews the released (blinded) datasets in output/ and lists findings for Retrieve. Needs QC PASS/WARN; Claude also needs your one-time confirmation.",
      "Compares the study with patterns from earlier studies (pre-flight detector). Claude gets the structure only; the local model also gets sample values."),
    stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------------------------------------------
# sources (each returns one part; the class is set by what the function reads)
# ---------------------------------------------------------------------------------------------------------------
.UB_BUILTIN_DOCS <- paste(
  "Universal Blinder: makes blinded, reversible copies of SDTM/ADaM PK datasets for sharing.",
  "Study folder UB_<StudyNumber>: input/ (unblinded datasets, private), output/ (released blinded datasets),",
  "private/ (the key: maps, codebook.csv, logs, QC reports; never share), blind_options.json (settings).",
  "Workflow 1 (PC with results): copy PC, ADNCA/ADPC, exposure datasets (EX/EC/ADEX) and ADSL into input,",
  "review the codebook (rows with a CHECK note need a decision), run Blind & QC, read qc_summary.md, share output/.",
  "Workflow 2 (PC without results plus a separate lab file): copy the lab CSV/Excel file into input, run Prepare PC",
  "(column mapping, visit_map, time_map, analytes) to fill PC, then continue as Workflow 1.",
  "QC FAIL blocks the release; WARN needs a look. Retrieve maps findings on the blinded data back to original records.",
  "Settings: dataset_aliases, identities (treatment_aliases, analyte_aliases, analyte_code_aliases, private_terms),",
  "rename_vars, extra_suppress_vars, drop_vars, blank_where, config_overrides (e.g. ADNCA exact_vars PCTEST),",
  "qc_waivers (need a reason and a reviewer), prepare_pc.", sep = "\n")

.ub_ai_docs_text <- function(max_chars = Inf) {
  cand <- c(system.file("app", "www", "docs", "USERGUIDE.md", package = "universalblinder"),
            system.file("app", "www", "docs", "README.md", package = "universalblinder"),
            system.file("USERGUIDE.md", package = "universalblinder"), system.file("README.md", package = "universalblinder"))
  cand <- unique(cand[nzchar(cand) & file.exists(cand)])
  txt <- if (length(cand)) paste(unlist(lapply(cand, readLines, warn = FALSE, encoding = "UTF-8")), collapse = "\n") else .UB_BUILTIN_DOCS
  if (nchar(txt) > max_chars) txt <- paste0(substr(txt, 1, max_chars), "\n[... user guide shortened]")
  txt
}

.ub_src_docs <- function(short) .ub_part("PUBLIC", "User guide", .ub_ai_docs_text(if (short) 6000 else 60000))

.ub_ai_options <- function(folder) {
  if (exists("ub_read_options", mode = "function"))
    return(tryCatch(get("ub_read_options", mode = "function")(folder), error = function(e) .ub_ai_read_options(folder)))
  .ub_ai_read_options(folder)
}

.ub_ai_input_files <- function(folder) {
  list.files(file.path(folder, "input"), pattern = "\\.(xpt|sas7bdat)$", ignore.case = TRUE, full.names = TRUE)
}

.ub_ai_codebook_raw <- function(folder) {
  p <- file.path(folder, "private", "codebook.csv")
  if (!file.exists(p)) return(NULL)
  t <- tryCatch(utils::read.csv(p, colClasses = "character", na.strings = character(), check.names = FALSE,
                                fileEncoding = "UTF-8-BOM"), error = function(e) NULL)
  if (is.null(t)) return(NULL)
  names(t) <- toupper(trimws(names(t)))
  for (c in c("TYPE", "ACTUAL_VALUE", "BLINDED_VALUE", "NOTE")) if (is.null(t[[c]])) t[[c]] <- rep("", nrow(t))
  t$ROW_KEY <- paste(toupper(trimws(t$TYPE)), trimws(t$ACTUAL_VALUE), sep = "|")
  t
}

.ub_ai_check_rows <- function(cb) {
  if (is.null(cb) || !nrow(cb)) return(logical())
  chk <- if (!is.null(cb$CHECK)) nzchar(trimws(cb$CHECK)) else rep(FALSE, nrow(cb))
  chk | trimws(cb$BLINDED_VALUE) %in% c("?", "") | grepl("CHECK|possible abbreviation|other spelling", cb$NOTE, ignore.case = TRUE)
}

# study status (PUBLIC: names, counts, statuses)
.ub_src_status <- function(folder) {
  lines <- character()
  st <- if (exists("ub_study_status", mode = "function"))
    tryCatch(get("ub_study_status", mode = "function")(folder), error = function(e) NULL) else NULL
  fs <- .ub_ai_input_files(folder)
  opts <- .ub_ai_options(folder)
  cfg <- .ub_ai_cfg(folder, opts)
  lines <- c(lines, "Datasets in input:")
  for (f in fs) {
    nm <- toupper(tools::file_path_sans_ext(basename(f)))
    prof <- .ub_ai_profile(nm, cfg, opts$dataset_aliases)
    lines <- c(lines, sprintf("- %s: profile %s", basename(f), if (is.na(prof)) "none (set a dataset alias)" else prof))
  }
  if (!length(fs)) lines <- c(lines, "- none")
  lab <- ub_lab_files(folder)
  lines <- c(lines, paste0("Lab result files in input: ", if (length(lab)) paste(lab, collapse = ", ") else "none"))
  if (!is.null(st$pc$message)) lines <- c(lines, paste0("PC results check: ", st$pc$message))
  maps <- file.path(folder, "private", "maps")
  lines <- c(lines, paste0("Key present: ", if (dir.exists(maps) && length(list.files(maps))) "yes" else "no"))
  cb <- .ub_ai_codebook_raw(folder)
  lines <- c(lines, if (is.null(cb)) "Codebook: not written yet (written by the first run)" else
    sprintf("Codebook: %d rows, %d need a decision (CHECK)", nrow(cb), sum(.ub_ai_check_rows(cb))))
  qc <- .ub_ai_qc_status(folder)
  lines <- c(lines, paste0("Last released QC result: ", if (is.na(qc)) "none (nothing released)" else qc))
  .ub_part("PUBLIC", "Study status", lines)
}

.ub_ai_read_inputs <- function(folder) {
  fs <- .ub_ai_input_files(folder)
  out <- lapply(fs, function(f) tryCatch(ub_read_dataset(f), error = function(e) NULL))
  names(out) <- toupper(tools::file_path_sans_ext(basename(fs)))
  Filter(Negate(is.null), out)
}

# column names, types, labels, % missing (PUBLIC structure); with samples = TRUE a PRIVATE part with values
.ub_src_structure <- function(folder, samples = FALSE, n_samples = 5L) {
  ds <- .ub_ai_read_inputs(folder)
  pub <- character(); priv <- character()
  for (nm in names(ds)) {
    d <- ds[[nm]]$data; lab <- ds[[nm]]$meta$labels
    pub <- c(pub, sprintf("%s: %d rows, %d columns", nm, nrow(d), ncol(d)))
    for (c in names(d)) {
      x <- d[[c]]
      miss <- if (is.character(x)) mean(is.na(x) | !nzchar(trimws(x))) else mean(is.na(x))
      pub <- c(pub, sprintf("  %s (%s%s) missing %d%%", c, if (is.character(x)) "char" else "num",
                            if (!is.null(lab[[c]]) && nzchar(lab[[c]])) paste0(", \"", lab[[c]], "\"") else "",
                            if (length(x)) round(100 * miss) else 0L))
      if (samples) {
        u <- unique(as.character(x[!is.na(x)])); u <- u[nzchar(trimws(u))]
        if (length(u)) priv <- c(priv, sprintf("%s.%s: %s", nm, c, paste(utils::head(u, n_samples), collapse = " | ")))
      }
    }
  }
  if (!length(pub)) pub <- "No datasets in input."
  parts <- list(.ub_part("PUBLIC", "Dataset structure (input)", pub))
  if (samples && length(priv)) parts[[2]] <- .ub_part("PRIVATE", "Sample values (input)", priv)
  parts
}

.ub_src_columns <- function(folder) {
  ds <- .ub_ai_read_inputs(folder)
  lines <- vapply(names(ds), function(nm) paste0(nm, ": ", paste(names(ds[[nm]]$data), collapse = " ")), "")
  .ub_part("PUBLIC", "Column names (input)", if (length(lines)) lines else "No datasets in input.")
}

.ub_src_settings <- function(folder) {
  opts <- .ub_ai_options(folder)
  .ub_part("PUBLIC", "Settings (blind_options.json)",
           as.character(jsonlite::toJSON(opts, auto_unbox = TRUE, pretty = TRUE, null = "null")))
}

# pre-flight findings: the messages are PUBLIC (masked for Claude), sample_values PRIVATE
.ub_src_preflight <- function(folder, samples = FALSE) {
  f <- NULL
  if (exists("ub_preflight", mode = "function"))
    f <- tryCatch(get("ub_preflight", mode = "function")(folder, .ub_ai_options(folder)), error = function(e) NULL)
  if (is.null(f)) f <- .ub_ai_mini_preflight(folder)
  if (!is.data.frame(f) || !nrow(f)) return(list(.ub_part("PUBLIC", "Pre-flight findings", "No pre-flight findings.")))
  col <- function(n) if (is.null(f[[n]])) rep("", nrow(f)) else as.character(f[[n]])
  pub <- sprintf("- [%s] %s %s.%s: %s%s", col("severity"), col("code"), col("dataset"), col("variable"), col("message"),
                 ifelse(nzchar(col("suggestion")), paste0(" | suggestion: ", col("suggestion")), ""))
  parts <- list(.ub_part("PUBLIC", "Pre-flight findings", pub))
  sv <- col("sample_values")
  if (samples && any(nzchar(sv)))
    parts[[2]] <- .ub_part("PRIVATE", "Pre-flight sample values",
                           sprintf("- %s %s.%s: %s", col("code"), col("dataset"), col("variable"), sv)[nzchar(sv)])
  parts
}

# used only when preflight.R is not available: structural checks of Section 6 that need no values
.ub_ai_mini_preflight <- function(folder) {
  opts <- .ub_ai_options(folder); cfg <- .ub_ai_cfg(folder, opts)
  ds <- .ub_ai_read_inputs(folder)
  r <- list()
  add <- function(sev, code, d, v, msg, sug) r[[length(r) + 1L]] <<- data.frame(severity = sev, code = code, dataset = d,
    variable = v, message = msg, suggestion = sug, sample_values = "", stringsAsFactors = FALSE)
  for (nm in names(ds)) {
    cols <- names(ds[[nm]]$data)
    prof <- .ub_ai_profile(nm, cfg, opts$dataset_aliases)
    if (is.na(prof)) add("red", "no_profile", nm, "", "dataset has no blinding profile", "dataset_aliases")
    if (!is.na(prof) && prof %in% c("ADNCA", "ADPC") && "PCTEST" %in% cols && !"ATEST" %in% cols &&
        is.null(cfg$datasets[[prof]]$exact_vars$PCTEST))
      add("red", "pctest_no_atest", nm, "PCTEST", "PCTEST present without ATEST: PCTEST would not be blinded",
          paste0("config_overrides/datasets/", prof, "/exact_vars/PCTEST = analyte_name"))
    if (identical(prof, "ADNCA") && "PCORRES" %in% cols && !"ARESC" %in% cols)
      add("amber", "precision_from", nm, "PCORRES", "PCORRES present but no ARESC",
          "config_overrides/datasets/ADNCA/analyte_numeric/precision_from = PCORRES")
    if (identical(prof, "PC") && "PCORRES" %in% cols && all(!nzchar(trimws(as.character(ds[[nm]]$data$PCORRES)))))
      add("amber", "pc_no_results", nm, "PCORRES", "PC has no concentrations (PCORRES empty)", "Prepare PC (Workflow 2)")
  }
  if (!length(r)) return(NULL)
  do.call(rbind, r)
}

# QC findings: the public text is PUBLIC (it is what output/qc_summary.md shows); the detail quotes values: PRIVATE
.ub_src_qc <- function(folder, details = FALSE) {
  p <- file.path(folder, "private", "qc", "qc_report.json")
  if (!file.exists(p)) return(list(.ub_part("PUBLIC", "QC findings", "No QC run yet.")))
  q <- tryCatch(.read_json(p), error = function(e) NULL)
  if (is.null(q)) return(list(.ub_part("PUBLIC", "QC findings", "QC report unreadable.")))
  F <- q$findings
  g <- function(x, n) .ub_or(as.character(x[[n]]), "")
  F <- Filter(function(x) g(x, "status") %in% c("FAIL", "WARN", "WAIVED"), F)
  pub <- c(paste0("QC overall: ", .ub_or(q$status, "?")),
           vapply(F, function(x) sprintf("- %s %s %s.%s: %s", g(x, "status"), g(x, "check"), g(x, "dataset"), g(x, "variable"),
                                         g(x, "public")), ""))
  parts <- list(.ub_part("PUBLIC", "QC findings", pub))
  if (details && length(F))
    parts[[2]] <- .ub_part("PRIVATE", "QC finding details",
                           vapply(F, function(x) sprintf("- %s %s.%s: %s", g(x, "check"), g(x, "dataset"), g(x, "variable"),
                                                         g(x, "detail")), ""))
  parts
}

.ub_src_finding <- function(finding) {
  if (is.null(finding)) return(NULL)
  if (is.data.frame(finding)) finding <- as.list(finding[1, , drop = FALSE])
  txt <- paste(paste0(names(finding), ": ", vapply(finding, function(v) paste(as.character(v), collapse = " "), "")), collapse = "\n")
  # the row comes from a pre-flight/QC table; a sample_values field is data, not a message
  sv <- finding$sample_values
  pub <- finding[setdiff(names(finding), "sample_values")]
  out <- list(.ub_part("PUBLIC", "Finding to explain",
                       paste(paste0(names(pub), ": ", vapply(pub, function(v) paste(as.character(v), collapse = " "), "")), collapse = "\n")))
  if (!is.null(sv) && any(nzchar(as.character(sv))))
    out[[2]] <- .ub_part("PRIVATE", "Finding sample values", paste("sample_values:", paste(as.character(sv), collapse = " ")))
  out
}

.ub_src_codebook <- function(folder) {
  cb <- .ub_ai_codebook_raw(folder)
  if (is.null(cb)) return(.ub_part("PRIVATE", "Codebook", "No codebook yet: run Blind & QC once to write it."))
  chk <- .ub_ai_check_rows(cb)
  cols <- intersect(c("ROW_KEY", "TYPE", "ACTUAL_VALUE", "BLINDED_VALUE", "CHECK", "NOTE"), names(cb))
  fmt <- function(d) utils::capture.output(utils::write.csv(d[, cols, drop = FALSE], row.names = FALSE))
  .ub_part("PRIVATE", "Codebook (private/codebook.csv)",
           c(sprintf("Rows needing a decision (%d):", sum(chk)), if (any(chk)) fmt(cb[chk, , drop = FALSE]) else "none",
             "", "All other rows (context):", if (any(!chk)) fmt(cb[!chk, , drop = FALSE]) else "none"))
}

.ub_src_question <- function(question) {
  if (is.null(question) || !nzchar(trimws(paste(question, collapse = "")))) return(NULL)
  .ub_part("PUBLIC", "Question", question)
}

# released datasets as CSV text, one entry per dataset (RELEASED)
.ub_ai_released_tables <- function(folder) {
  fs <- list.files(file.path(folder, "output"), pattern = "\\.xpt$", ignore.case = TRUE, full.names = TRUE)
  out <- list()
  for (f in fs) {
    d <- tryCatch(ub_read_dataset(f)$data, error = function(e) NULL)
    if (is.null(d)) next
    out[[toupper(tools::file_path_sans_ext(basename(f)))]] <- d
  }
  out
}

.ub_csv_lines <- function(d) {
  for (c in names(d)) if (is.numeric(d[[c]])) d[[c]] <- ifelse(is.na(d[[c]]), "", format(d[[c]], digits = 8, trim = TRUE, scientific = FALSE, drop0trailing = TRUE))
  utils::capture.output(utils::write.csv(d, row.names = FALSE, na = ""))
}

# split released datasets into blocks of at most BUDGET characters: whole dataset, else per subject, else per rows
.ub_ai_release_blocks <- function(tables, budget) {
  blocks <- list()
  for (nm in names(tables)) {
    d <- tables[[nm]]
    lines <- .ub_csv_lines(d)
    txt <- paste(lines, collapse = "\n")
    if (nchar(txt) <= budget) { blocks[[length(blocks) + 1L]] <- list(label = paste0(nm, " (all ", nrow(d), " rows)"), text = txt); next }
    header <- lines[1]; body <- lines[-1]
    subj <- if ("USUBJID" %in% names(d)) as.character(d$USUBJID) else rep("", nrow(d))
    cur <- character(); cur_n <- nchar(header); from <- 1L
    flush <- function(to) {
      if (length(cur)) blocks[[length(blocks) + 1L]] <<- list(label = sprintf("%s rows %d-%d of %d", nm, from, to, nrow(d)),
                                                             text = paste(c(header, cur), collapse = "\n"))
      cur <<- character(); cur_n <<- nchar(header); from <<- to + 1L
    }
    grp <- split(seq_along(body), factor(subj, levels = unique(subj)))
    for (g in grp) {
      gl <- body[g]; gn <- sum(nchar(gl) + 1L)
      if (cur_n + gn > budget && length(cur)) flush(g[1] - 1L)
      if (gn > budget) {                       # one subject larger than a block: by rows
        for (i in seq_along(gl)) {
          if (cur_n + nchar(gl[i]) + 1L > budget && length(cur)) flush(g[i] - 1L)
          cur <- c(cur, gl[i]); cur_n <- cur_n + nchar(gl[i]) + 1L
        }
      } else { cur <- c(cur, gl); cur_n <- cur_n + gn }
    }
    flush(nrow(d))
  }
  blocks
}

# ---------------------------------------------------------------------------------------------------------------
# system prompts
# ---------------------------------------------------------------------------------------------------------------
.UB_SYS_BASE <- paste(
  "You help a pharmacokineticist use the Universal Blinder, a tool that makes blinded, reversible copies of",
  "SDTM/ADaM PK datasets. Placeholders such as <SUBJECT_3>, <TREATMENT_1>, <ANALYTE_2>, <SITE_1>, <TERM_1>, <DATE>",
  "stand for private values that were removed before sending; refer to them as they are and never guess the real",
  "values. Codes such as TRT01, ANL01, ARM01, S0001, SITE001, STUDY-XXX are the blinded values. Answer in plain",
  "language for a non-programmer. Be brief and concrete.")

.UB_SYS_TASK <- list(
  workflow = "Answer the question about the workflow, using the user guide and the study status. Name the app tab and the next step.",
  triage = paste("Explain each pre-flight/QC finding: what it means, whether it must be fixed before blinding, and the setting",
                 "that fixes it. End with a fenced ```json block: a list of objects",
                 "{\"code\": <finding code or check>, \"explanation\": <one sentence>, \"suggested_setting\": <setting path = value, or \"\">}."),
  codebook = paste("Propose decisions for the codebook rows that need one (CHECK). BLINDED_VALUE rules: the code of the",
                   "treatment/analyte the spelling belongs to (e.g. TRT01, ANL02), '-' to leave the text as it is (not a",
                   "drug/analyte name), blank = next free code, '?' = still open. End with a fenced ```json block: a list of",
                   "objects {\"row_key\": <ROW_KEY exactly as given>, \"column\": \"BLINDED_VALUE\", \"value\": <new value>,",
                   "\"reason\": <short reason>}."),
  settings = paste("Suggest changes to the study settings (blind_options.json) that fix the findings. Known keys:",
                   "dataset_aliases, identities, rename_vars, extra_suppress_vars, drop_vars, blank_where, config_overrides,",
                   "qc_waivers (never suggest a waiver to get past a FAIL), prepare_pc. End with a fenced ```json block: a list",
                   "of objects {\"path\": <slash-separated path, e.g. config_overrides/datasets/ADNCA/exact_vars/PCTEST>,",
                   "\"value\": <JSON value>, \"reason\": <short reason>}."),
  review = paste("Review the blinded datasets for data problems a PK analyst would query: implausible or inconsistent values,",
                 "missing records, time/dose inconsistencies, duplicated records, BQL handling, unit problems. Use only the",
                 "data given. End with a fenced ```json block: a list of objects",
                 "{\"DATASET\": <dataset>, \"UBRECID\": <record id or \"\">, \"SUBJID\": <blinded SUBJID or \"\">,",
                 "\"FINDING\": <one sentence>}. Use an empty list when there is nothing to report."),
  unexpected = paste("Compare the study structure with what the Universal Blinder expects. Known patterns from earlier studies:",
                     "datasets with no profile (dataset_aliases); ADNCA/ADPC with PCTEST but no ATEST (config_overrides exact_vars",
                     "PCTEST = analyte_name); result text spelled BLQ<(x) or other BQL forms; PC with empty PCORRES (Workflow 2,",
                     "Prepare PC); ADNCA with PCORRES but no ARESC (analyte_numeric precision_from = PCORRES); dose or numeric",
                     "arm columns that reveal placebo (drop_vars); rare demographics RACE/ETHNIC, BSA via DOSEP (drop_vars);",
                     "PC carrying SUBJID (id_vars subjid); labels naming the drug in EXTRT/TRTP/TRTA/APROFILE/PARCAT1",
                     "(text_vars); lab files whose columns do not match the expected roles. Explain each finding and end with a",
                     "fenced ```json block: a list of objects {\"code\": <short code>, \"explanation\": <one sentence>,",
                     "\"suggested_setting\": <setting path = value, or \"\">}."))

.ub_ai_system <- function(task, short) {
  if (short) return(paste("You help with the Universal Blinder (blinding of PK datasets). Placeholders like <SUBJECT_1>",
                          "hide private values: keep them as they are. Be brief.", .UB_SYS_TASK[[task]]))
  paste(.UB_SYS_BASE, .UB_SYS_TASK[[task]], sep = "\n\n")
}

.ub_ai_render <- function(parts) {
  paste(vapply(parts, function(p) paste0("### ", p$label, "\n", p$text), ""), collapse = "\n\n")
}

# shorten the largest parts until the prompt fits (not for review: that is chunked)
.ub_ai_fit <- function(parts, budget) {
  size <- function() sum(vapply(parts, function(p) nchar(p$text) + nchar(p$label) + 6L, 1L))
  guard <- 0L
  while (size() > budget && guard < 50L) {
    guard <- guard + 1L
    n <- vapply(parts, function(p) nchar(p$text), 1L)
    i <- which.max(n)
    over <- size() - budget
    keep <- max(200L, n[i] - over - 100L)
    if (keep >= n[i]) break
    parts[[i]]$text <- paste0(substr(parts[[i]]$text, 1, keep), "\n[... shortened to fit the model's context]")
  }
  parts
}

#' Prepare an AI request (prompt preview and data gate)
#'
#' Builds the prompt of a task from parts whose class (PUBLIC / RELEASED / PRIVATE) is set by their source. For the
#' Claude API, PRIVATE parts are left out, PUBLIC parts are masked, and the whole prompt is leak-checked: any private
#' value left blocks the request (no override). Nothing is sent here.
#' @param task one of [ub_ai_tasks()]`$task`
#' @param folder the Universal Blinder folder of a study
#' @param conn a [ub_llm_connect()] connection (only its provider and context size are used)
#' @param question the user's question or instruction (optional)
#' @param context optional list, e.g. `finding` (one pre-flight or QC finding row from the app)
#' @return list(task, provider, allowed, blocked_reason, parts = data.frame(class, label, chars), system, user,
#'   size_chars, chunks, needs_consent, chunk_summary)
#' @export
ub_ai_prepare <- function(task, folder, conn, question = "", context = list()) {
  task <- match.arg(task, ub_ai_tasks()$task)
  stopifnot(inherits(conn, "ub_llm_conn"))
  folder <- normalizePath(folder, mustWork = FALSE)
  provider <- conn$provider
  claude <- provider == "claude"
  short <- conn$context_chars < 50000
  out <- list(task = task, provider = provider, allowed = TRUE, blocked_reason = "",
              parts = data.frame(class = character(), label = character(), chars = integer(), stringsAsFactors = FALSE),
              system = "", user = "", size_chars = 0L, chunks = list(), needs_consent = FALSE, chunk_summary = "")
  block <- function(reason, consent = FALSE) { out$allowed <- FALSE; out$blocked_reason <- reason; out$needs_consent <- consent; out }

  if (task == "codebook" && claude)
    return(block("Codebook decisions need the real treatment and analyte names, which are never sent to the Claude API: use the local model."))
  if (task == "review") {
    qc <- .ub_ai_qc_status(folder)
    if (is.na(qc) || !qc %in% c("PASS", "WARN"))
      return(block(paste0("Review needs released datasets from a run with QC PASS or WARN (last result: ",
                          if (is.na(qc)) "nothing released" else qc, "). Run Blind & QC first.")))
    if (claude && !ub_ai_consent_get(folder))
      return(block(paste("Sending the blinded datasets to the Claude API needs your one-time confirmation for this study:",
                         .UB_CONSENT_TEXT), consent = TRUE))
  }

  # PRIVATE parts are built for every provider; the gate below drops them for Claude
  parts <- list()
  addp <- function(p) if (!is.null(p)) parts[[length(parts) + 1L]] <<- p
  addl <- function(l) for (p in l) addp(p)
  addp(.ub_src_question(question))
  addl(.ub_src_finding(context$finding))
  switch(task,
    workflow = { addp(.ub_src_status(folder)); addp(.ub_src_docs(short)) },
    triage = { addl(.ub_src_preflight(folder, samples = TRUE)); addl(.ub_src_qc(folder, details = TRUE))
               addp(.ub_src_columns(folder)); addp(.ub_src_settings(folder)) },
    codebook = { addp(.ub_src_codebook(folder)); addp(.ub_src_columns(folder)) },
    settings = { addl(.ub_src_preflight(folder, samples = TRUE)); addl(.ub_src_qc(folder))
                 addp(.ub_src_columns(folder)); addp(.ub_src_settings(folder))
                 addl(.ub_src_structure(folder, samples = TRUE)[-1]) },
    review = { addp(.ub_part("RELEASED", "QC summary (output/qc_summary.md)",
                             readLines(file.path(folder, "output", "qc_summary.md"), warn = FALSE))) },
    unexpected = { addl(.ub_src_preflight(folder, samples = TRUE)); addl(.ub_src_structure(folder, samples = TRUE))
                   lab <- ub_lab_files(folder)
                   if (length(lab)) addp(.ub_part("PUBLIC", "Lab files in input", lab)) })

  values <- ub_ai_private_values(folder)
  g <- .ub_ai_gate_parts(parts, provider, values)
  parts <- g$parts
  system <- .ub_ai_system(task, short)
  if (claude) system <- ub_ai_mask(system, values)
  budget <- max(2000, conn$context_chars - nchar(system) - 500)

  chunks <- list()
  if (task == "review") {
    tables <- .ub_ai_released_tables(folder)
    head_txt <- .ub_ai_render(parts)
    blocks <- .ub_ai_release_blocks(tables, max(1000, budget - nchar(head_txt) - 200))
    if (!length(blocks)) return(block("There are no released datasets in output/."))
    # pack consecutive blocks into as few requests as fit
    room <- max(1000, budget - nchar(head_txt) - 200)
    groups <- list(); cur <- integer(); cur_n <- 0L
    for (i in seq_along(blocks)) {
      n <- nchar(blocks[[i]]$text) + nchar(blocks[[i]]$label) + 40L
      if (length(cur) && cur_n + n > room) { groups[[length(groups) + 1L]] <- cur; cur <- integer(); cur_n <- 0L }
      cur <- c(cur, i); cur_n <- cur_n + n
    }
    groups[[length(groups) + 1L]] <- cur
    ng <- length(groups)
    chunks <- lapply(seq_len(ng), function(k)
      paste0(head_txt, if (nzchar(head_txt)) "\n\n" else "",
             if (ng > 1) sprintf("Part %d of %d of the released data.\n\n", k, ng) else "",
             paste(vapply(groups[[k]], function(i) paste0("### Blinded dataset ", blocks[[i]]$label, " (CSV)\n",
                                                          blocks[[i]]$text), ""), collapse = "\n\n")))
    out$chunk_summary <- if (ng > 1) sprintf("Sent in %d requests (the data are larger than %s characters): %s", ng,
      format(budget, big.mark = ","), paste(vapply(groups, function(g) paste(vapply(blocks[g], function(b) b$label, ""),
                                                                            collapse = " + "), ""), collapse = " | ")) else ""
    user <- paste(unlist(chunks), collapse = "\n\n")
    pdf <- data.frame(class = c(vapply(parts, function(p) p$class, ""), rep("RELEASED", length(blocks))),
                      label = c(vapply(parts, function(p) p$label, ""), paste("Blinded dataset", vapply(blocks, function(b) b$label, ""))),
                      chars = c(vapply(parts, function(p) nchar(p$text), 1L), vapply(blocks, function(b) nchar(b$text), 1L)),
                      stringsAsFactors = FALSE)
  } else {
    parts <- .ub_ai_fit(parts, budget)
    user <- .ub_ai_render(parts)
    if (!nzchar(user)) user <- "(no content)"
    chunks <- list(user)
    pdf <- data.frame(class = vapply(parts, function(p) p$class, ""), label = vapply(parts, function(p) p$label, ""),
                      chars = vapply(parts, function(p) nchar(p$text), 1L), stringsAsFactors = FALSE)
  }
  if (length(g$dropped))
    out$chunk_summary <- trimws(paste(out$chunk_summary, sprintf("Not sent to the Claude API (PRIVATE): %s.",
                                                                 paste(g$dropped, collapse = ", "))))
  out$parts <- pdf
  out$system <- system
  out$user <- user
  out$chunks <- chunks
  out$size_chars <- nchar(system) + sum(vapply(chunks, nchar, 1L))
  if (claude) {
    reason <- .ub_ai_leak_reason(c(list(system = system), stats::setNames(chunks, paste("part", seq_along(chunks)))), values)
    if (!is.null(reason)) { out$allowed <- FALSE; out$blocked_reason <- reason }
  }
  out
}

# ---------------------------------------------------------------------------------------------------------------
# replies
# ---------------------------------------------------------------------------------------------------------------
.ub_ai_json_blocks <- function(text) {
  m <- regmatches(text, gregexpr("(?s)```(?:json|JSON)?[ \t]*\\r?\\n(.*?)```", text, perl = TRUE))[[1]]
  blocks <- sub("(?s)^```(?:json|JSON)?[ \t]*\\r?\\n", "", sub("```$", "", m), perl = TRUE)
  if (!length(blocks)) {
    t <- trimws(text)
    if (grepl("^[\\[{]", t)) blocks <- t
  }
  out <- list()
  for (b in blocks) {
    j <- tryCatch(jsonlite::fromJSON(b, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(j)) next
    if (!is.null(names(j)) && length(j) >= 1) {
      arr <- Filter(function(x) is.list(x) && is.null(names(x)), j)    # {"suggestions": [...]}
      j <- if (length(arr)) arr[[1]] else list(j)
    }
    out <- c(out, Filter(function(x) is.list(x) && !is.null(names(x)), j))
  }
  out
}

.ub_ai_field <- function(x, ...) {
  for (n in c(...)) {
    k <- names(x)[tolower(names(x)) == tolower(n)]
    if (length(k)) {
      v <- x[[k[1]]]
      if (is.null(v)) return("")
      if (is.list(v) || length(v) > 1) return(as.character(jsonlite::toJSON(v, auto_unbox = TRUE)))
      return(as.character(v))
    }
  }
  NA_character_
}

.UB_AI_SETTING_KEYS <- c("dataset_aliases", "identities", "rename_vars", "extra_suppress_vars", "qc_waivers", "qc_waiver_notes",
                         "drop_vars", "blank_where", "config", "config_overrides", "prepare_pc")

# parse and validate the structured part of a reply; NULL when there is none or nothing valid
.ub_ai_parse_suggestions <- function(task, text, folder = NULL) {
  recs <- .ub_ai_json_blocks(text)
  if (!length(recs) || task == "workflow") return(NULL)
  f <- function(n, ...) vapply(recs, function(r) .ub_ai_field(r, n, ...), "")
  df <- switch(task,
    codebook = {
      d <- data.frame(row_key = f("row_key"), column = toupper(f("column")), value = f("value"), reason = f("reason"),
                      stringsAsFactors = FALSE)
      keys <- if (!is.null(folder)) .ub_ai_codebook_raw(folder)$ROW_KEY else NULL
      ok <- !is.na(d$row_key) & !is.na(d$value) & d$column %in% c("BLINDED_VALUE", "NOTE", "CHECK")
      if (!is.null(keys)) ok <- ok & d$row_key %in% keys
      d[ok, , drop = FALSE]
    },
    settings = {
      vals <- vapply(recs, function(r) {
        k <- names(r)[tolower(names(r)) == "value"]
        if (!length(k)) NA_character_ else as.character(jsonlite::toJSON(r[[k[1]]], auto_unbox = TRUE, null = "null"))
      }, "")
      d <- data.frame(path = gsub("^/+|/+$", "", gsub("[.]", "/", f("path"))), value = vals, reason = f("reason"),
                      stringsAsFactors = FALSE)
      top <- sub("/.*$", "", d$path)
      d[!is.na(d$path) & nzchar(d$path) & !is.na(d$value) & top %in% .UB_AI_SETTING_KEYS, , drop = FALSE]
    },
    review = {
      d <- data.frame(DATASET = toupper(f("DATASET")), UBRECID = f("UBRECID"), SUBJID = f("SUBJID"), FINDING = f("FINDING"),
                      stringsAsFactors = FALSE)
      d$UBRECID[is.na(d$UBRECID)] <- ""; d$SUBJID[is.na(d$SUBJID)] <- ""
      d[!is.na(d$DATASET) & nzchar(d$DATASET) & !is.na(d$FINDING) & nzchar(d$FINDING), , drop = FALSE]
    },
    {
      d <- data.frame(code = f("code"), explanation = f("explanation"), suggested_setting = f("suggested_setting"),
                      stringsAsFactors = FALSE)
      d$suggested_setting[is.na(d$suggested_setting)] <- ""
      .ub_ai_setting_cols(d[!is.na(d$code) & !is.na(d$explanation), , drop = FALSE])
    })
  if (is.null(df) || !nrow(df)) return(NULL)
  rownames(df) <- NULL
  if (!is.null(df$reason)) df$reason[is.na(df$reason)] <- ""
  df
}

# "drop_vars.ADNCA = [\"RACE\",\"ETHNIC\"]" or "config_overrides/datasets/ADNCA/text_vars/APROFILE = any"
# -> list(path = "drop_vars/ADNCA", value = <JSON text>); NULL when it is not a known setting
.ub_ai_split_setting <- function(s) {
  s <- trimws(s %||% "")
  if (is.na(s) || !nzchar(s)) return(NULL)
  m <- regmatches(s, regexec("^\\s*([A-Za-z0-9_*?./]+)\\s*[:=]\\s*(.+)$", s))[[1]]
  if (length(m) != 3) return(NULL)
  path <- gsub("^/+|/+$", "", gsub("[.]", "/", m[2]))
  if (!sub("/.*$", "", path) %in% .UB_AI_SETTING_KEYS) return(NULL)
  v <- trimws(m[3])
  val <- tryCatch(jsonlite::fromJSON(v, simplifyVector = FALSE), error = function(e) gsub("^['\"]|['\"]$", "", v))
  list(path = path, value = as.character(jsonlite::toJSON(val, auto_unbox = TRUE, null = "null")))
}

# rows of task "triage"/"unexpected" with the setting split into path + value ("" when there is none)
.ub_ai_setting_cols <- function(df) {
  sp <- lapply(df$suggested_setting, .ub_ai_split_setting)
  df$path <- vapply(sp, function(x) x$path %||% "", "")
  df$value <- vapply(sp, function(x) x$value %||% "", "")
  df
}

# list-valued settings: a suggestion ADDS its values to what is there (two rows suggesting
# drop_vars ADNCA = [RACE, ETHNIC] and [RACE] do not undo each other or an earlier choice)
.UB_AI_LIST_SETTINGS <- c("drop_vars", "extra_suppress_vars", "qc_waivers", "identities/private_terms")

#' Apply suggested settings to a study's blind_options.json
#'
#' @param folder UB study folder
#' @param path slash-separated setting paths (e.g. `drop_vars/ADNCA`)
#' @param value JSON texts of the values
#' @return list(applied = n, problems = character()); nothing is written when there are problems
#' @keywords internal
ub_ai_apply_settings <- function(folder, path, value) {
  keep <- !is.na(path) & nzchar(path) & !is.na(value) & nzchar(value)
  path <- path[keep]; value <- value[keep]
  if (!length(path)) return(list(applied = 0L, problems = "none of the selected rows has a setting to apply"))
  o <- ub_read_options(folder)
  for (i in seq_along(path)) {
    v <- tryCatch(jsonlite::fromJSON(value[i], simplifyVector = FALSE), error = function(e) value[i])
    p <- strsplit(path[i], "/", fixed = TRUE)[[1]]
    is_list <- any(path[i] == .UB_AI_LIST_SETTINGS | startsWith(path[i], paste0(.UB_AI_LIST_SETTINGS, "/")))
    if (is_list) v <- as.list(unique(c(unlist(.ub_get_path(o, p)), unlist(v))))
    o <- .ub_set_path(o, p, v)
  }
  pr <- ub_validate_options(o)
  if (length(pr)) return(list(applied = 0L, problems = pr))
  ub_write_options(folder, o)
  list(applied = length(unique(path)), problems = character())
}

.ub_ai_log <- function(folder, rec) {
  p <- file.path(folder, "private", "logs", "ai_log.jsonl")
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  line <- as.character(jsonlite::toJSON(rec, auto_unbox = TRUE, null = "null", na = "null"))
  cat(line, "\n", file = p, append = TRUE, sep = "")
  line
}

#' Send a prepared AI request
#'
#' Refuses a request the gate blocked. For the Claude API the leak check runs again on the exact text sent.
#' Every request (and every refusal) is logged in `private/logs/ai_log.jsonl` (prompt and reply, never the key).
#' @param prep the result of [ub_ai_prepare()]
#' @param conn the [ub_llm_connect()] connection (same provider as `prep`)
#' @param folder the Universal Blinder folder of the study
#' @return list(text, suggestions (data.frame or NULL), log_line)
#' @export
ub_ai_send <- function(prep, conn, folder) {
  stopifnot(inherits(conn, "ub_llm_conn"))
  folder <- normalizePath(folder, mustWork = FALSE)
  base <- list(time = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), provider = conn$provider,
               model = .ub_or(conn$model, ""), task = prep$task, classes = unique(prep$parts$class),
               size = prep$size_chars)
  refuse <- function(reason) {
    .ub_ai_log(folder, c(base, list(allowed = FALSE, reason = reason)))
    .ub_llm_error(reason)
  }
  if (!isTRUE(prep$allowed)) refuse(paste("Not sent:", prep$blocked_reason))
  if (!identical(prep$provider, conn$provider))
    refuse(paste0("Not sent: this request was prepared for '", prep$provider, "' but the connection is '",
                  conn$provider, "'. Prepare it again."))
  if (conn$provider == "claude") {
    if (any(prep$parts$class == "PRIVATE")) refuse("Not sent: the request holds PRIVATE content.")
    reason <- .ub_ai_leak_reason(c(list(system = prep$system), stats::setNames(prep$chunks, paste("part", seq_along(prep$chunks)))),
                                 ub_ai_private_values(folder))
    if (!is.null(reason)) refuse(reason)
  }
  texts <- character(); sugg <- list(); lines <- character()
  n <- length(prep$chunks)
  for (i in seq_len(n)) {
    r <- tryCatch(ub_llm_chat(conn, prep$system, prep$chunks[[i]], max_tokens = 4000),
                  ub_llm_error = function(e) {
                    lines <<- c(lines, .ub_ai_log(folder, c(base, list(allowed = TRUE, part = i, parts = n,
                                                                         prompt = prep$chunks[[i]], error = conditionMessage(e)))))
                    stop(e)
                  })
    model <- .ub_or(conn$model, .ub_or(r$raw$model, ""))
    lines <- c(lines, .ub_ai_log(folder, c(utils::modifyList(base, list(model = model)),
                                          list(allowed = TRUE, part = i, parts = n, system = prep$system,
                                               prompt = prep$chunks[[i]], reply = r$text,
                                               input_tokens = .ub_or(r$usage$input_tokens, NA),
                                               output_tokens = .ub_or(r$usage$output_tokens, NA)))))
    texts <- c(texts, r$text)
    s <- .ub_ai_parse_suggestions(prep$task, r$text, folder)
    if (!is.null(s)) sugg[[length(sugg) + 1L]] <- s
  }
  text <- if (n > 1) paste(sprintf("--- Part %d of %d ---\n%s", seq_len(n), n, texts), collapse = "\n\n") else texts
  suggestions <- if (length(sugg)) { s <- do.call(rbind, sugg); rownames(s) <- NULL; s } else NULL
  list(text = text, suggestions = suggestions, log_line = lines)
}
