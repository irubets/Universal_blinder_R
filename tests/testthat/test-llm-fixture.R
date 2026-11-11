# Shared fixture of the AI-layer tests (sourced by the other test-llm-*.R files; defines no tests itself).
# A synthetic Universal Blinder folder with private values of every kind planted in its sources.

LLM_KEY <- "sk-ant-TEST-KEY-must-never-be-logged-0123456789"

llm_study <- function(release = FALSE, qc = "PASS") {
  d <- tempfile("ubllm")
  dir.create(file.path(d, "input"), recursive = TRUE)
  dir.create(file.path(d, "private", "maps"), recursive = TRUE)
  src <- system.file("examples", "synthetic", "source", package = "universalblinder")
  file.copy(file.path(src, c("adsl.xpt", "pc.xpt", "ex.xpt")), file.path(d, "input"))
  opts <- list(dataset_aliases = setNames(list(), character()),
               identities = list(treatment_aliases = list(FICTINIB = list("FCT-INJ")),
                                 analyte_aliases = setNames(list(), character()),
                                 analyte_code_aliases = setNames(list(), character()),
                                 private_terms = list("Zorblax Study Centre")))
  writeLines(jsonlite::toJSON(opts, auto_unbox = TRUE, pretty = TRUE), file.path(d, "blind_options.json"))
  cb <- data.frame(TYPE = c("TREATMENT", "TREATMENT", "ANALYTE", "ANALYTE", "ANALYTE", "ARM", "TEXT"),
                   ACTUAL_VALUE = c("FICTINIB", "SYNTHAZOLE", "FICT", "Fictinib", "SZ-M1", "Crossover Group A", "Dr Quillfeather"),
                   BLINDED_VALUE = c("TRT01", "TRT02", "ANL01", "ANL01", "?", "ARM01", "TERM01"),
                   NOTE = c("treatment", "treatment", "analyte code", "analyte name", "CHECK: other spelling of SYNZM1?",
                            "ARM value", "private text"), stringsAsFactors = FALSE)
  utils::write.csv(cb, file.path(d, "private", "codebook.csv"), row.names = FALSE)
  m <- file.path(d, "private", "maps")
  utils::write.csv(data.frame(STUDYID_ORIG = "SYN-PK-001", USUBJID_ORIG = "SYN-PK-001-955-501", SUBJID_ORIG = "955-501",
                              STUDYID_BLIND = "STUDY-XXX", USUBJID_BLIND = "STUDY-XXX-S0001", SUBJID_BLIND = "S0001"),
                   file.path(m, "replace_subjid.csv"), row.names = FALSE)
  utils::write.csv(data.frame(SITEID_ORIG = "SITE-777", SITEID_BLIND = "SITE001"), file.path(m, "replace_siteid.csv"), row.names = FALSE)
  utils::write.csv(data.frame(TYPE = "PRIVATE", ROLE = "TERM", IDENTITY_KEY = "T1", SEQ = "1", ORIG_TEXT = "Moonbase Clinic",
                              BLIND_TEXT = "TERM02"), file.path(m, "replace_terms.csv"), row.names = FALSE)
  if (release) {
    dir.create(file.path(d, "output"))
    writeLines(c("# Blinding QC summary", "", paste0("**Overall: ", qc, "** -- INFO: 3")), file.path(d, "output", "qc_summary.md"))
    rel <- data.frame(STUDYID = "STUDY-XXX", USUBJID = sprintf("STUDY-XXX-S%04d", rep(1:4, each = 3)),
                      SUBJID = sprintf("S%04d", rep(1:4, each = 3)), UBRECID = sprintf("R%05d", 1:12),
                      PARAMCD = "ANL01", AVAL = round(seq(1.5, 40, length.out = 12), 2), stringsAsFactors = FALSE)
    haven::write_xpt(rel, file.path(d, "output", "adnca.xpt"))
  }
  d
}

# a planted value of each kind, as it appears in the sources above
LLM_PLANTED <- c(subject = "SYN-PK-001-955-501", subject_short = "955-501", site = "SITE-777", study = "SYN-PK-001",
                 treatment = "FICTINIB", treatment_alias = "FCT-INJ", analyte = "SZ-M1", arm = "Crossover Group A",
                 term_cb = "Dr Quillfeather", term_key = "Moonbase Clinic", term_opt = "Zorblax Study Centre")

claude_conn <- function(...) ub_llm_connect("claude", api_key = LLM_KEY, ...)
ollama_conn <- function(...) ub_llm_connect("ollama", model = "qwen3.5:4b", ...)

claude_reply <- function(text) {
  httr2::response_json(body = list(id = "msg_1", type = "message", role = "assistant", model = "claude-opus-5-5",
                                   content = list(list(type = "text", text = text)),
                                   usage = list(input_tokens = 10L, output_tokens = 5L)))
}
ollama_reply <- function(text) {
  httr2::response_json(body = list(model = "qwen3.5:4b", message = list(role = "assistant", content = text), done = TRUE,
                                   prompt_eval_count = 10L, eval_count = 5L))
}
no_http <- function(req) stop("an HTTP request was made although none was expected: ", req$url)

if (!exists("llm_sourced", inherits = FALSE)) {
  test_that("the AI test fixture plants every kind of private value", {
    f <- llm_study(release = TRUE)
    v <- ub_ai_private_values(f)
    expect_true(all(toupper(LLM_PLANTED) %in% toupper(v$value)))
    expect_true(file.exists(file.path(f, "output", "adnca.xpt")))
  })
}
