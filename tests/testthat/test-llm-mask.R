llm_sourced <- TRUE
source(test_path("test-llm-fixture.R"), local = TRUE)

test_that("the private value set covers codebook, key, settings and input identity columns", {
  f <- llm_study()
  v <- ub_ai_private_values(f)
  expect_named(v, c("kind", "value", "placeholder"))
  kind_of <- function(x) v$kind[toupper(v$value) == toupper(x)]
  expect_equal(kind_of("SYN-PK-001-955-501"), "subject")     # key
  expect_equal(kind_of("955-501"), "subject")
  expect_equal(kind_of("SITE-777"), "site")
  expect_equal(kind_of("SYN-PK-001"), "study")
  expect_equal(kind_of("FICTINIB"), "treatment")               # codebook
  expect_equal(kind_of("FCT-INJ"), "treatment")                # settings alias
  expect_equal(kind_of("SZ-M1"), "analyte")
  expect_equal(kind_of("Crossover Group A"), "arm")
  expect_equal(kind_of("Dr Quillfeather"), "term")
  expect_equal(kind_of("Moonbase Clinic"), "term")
  expect_equal(kind_of("Zorblax Study Centre"), "term")
  # input identity columns (no key needed): subjects of the synthetic ADSL, sites, PC analyte codes
  adsl <- haven::read_xpt(file.path(f, "input", "adsl.xpt"))
  expect_true(all(toupper(adsl$USUBJID) %in% toupper(v$value[v$kind == "subject"])))
  expect_true(all(unique(adsl$SITEID) %in% v$value[v$kind == "site"]))
  pc <- haven::read_xpt(file.path(f, "input", "pc.xpt"))
  expect_true(all(toupper(unique(pc$PCTESTCD)) %in% toupper(v$value)))
  # typed, numbered placeholders, unique values
  expect_true(all(grepl("^<(SUBJECT|SITE|STUDY|TREATMENT|ANALYTE|TERM|ARM)_[0-9]+>$", v$placeholder)))
  expect_false(any(duplicated(toupper(v$value))))
  expect_false(any(duplicated(v$placeholder)))
  expect_true(all(nchar(v$value) >= 3))
})

test_that("the value set is cached and refreshed when a source changes", {
  f <- llm_study()
  v1 <- ub_ai_private_values(f)
  expect_identical(ub_ai_private_values(f), v1)
  o <- jsonlite::fromJSON(file.path(f, "blind_options.json"), simplifyVector = FALSE)
  o$identities$private_terms <- list("Zorblax Study Centre", "Xanthic Labs")
  Sys.sleep(1.1)
  writeLines(jsonlite::toJSON(o, auto_unbox = TRUE), file.path(f, "blind_options.json"))
  expect_true("Xanthic Labs" %in% ub_ai_private_values(f)$value)
})

test_that("masking replaces every kind of value, longest first and ignoring case", {
  f <- llm_study()
  v <- ub_ai_private_values(f)
  txt <- paste("Subject syn-pk-001-955-501 (955-501) of study SYN-PK-001 at site SITE-777 got fictinib (alias FCT-INJ);",
               "analyte sz-m1 in arm Crossover Group A, seen by dr quillfeather at Moonbase Clinic, Zorblax Study Centre.",
               "Dosed 2024-03-01T08:30 and 05JAN2024.")
  m <- ub_ai_mask(txt, v)
  for (x in LLM_PLANTED) expect_false(grepl(x, m, ignore.case = TRUE, fixed = FALSE), info = x)
  ph <- function(x) v$placeholder[toupper(v$value) == toupper(x)]
  expect_match(m, ph("SYN-PK-001-955-501"), fixed = TRUE)      # the whole USUBJID, not its study prefix
  expect_match(m, ph("955-501"), fixed = TRUE)
  expect_match(m, ph("SITE-777"), fixed = TRUE)
  expect_match(m, ph("Crossover Group A"), fixed = TRUE)
  expect_false(grepl("2024", m))
  expect_equal(lengths(regmatches(m, gregexpr("<DATE>", m, fixed = TRUE))), 2L)
  expect_equal(nrow(ub_ai_leak_check(m, v)), 0L)
})

test_that("masking respects word and number boundaries and skips very short values", {
  v <- data.frame(kind = c("site", "subject", "analyte", "term", "subject"),
                  value = c("901", "7-1", "FICT", "AB", "12"),
                  placeholder = c("<SITE_1>", "<SUBJECT_1>", "<ANALYTE_1>", "<TERM_1>", "<SUBJECT_2>"), stringsAsFactors = FALSE)
  expect_equal(ub_ai_mask("site 901, value 1901 and 901.5 and 3.901", v),
               "site <SITE_1>, value 1901 and 901.5 and 3.901")
  expect_equal(ub_ai_mask("FICTINIB is not FICT; FICT_conc is", v), "FICTINIB is not <ANALYTE_1>; <ANALYTE_1>_conc is")
  expect_equal(ub_ai_mask("subject 7-1 and 17-12 AB 12", v), "subject <SUBJECT_1> and 17-12 AB 12")
  expect_equal(ub_ai_mask("a [x] (y) value.", data.frame(kind = "term", value = "[x] (y)", placeholder = "<TERM_9>")),
               "a <TERM_9> value.")
})

test_that("the leak check reports kind and position, never the value", {
  v <- data.frame(kind = c("treatment", "subject"), value = c("FICTINIB", "955-501"),
                  placeholder = c("<TREATMENT_1>", "<SUBJECT_1>"), stringsAsFactors = FALSE)
  h <- ub_ai_leak_check("line one\nthe Fictinib dose for 955-501", v)
  expect_equal(nrow(h), 2L)
  expect_equal(h$kind, c("treatment", "subject"))
  expect_equal(h$line, c(2L, 2L))
  expect_equal(h$position[1], 14L)
  expect_false(any(vapply(h, function(col) any(grepl("FICTINIB|955-501", col, ignore.case = TRUE)), logical(1))))
  expect_equal(nrow(ub_ai_leak_check("nothing here", v)), 0L)
})
