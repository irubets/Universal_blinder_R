test_that("title case and case styles match the Python rules", {
  expect_equal(universalblinder:::ub_title_case(c("x-12 metabolite", "abc1def", "tab01")), c("X-12 Metabolite", "Abc1Def", "Tab01"))
  expect_equal(universalblinder:::ub_case_style("FICTINIB"), "UPPER")
  expect_equal(universalblinder:::ub_case_style("fictinib"), "LOWER")
  expect_equal(universalblinder:::ub_case_style("Fictinib"), "TITLE")
  expect_equal(universalblinder:::ub_case_style("FicTinib"), "MIXED")
})

test_that("substitution is case-preserving, word-bounded and reversible", {
  fwd <- universalblinder:::ub_substituter(data.frame(find = c("FICTINIB", "FCT"), replace = c("TREAT_01", "Tab01"), priority = 1))
  rev <- universalblinder:::ub_substituter(data.frame(find = c("TREAT_01", "Tab01"), replace = c("FICTINIB", "FCT"), priority = 1))
  x <- c("SC fictinib Period 1", "FICTINIB", "Plasma Fictinib", "40 mg/m2 SC FCT", "SC Fct", "C1D1FCT")
  expect_equal(rev(fwd(x)), x)
  expect_equal(fwd("SC fictinib"), "SC treat_01")
  expect_equal(fwd("C1D1FCT"), "C1D1FCT")
})

test_that("numeric helpers", {
  r <- universalblinder:::ub_round_half_away
  expect_equal(r(c(2.5, -2.5, 2.675), c(0, 0, 2)), c(3, -3, 2.68))
  expect_equal(universalblinder:::ub_infer_decimals(c(84, 161.6, 1.46, 32.00000000000001)), c(0L, 1L, 2L, 0L))
  expect_equal(universalblinder:::ub_text_decimals(c("BQL<(1.00)", "544", "61.9")), c(2L, 0L, 1L))
  expect_equal(universalblinder:::ub_format_fixed(c(626, 80), c(0, 1)), c("626", "80.0"))
})

test_that("BQL<(x) and BLQ<(x) are both recognised; the spelling is kept", {
  for (p in list(c("BQL<(1.00)", "BQL"), c("BLQ<(0.500)", "BLQ"), c(" blq < ( 0.5 ) ", "blq"))) {
    expect_true(universalblinder:::.ub_is_bql(p[1]))
    expect_equal(universalblinder:::ub_bql_prefix(p[1]), p[2])
  }
  expect_equal(universalblinder:::ub_bql_value("BLQ<(0.500)"), 0.5)
  expect_equal(universalblinder:::ub_text_decimals("BLQ<(0.500)"), 3L)
  expect_equal(universalblinder:::ub_make_bql_text(0.4567, 3, "BLQ"), "BLQ<(0.457)")
  expect_equal(universalblinder:::ub_make_bql_text(1.0, 2), "BQL<(1.00)")
  expect_false(universalblinder:::.ub_is_bql("<0.5"))
  expect_false(universalblinder:::.ub_is_bql("BLQ"))
  expect_equal(universalblinder:::ub_bql_prefix("12.3"), "BQL")
  expect_identical(universalblinder:::.ub_is_bql(c("BQL<(1)", NA, "12", "BLQ<(2.5)")), c(TRUE, FALSE, FALSE, TRUE))
  expect_identical(universalblinder:::.ub_is_bql(c(1, NA)), c(FALSE, FALSE))
})

test_that("an analyte name identical to its code maps through the code (exact_map)", {
  t <- data.frame(TYPE = c("ANALYTE", "ANALYTE", "ANALYTE"), ROLE = c("CODE", "NAME", "CODE"),
                  IDENTITY_KEY = c("ALPHA", "ALPHA", "ABC123"), SEQ = c("1", "1", "2"),
                  ORIG_TEXT = c("ALPHA", "Alphadrug", "ABC123"), BLIND_TEXT = c("ANL01", "ANL01", "ANL02"),
                  stringsAsFactors = FALSE)
  keys <- universalblinder:::.new_keys(list(terms = t), list())
  b <- universalblinder:::ub_exact_map(keys, "analyte_name", "blind")
  expect_equal(unname(b[c("Alphadrug", "ABC123")]), c("ANL01", "ANL02"))
  expect_equal(sum(names(b) == "Alphadrug"), 1L)
  u <- universalblinder:::ub_exact_map(keys, "analyte_name", "unblind")
  expect_equal(unname(u[c("ANL01", "ANL02")]), c("Alphadrug", "ABC123"))   # the NAME row wins
})
