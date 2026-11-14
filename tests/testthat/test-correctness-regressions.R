# Raw counts of the regression fixture, computed once for the tests below
# (counts are per document).
regression_raw <- local({
  raw <- NULL
  function() {
    if (is.null(raw)) {
      tokens <- readRDS(test_path("fixtures", "spanish-gsd-regressions.rds"))
      raw <<- biber_fixture(tokens, measure = "none", normalize = FALSE)
    }
    raw
  }
})

test_that("passive agents follow dependencies rather than token windows", {
  raw <- regression_raw()
  expect_equal(raw$f_18_by_passives[raw$doc_id == "distant_agent"], 1)
  expect_equal(raw$f_17_agentless_passives[raw$doc_id == "distant_agent"], 0)
  expect_equal(raw$f_18_by_passives[raw$doc_id == "next_sentence"], 0)
  expect_equal(raw$f_17_agentless_passives[raw$doc_id == "next_sentence"], 1)
})

test_that("multiword dictionaries and prepositions have exact positional audits", {
  tokens <- readRDS(test_path("fixtures", "spanish-gsd-regressions.rds"))
  raw <- regression_raw()
  # One audit of both documents; f_39 only uses counts, so window = 1 is safe.
  audits <- audit_by_feature(tokens[tokens$doc_id %in% c("preposition", "conjunct"), ],
                             c("f_39_prepositions", "f_45_conjuncts"), window = 1)
  audit <- audits$f_39_prepositions[audits$f_39_prepositions$doc_id == "preposition", ]
  expect_equal(raw$f_39_prepositions[raw$doc_id == "preposition"], 1)
  expect_equal(sum(audit$count), raw$f_39_prepositions[raw$doc_id == "preposition"])
  audit <- audits$f_45_conjuncts[audits$f_45_conjuncts$doc_id == "conjunct", ]
  expect_equal(audit$keyword, "Sin embargo")
  expect_equal(audit$left, "")
  expect_equal(audit$right, ",")
  expect_equal(sum(audit$count), raw$f_45_conjuncts[raw$doc_id == "conjunct"])
})

test_that("UD container rows and empty nodes do not change normalization", {
  tokens <- contract_tokens()
  containers <- tokens[c(1, 2), ]
  containers$token_id <- c("1-2", "2.1")
  expect_equal(biber_fixture(rbind(tokens, containers), measure = "none"),
               biber_fixture(tokens, measure = "none"))
  tokens$upos <- "PUNCT"
  expect_warning(punctuation <- biber_fixture(tokens, measure = "none"), "no word tokens")
  expect_true(is.na(punctuation$f_03_present_tense))
})

test_that("none omits diversity and short CTTR is not replaced by TTR", {
  tokens <- contract_tokens()
  none <- biber_fixture(tokens, measure = "none")
  expect_false("f_43_type_token" %in% names(none))
  expect_length(grep("^f_", names(none)), 66)
  expect_equal(biber_fixture(tokens, measure = "CTTR")$f_43_type_token, 3 / sqrt(6))
})

test_that("the public namespace exposes common pipelines, not applications", {
  exports <- getNamespaceExports("pseudobibeR.es")
  expect_true(all(c("biber", "audit_features") %in% exports))
  expect_false(any(c("biber_es", "biber_es_batch", "biber_es_traced",
                     "write_biber_xlsx") %in% exports))
})

test_that("legacy batch helpers are absent from the package namespace", {
  namespace <- asNamespace("pseudobibeR.es")
  for (helper in c("biber_es_batch", "read_corpus_input",
                   "run_batch_fast", "run_batch_safe")) {
    expect_false(exists(helper, envir = namespace, inherits = FALSE))
  }
})
