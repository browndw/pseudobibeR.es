test_that("auditing exposes the common output schema and original surface text", {
  tokens <- contract_tokens()
  out <- audit_fixture(tokens, feature = "f_06_first_person_pronouns", window = 1)
  expect_s3_class(out, "audit_features_result")
  expect_named(out, c("feature", "category", "doc_id", "sentence_id", "count",
                      "token_count", "match_type", "left", "keyword", "right", "chunk"))
  expect_equal(out$keyword, "Yo")
  expect_equal(out$right, "pienso")
  expect_equal(out$count, 1L)
  expect_equal(out$category, "C")
  expect_equal(nrow(audit_fixture(tokens, category = "A")), 1L)
  expect_equal(nrow(audit_fixture(tokens, feature = "f_09_pronoun_it")), 0L)
})

test_that("auditing validates selectors and sampling without leaking random state", {
  tokens <- contract_tokens()
  expect_error(audit_fixture(tokens), "exactly one")
  expect_error(audit_fixture(tokens, feature = "unknown"), "known canonical")
  expect_error(audit_fixture(tokens, category = "Z"), "Unknown category")
  expect_error(audit_fixture(tokens, feature = "f_43_type_token"), "metrics")
  expect_error(audit_fixture(tokens, category = "A", sample_n = NA), "sample_n")
  expect_error(audit_fixture(tokens, category = "A", window = 1.5), "window")
  set.seed(42)
  before <- .Random.seed
  out <- audit_fixture(tokens, category = "C", sample_n = 1, seed = 7)
  expect_identical(.Random.seed, before)
  expect_equal(nrow(out), 1L)
})
