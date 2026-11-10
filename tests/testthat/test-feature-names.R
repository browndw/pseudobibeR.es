test_that("feature_names maps shared identifiers to display names with the same number", {
  map <- pseudobibeR.es::feature_names
  expect_true(all(names(map) %in% names(pseudobibeR.es:::.feature_categories)))
  expect_identical(substr(names(map), 1, 5), substr(unname(map), 1, 5))
  expect_false(anyDuplicated(unname(map)) > 0)
})

test_that("biber() shows display names by default and shared identifiers on request", {
  map <- pseudobibeR.es::feature_names
  language <- suppressMessages(biber(udpipe_samples, measure = "none", normalize = FALSE,
                                     feature_names = "language"))
  shared <- suppressMessages(biber(udpipe_samples, measure = "none", normalize = FALSE,
                                   feature_names = "shared"))
  expect_true(all(unname(map) %in% names(language)))
  expect_false(any(names(map) %in% names(language)))
  expect_true(all(names(map) %in% names(shared)))
  expect_identical(unname(as.list(language)), unname(as.list(shared)))
  expect_identical(names(language), unname(pseudobibeR.es:::display_feature_names(names(shared))))
  old <- options(pseudobibeR.feature_names = "language")
  on.exit(options(old), add = TRUE)
  default <- suppressMessages(biber(udpipe_samples, measure = "none", normalize = FALSE))
  expect_identical(names(default), names(language))
  expect_error(biber(udpipe_samples, feature_names = "english"), "feature_names")
})

test_that("audit_features() accepts either name and reports the chosen one", {
  by_display <- audit_features(udpipe_samples, feature = "f_19_copula_main_verb",
                               feature_names = "language")
  by_shared <- audit_features(udpipe_samples, feature = "f_19_be_main_verb",
                              feature_names = "language")
  expect_identical(by_display, by_shared)
  expect_true(all(by_display$feature == "f_19_copula_main_verb"))
  shared <- audit_features(udpipe_samples, feature = "f_19_copula_main_verb",
                           feature_names = "shared")
  expect_true(all(shared$feature == "f_19_be_main_verb"))
})
