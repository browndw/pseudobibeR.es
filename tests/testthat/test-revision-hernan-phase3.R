# test-revision-hernan-phase3.R
#
# The old csubj-only proposal was reverted. D-23 now counts clear noun-like
# nominal infinitives, without transferring ordinary infinitival clauses.
#
# nolint start: line_length_linter, object_name_linter

feat_p3 <- function(text, feature) as.numeric(run_biber(text)[[feature]])

test_that("f_15 distinguishes noun-like infinitives from ordinary infinitival clauses", {
  skip_if_not_installed("udpipe")
  expect_equal(feat_p3("Fumar perjudica la salud.", "f_15_gerunds"), 0)
  expect_equal(feat_p3("Me gusta nadar en el mar.", "f_15_gerunds"), 0)
  expect_equal(feat_p3("El fumar constante daña la salud.", "f_15_gerunds"), 1)
})

test_that("f_24 retains verbal infinitives, including clausal subjects", {
  skip_if_not_installed("udpipe")
  # 'nadar' (formerly csubj -> f_15) now counts in f_24, as in the original design.
  r <- run_biber("Me gusta nadar en el mar.")
  expect_equal(as.numeric(r$f_15_gerunds), 0)
  expect_equal(as.numeric(r$f_24_infinitives), 1)
  # The cases that already counted in f_24 (xcomp) do not change:
  expect_equal(feat_p3("Quiere estudiar medicina.", "f_24_infinitives"), 1)
  expect_equal(feat_p3("Prohibieron fumar en el bar.", "f_24_infinitives"), 1)
})

# nolint end
