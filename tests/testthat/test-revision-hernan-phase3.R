# test-revision-hernan-phase3.R
#
# Phase 3 (REVERTED at the author's request): Hernan's review had
# activated f_15_gerunds to count the infinitive in nominal-subject function
# (csubj). The author asked to return f_15 to its original behavior:
# an always-zero column, with no detection. This file documents the revert.
#
# nolint start: line_length_linter, object_name_linter

feat_p3 <- function(text, feature) as.numeric(run_biber(text)[[feature]])

test_that("f_15 se mantiene siempre en 0 (comportamiento original, revertido)", {
  skip_if_not_installed("udpipe")
  # Cases that triggered f_15 = 1 in Phase 3 (before the revert):
  expect_equal(feat_p3("Fumar perjudica la salud.", "f_15_gerunds"), 0)
  expect_equal(feat_p3("Me gusta nadar en el mar.", "f_15_gerunds"), 0)
  expect_equal(feat_p3("El fumar constante daña la salud.", "f_15_gerunds"), 0)
})

test_that("f_24 vuelve a contar TODOS los infinitivos, incluidos los que antes iban a f_15", {
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
