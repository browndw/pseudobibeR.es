# Regression: output contract of biber() for Spanish.
#
# v0.024+: surface parity with pseudobibeR.fr — biber() exposes the
# 67 feature names of Biber (1988). The 10 that cannot be detected in Spanish
# (f_09, f_12, f_15, f_28, f_31, f_32, f_59, f_60, f_61, f_62) are
# constant-zero columns. The 55 detectable ones + 2 merges (f_29 absorbs
# f_31, f_30 absorbs f_32) fulfill the 1:1 contract with Biber.
# Historical bug blocked: f_69/f_70/f_71 (internal extensions) must not
# appear; f_44 must not be doubled when normalizing.

spanish_udpipe_model_or_skip <- function() {
  .fc_ud()
}

test_that("biber expone los 67 nombres de rasgo (paridad superficial FR)", {
  skip_if_not_installed("udpipe")
  ud <- spanish_udpipe_model_or_skip()

  parsed <- udpipe::udpipe_annotate(
    ud,
    x = paste(
      "La producción de conocimiento requiere una evaluación rigurosa.",
      "Esos métodos fueron analizados por el comité de expertos."
    )
  ) |> as.data.frame()

  res <- biber(parsed, measure = "MATTR", normalize = TRUE)
  feat_cols <- grep("^f_", names(res), value = TRUE)

  # Complete surface inventory: 67 features
  expect_equal(length(feat_cols), 67L)
  # No internal extension f_68+ (excluded from Biber's catalog)
  expect_length(grep("^f_(68|69|70|71)(_|$)", feat_cols), 0L)
  # f_43 (ratio) and f_44 (mean length) present
  expect_true(all(c("f_43_type_token", "f_44_mean_word_length") %in% feat_cols))
  # The 10 features that cannot be detected in Spanish must be 0
  surface_zero <- c("f_09_pronoun_it", "f_12_proverb_do", "f_15_gerunds",
                    "f_28_present_participle_whiz", "f_31_wh_subj",
                    "f_32_wh_obj", "f_59_contractions", "f_60_that_deletion",
                    "f_61_stranded_preposition", "f_62_split_infinitive")
  for (sf in surface_zero) {
    expect_true(sf %in% feat_cols, info = paste(sf, "ausente"))
    expect_equal(as.numeric(res[[sf]][1]), 0, info = paste(sf, "no es 0"))
  }
})

test_that("f_43/f_44 no se doblan al normalizar; conteos en rango sano", {
  skip_if_not_installed("udpipe")
  ud <- spanish_udpipe_model_or_skip()

  parsed <- udpipe::udpipe_annotate(
    ud,
    x = "La producción de conocimiento requiere una evaluación rigurosa y detallada."
  ) |> as.data.frame()

  rF <- biber(parsed, normalize = FALSE)
  rT <- biber(parsed, normalize = TRUE)

  # Derived metrics identical with and without normalization
  expect_equal(rF$f_43_type_token,       rT$f_43_type_token,       tolerance = 1e-6)
  expect_equal(rF$f_44_mean_word_length, rT$f_44_mean_word_length, tolerance = 1e-6)

  # Mean word length in a plausible range (3-15 characters)
  expect_gt(rT$f_44_mean_word_length, 3)
  expect_lt(rT$f_44_mean_word_length, 15)

  # No rate per 1000 exceeds a reasonable ceiling (no double normalization)
  feat_cols <- setdiff(grep("^f_", names(rT), value = TRUE),
                       c("f_43_type_token", "f_44_mean_word_length"))
  expect_lt(max(as.numeric(rT[1, feat_cols]), na.rm = TRUE), 1000)
})
