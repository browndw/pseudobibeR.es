# Tests of the internal .feature_categories lookup.
#
# Ensures that the feature->category mapping defined in R/feature_categories.R
# stays complete and consistent with the real output of biber().
# Any new feature added to the output that forgets its entry in
# .feature_categories breaks this test.

library(testthat)

# The object lives in the package's internal namespace.
cats          <- pseudobibeR.es:::.feature_categories
zero_out      <- pseudobibeR.es:::.zero_output_features
metric_feats  <- pseudobibeR.es:::.metric_features
detectable    <- pseudobibeR.es:::.detectable_features
evidence_pool <- pseudobibeR.es:::.evidence_features

test_that(".feature_categories cubre los 67 rasgos esperados", {
  expect_length(cats, 67L)
  expect_true(all(grepl("^f_\\d{2}_", names(cats))))
  expect_false(any(duplicated(names(cats))))
})

test_that(".feature_categories tiene exactamente 16 categorias (A-P)", {
  uniq <- sort(unique(unname(cats)))
  expect_length(uniq, 16L)
  expect_true(all(grepl("^[A-P]\\. ", uniq)))
})

test_that(".feature_categories cubre cada columna f_NN_ del output de biber()", {
  skip_if_not_installed("udpipe")
  ud_model <- .fc_ud()
  parsed   <- udpipe::udpipe_annotate(ud_model, x = "El gato come pescado.", doc_id = "d1")
  out      <- pseudobibeR.es::biber(parsed, measure = "TTR", normalize = FALSE)

  feat_cols <- grep("^f_\\d{2}_", colnames(out), value = TRUE)
  missing   <- setdiff(feat_cols, names(cats))
  extra     <- setdiff(names(cats), feat_cols)

  expect_equal(missing, character(0),
               info = "rasgos en biber() sin categoria asignada")
  expect_equal(extra,   character(0),
               info = "categorias asignadas a rasgos que no existen en biber()")
})

test_that(".zero_output_features tiene el 1 esperado y esta en .feature_categories", {
  expect_length(zero_out, 1L)
  expect_true(all(zero_out %in% names(cats)))
  expect_false("f_09_pronoun_it" %in% zero_out)
  expect_false("f_12_proverb_do" %in% zero_out)
  expect_false("f_15_gerunds" %in% zero_out)
  expect_false("f_31_wh_subj" %in% zero_out)
  expect_false("f_32_wh_obj" %in% zero_out)
  expect_false("f_59_contractions" %in% zero_out)
  expect_false("f_60_that_deletion" %in% zero_out)
  expect_false("f_62_split_infinitive" %in% zero_out)
  expect_false("f_61_stranded_preposition" %in% zero_out)
})

test_that(".metric_features cubre f_43 y f_44", {
  expect_setequal(metric_feats, c("f_43_type_token", "f_44_mean_word_length"))
  expect_true(all(metric_feats %in% names(cats)))
})

test_that(".detectable_features = 67 - 1 zero-output = 66", {
  expect_length(detectable, 66L)
  expect_true(all(detectable %in% names(cats)))
  expect_equal(length(intersect(detectable, zero_out)), 0L)
})

test_that(".evidence_features = detectable - metricas = 64", {
  expect_length(evidence_pool, 64L)
  expect_true("f_09_pronoun_it" %in% evidence_pool)
  expect_true("f_12_proverb_do" %in% evidence_pool)
  expect_true("f_15_gerunds" %in% evidence_pool)
  expect_true("f_31_wh_subj" %in% evidence_pool)
  expect_true("f_32_wh_obj" %in% evidence_pool)
  expect_true("f_59_contractions" %in% evidence_pool)
  expect_true("f_60_that_deletion" %in% evidence_pool)
  expect_true("f_62_split_infinitive" %in% evidence_pool)
  expect_true("f_61_stranded_preposition" %in% evidence_pool)
  expect_equal(length(intersect(evidence_pool, metric_feats)), 0L)
  expect_equal(length(intersect(evidence_pool, zero_out)),     0L)
})

# Topology of the counting path (M1: relaxed invariant for dual-path)

dual_path  <- pseudobibeR.es:::.dual_path_features
dict_only  <- pseudobibeR.es:::.dict_only_features
strict_ev  <- pseudobibeR.es:::.strict_evidence_features

test_that(".dual_path_features, .dict_only_features y .strict_evidence_features son disjuntos", {
  expect_equal(length(intersect(dual_path, dict_only)), 0L)
  expect_equal(length(intersect(dual_path, strict_ev)), 0L)
  expect_equal(length(intersect(dict_only, strict_ev)), 0L)
})

test_that("la union de los tres = .evidence_features (sin huecos ni solapes)", {
  union_all <- sort(unique(c(dual_path, dict_only, strict_ev)))
  expect_setequal(union_all, evidence_pool)
})

test_that(".udpipe_limited_features es subconjunto de .evidence_features", {
  udpipe_limit <- pseudobibeR.es:::.udpipe_limited_features
  expect_true(all(udpipe_limit %in% evidence_pool))
})

test_that(".dual_path_features contains the two adverbial features", {
  expect_length(dual_path, 2L)
  expect_true(all(dual_path %in% names(cats)))
})

test_that(".dict_only_features tiene los 6 esperados (categoria K)", {
  expect_length(dict_only, 6L)
  expect_true(all(dict_only %in% names(cats)))
  expect_true(all(grepl("^K\\.", cats[dict_only])))
})

test_that(".strict_evidence_features = 64 - 2 - 6 = 56", {
  expect_length(strict_ev, 56L)
})
