# test-revision-hernan-phase1.R
#
# Phase 1 of Hernan's review (INSTRUCCIONES_CLAUDE_CODE_pseudobiber_es.md):
# coverage fixes for f_01, f_06-f_08 and f_42. The expected counts
# are verified empirically against spanish-gsd; the UD tags that
# support them are asserted in test-udpipe-tag-verification.R.
#
# The sentences avoid putting the target verb/form in initial position
# (spanish-gsd mis-tags capitalized words at the start of a sentence).
#
# nolint start: line_length_linter, object_name_linter

feat_count <- function(text, feature) {
  as.numeric(run_biber(text)[[feature]])
}

test_that("f_01 cuenta pasados de indicativo y subjuntivo (perfecto simple + imperfecto)", {
  skip_if_not_installed("udpipe")
  expect_equal(feat_count("Ayer corrí hasta la esquina.", "f_01_past_tense"), 1)          # simple perfect
  expect_equal(feat_count("Cuando era joven caminaba mucho.", "f_01_past_tense"), 2)       # imperfect ind. (era, caminaba)
  expect_equal(feat_count("Yo quería que corriera más rápido.", "f_01_past_tense"), 2)     # + imperfect subj. (corriera)
  expect_equal(feat_count("Ya había corrido dos kilómetros.", "f_01_past_tense"), 1)       # aux 'había' of the compound tense
  expect_equal(feat_count("Ella corre todos los días.", "f_01_past_tense"), 0)             # present: does not count
})

test_that("f_06/f_07/f_08 incluyen posesivos ruteados por persona", {
  skip_if_not_installed("udpipe")
  r1 <- run_biber("Nuestro equipo revisó mi propuesta.")
  expect_equal(as.numeric(r1$f_06_first_person_pronouns), 2)   # nuestro + mi (Poss=Yes, Person=1)
  expect_equal(as.numeric(r1$f_07_second_person_pronouns), 0)
  expect_equal(as.numeric(r1$f_08_third_person_pronouns), 0)

  r2 <- run_biber("Usted debe entregar su informe.")
  expect_equal(as.numeric(r2$f_07_second_person_pronouns), 1)  # usted
  expect_equal(as.numeric(r2$f_08_third_person_pronouns), 1)   # su -> Person=3 (morphological criterion)

  # Nominalized stressed possessives (NOUN, without Poss=Yes) captured by lemma.
  r3 <- run_biber("Los resultados míos superan los tuyos.")
  expect_equal(as.numeric(r3$f_06_first_person_pronouns), 1)   # míos
  expect_equal(as.numeric(r3$f_07_second_person_pronouns), 1)  # tuyos

  # Voseo: 'vos' is mis-tagged as NOUN; it is recovered by token.
  expect_equal(feat_count("Vos tenés razón siempre.", "f_07_second_person_pronouns"), 1)
})

test_that("LIMITACIÓN documentada: 'se' argumental no es separable de la pasiva refleja", {
  skip_if_not_installed("udpipe")
  # 'Ella se vio' -> only 'ella' counts (=1). The argumental reflexive 'se' is NOT
  # counted because it is morphologically identical to the reflexive passive / impersonal
  # (see test-udpipe-tag-verification.R). The spec asked for 2; the deviation is
  # documented so as not to inflate f_08 in academic corpora full of impersonal 'se'.
  expect_equal(feat_count("Ella se vio en el espejo.", "f_08_third_person_pronouns"), 1)
  # Reflexive passive: 'se' correctly NOT counted.
  r <- run_biber("Se publicaron los informes.")
  expect_equal(as.numeric(r$f_08_third_person_pronouns), 0)
  expect_equal(as.numeric(r$f_06_first_person_pronouns), 0)
})

test_that("f_42 cuenta TODOS los adverbios, con solapamientos deliberados", {
  skip_if_not_installed("udpipe")
  # 'no' (also f_67), 'rápidamente', 'hoy' (also f_05): all three in f_42.
  expect_equal(feat_count("No corre rápidamente hoy.", "f_42_adverbs"), 3)
})

# nolint end
