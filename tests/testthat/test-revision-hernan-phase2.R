# test-revision-hernan-phase2.R
#
# Historical Phase 2 cases now follow the approved family/role partition.
# Roleless pro-drop gaps are not automatically classified as subjects.
#
# nolint start: line_length_linter, object_name_linter

feat_p2 <- function(text, feature) as.numeric(run_biber(text)[[feature]])

test_that("f_29: relativa de 'que' en función de sujeto", {
  skip_if_not_installed("udpipe")
  expect_equal(feat_p2("El libro que está en la mesa es mío.", "f_29_that_subj"), 1)
})

test_that("f_30: relativa de 'que' objeto (requiere sujeto explícito en la relativa)", {
  skip_if_not_installed("udpipe")
  expect_equal(feat_p2("El libro que María escribió es famoso.", "f_30_that_obj"), 1)
})

test_that("f_31/f_32: quien/el cual occupy their own subject/object bins", {
  skip_if_not_installed("udpipe")
  r1 <- run_biber("La autora, quien presentó el proyecto, respondió.")
  expect_equal(as.numeric(r1$f_31_wh_subj), 1)
  expect_equal(as.numeric(r1$f_29_that_subj), 0)

  r2 <- run_biber("El informe, el cual revisamos ayer, ya se publicó.")
  expect_equal(as.numeric(r2$f_31_wh_subj), 0)
  expect_equal(as.numeric(r2$f_32_wh_obj), 1)
  expect_equal(as.numeric(r2$f_29_that_subj), 0)
})

test_that("f_33: pied-piping (preposición + relativo) — incluye 'a/con/por quien|cual'", {
  skip_if_not_installed("udpipe")
  expect_equal(feat_p2("El método por el cual se analizaron los datos falló.", "f_33_pied_piping"), 1)
  expect_equal(feat_p2("La persona con quien Juan habló se fue.", "f_33_pied_piping"), 1)
})

test_that("f_34: relativa oracional (lo que / lo cual con antecedente proposicional)", {
  skip_if_not_installed("udpipe")
  expect_equal(feat_p2("No entregó el informe, lo que retrasó el proceso.", "f_34_sentence_relatives"), 1)
  # Free relative without a propositional antecedent: NOT f_34.
  expect_equal(feat_p2("No entiendo lo que dices.", "f_34_sentence_relatives"), 0)
})

test_that("LÍMITE pro-drop: a roleless misparse is not guessed to be a subject", {
  skip_if_not_installed("udpipe")
  # GSD tags lei as ADV and attaches roleless que to the predicate adjective.
  r1 <- run_biber("La novela que leí era larga.")
  expect_equal(as.numeric(r1$f_30_that_obj), 0)
  expect_equal(as.numeric(r1$f_29_that_subj), 0)
})

# nolint end
