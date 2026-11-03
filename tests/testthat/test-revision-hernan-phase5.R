# test-revision-hernan-phase5.R
#
# Phase 5: rule refinements. Counts verified against spanish-gsd.
# The rules blocked by the model (f_18 obl:agent) are NOT forced and are
# documented in test-udpipe-tag-verification.R.
#
# nolint start: line_length_linter, object_name_linter

p5 <- function(text, feature) as.numeric(run_biber(text)[[feature]])

test_that("f_17: la pasiva refleja cuenta; la impersonal con se se excluye", {
  skip_if_not_installed("udpipe")
  expect_equal(p5("Se publicaron los informes.", "f_17_agentless_passives"), 1)   # reflexive passive (nsubj)
  expect_equal(p5("Se venden casas baratas.", "f_17_agentless_passives"), 1)       # reflexive passive
  expect_equal(p5("Se entrevistó a los candidatos.", "f_17_agentless_passives"), 0) # impersonal (obj with 'a')
  expect_equal(p5("Se recomienda leer el manual.", "f_17_agentless_passives"), 0)   # impersonal (xcomp)
  # The periphrastic passive is not affected:
  expect_equal(p5("La tarea fue realizada.", "f_17_agentless_passives"), 1)
})

test_that("f_52: 'puede que' cuenta como posibilidad (correlato de may/might)", {
  skip_if_not_installed("udpipe")
  expect_equal(p5("Puede que llueva mañana.", "f_52_modal_possibility"), 1)
  # poder + infinitive still counts:
  expect_equal(p5("Puede hacerlo fácilmente.", "f_52_modal_possibility"), 1)
})

test_that("f_64: cubre la coordinación de verbos (SV compartido), sin doblar f_65", {
  skip_if_not_installed("udpipe")
  rv <- run_biber("Lee y escribe muy bien.")
  expect_equal(as.numeric(rv$f_64_phrasal_coordination), 1)   # verbs, shared subject
  expect_equal(as.numeric(rv$f_65_clausal_coordination), 0)
  expect_equal(p5("Juan y María vinieron temprano.", "f_64_phrasal_coordination"), 1) # nouns
  # Clausal coordination (own subjects) -> f_65, not f_64:
  rc <- run_biber("Ella canta y él baila.")
  expect_equal(as.numeric(rc$f_65_clausal_coordination), 1)
  expect_equal(as.numeric(rc$f_64_phrasal_coordination), 0)
})

test_that("f_67: 'no' de foco sobre constituyentes no verbales cuenta; excluye sustantivo", {
  skip_if_not_installed("udpipe")
  expect_equal(p5("Una respuesta no definitiva.", "f_67_neg_analytic"), 1)  # focus on an ADJ
  expect_equal(p5("No muy lejos de aquí.", "f_67_neg_analytic"), 1)         # focus on an ADV
  expect_equal(p5("No corre rápido hoy.", "f_67_neg_analytic"), 1)          # preverbal (control)
  expect_equal(p5("El no del comité fue claro.", "f_67_neg_analytic"), 0)   # 'no' as a noun
})

test_that("f_37: 'a menos que' y 'salvo que' cuentan como condicional (no en f_38)", {
  skip_if_not_installed("udpipe")
  r1 <- run_biber("No saldré a menos que pare la lluvia.")
  expect_equal(as.numeric(r1$f_37_if), 1)
  expect_equal(as.numeric(r1$f_38_other_adv_sub), 0)
  r2 <- run_biber("Salvo que llueva, saldremos.")
  expect_equal(as.numeric(r2$f_37_if), 1)
  expect_equal(as.numeric(r2$f_38_other_adv_sub), 0)
  # conditional 'si' still counts; 'cuando' stays in f_38:
  expect_equal(p5("Si llueve no salgo.", "f_37_if"), 1)
  expect_equal(p5("Cuando llegó, todos aplaudieron.", "f_38_other_adv_sub"), 1)
})

test_that("f_58: 'resultar' solo en uso copulativo de apariencia; 'parecer' siempre", {
  skip_if_not_installed("udpipe")
  expect_equal(p5("La propuesta resulta adecuada.", "f_58_verb_seem"), 1)     # cop
  expect_equal(p5("Parece difícil el examen.", "f_58_verb_seem"), 1)          # parecer
  expect_equal(p5("Resultó ganador del premio.", "f_58_verb_seem"), 0)        # change of state
  expect_equal(p5("El accidente resultó de una falla.", "f_58_verb_seem"), 0) # consequence
})

test_that("f_63: adverbio interpuesto (auxiliar y perífrasis modal); excluye sujeto", {
  skip_if_not_installed("udpipe")
  expect_equal(p5("Ha siempre sostenido su postura.", "f_63_split_auxiliary"), 1)  # perfect AUX
  expect_equal(p5("Puede fácilmente hacerlo.", "f_63_split_auxiliary"), 1)       # modal periphrasis
  expect_equal(p5("Debe siempre revisar los datos.", "f_63_split_auxiliary"), 1)   # modal periphrasis
  expect_equal(p5("Podía yo saberlo.", "f_63_split_auxiliary"), 0)                 # intervening subject
})

test_that("f_39: una locución preposicional cuenta como UNA preposición", {
  skip_if_not_installed("udpipe")
  # Unaccented phrases (locale-robust): 'a causa de' and 'por medio de'
  # have 2 ADPs but count 1. (Accented ones such as 'en relación con'
  # need a UTF-8 locale; see deaccent in features_modals_verbs.R.)
  expect_equal(p5("Llegó a causa de la lluvia.", "f_39_prepositions"), 1)
  expect_equal(p5("Lo resolvió por medio de un truco.", "f_39_prepositions"), 1)
  # Single prepositions (incl. the contractions al/del) count once each:
  expect_equal(p5("Fue del pueblo al río.", "f_39_prepositions"), 2)
})

# nolint end
