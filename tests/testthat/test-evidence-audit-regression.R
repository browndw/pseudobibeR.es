# Specific regression tests for the 4 features whose detector was
# fixed during the audit (commits 7479f0e..8d285a5):
#
#   f_06_first_person_pronouns -- morphological supplement with a filter on
#                                 known forms/lemmas
#   f_23_wh_clause             -- surface accent + exclusion of
#                                 acl/acl:relcl heads
#   f_24_infinitives           -- verbal infinitives, including roots and auxiliaries
#   f_47_hedges                -- UTF-8 locale forced in biber so
#                                 that quanteda matches accents
#
# Phase 3 motivation (M1 design): the invariant count == nrow(evidence)
# validates that the number is right. These tests also check that the
# EVIDENCE contains the SPECIFIC expected TOKENS. It catches the subtle
# bug where the detector counts correctly but captures the wrong
# tokens -- a regression that a pure invariant test does not detect.

library(testthat)

# Locate model (relative or in package root)
ud_model <- .fc_ud_or_null()

trace_text <- function(text, doc_id = "t1") {
  parsed <- udpipe::udpipe_annotate(ud_model, x = text, doc_id = doc_id)
  pseudobibeR.es:::biber_es_traced(parsed, measure = "none", normalize = FALSE)
}

# ---------------------------------------------------------------------------
# f_06_first_person_pronouns -- audit commit bcbf08c
# The bug: the morphological supplement accepted any PRON with Person=1
# without checking that the form/lemma was a known pronoun. Conjugated
# verbs (caigamos, vivamos) with Person=1 counted as pronouns.
# Fix: filter by form or lemma in a closed list of pronouns.
# Regression target: "Yo creo que nosotros podemos mejorar" must count yo
# and nosotros, NOT the verbs.
# ---------------------------------------------------------------------------

test_that("f_06: la evidencia contiene yo y nosotros, no los verbos conjugados", {
  skip_if(is.null(ud_model), "modelo UDPipe no disponible")
  skip_if_not(exists("biber_es_traced", envir = asNamespace("pseudobibeR.es")),
              "biber_es_traced() requerida")

  result <- trace_text("Yo creo que nosotros podemos mejorar.")
  ev_f06 <- result$evidence[result$evidence$feature == "f_06_first_person_pronouns", ]

  expect_gte(nrow(ev_f06), 2L)
  tokens_lower <- tolower(ev_f06$token)
  expect_true("yo"        %in% tokens_lower, info = "yo ausente de evidencia f_06")
  expect_true("nosotros"  %in% tokens_lower, info = "nosotros ausente de evidencia f_06")

  # Conjugated verbs with Person=1 must not appear (regression of the f_06 bug)
  expect_false("creo"     %in% tokens_lower, info = "creo aparece como falso positivo")
  expect_false("podemos"  %in% tokens_lower, info = "podemos aparece como falso positivo")
})

# ---------------------------------------------------------------------------
# f_23_wh_clause -- audit commit 76bc5c2
# The bug: it accepted relative "que" (without an accent) in any dep_rel, so
# "El equipo que fue asignado" counted "que" as a wh-clause when
# it is a relative. Fix: require a surface accent + exclude acl/acl:relcl heads.
# Regression target: "No se quien llamo ni cuando llego" has quien and
# cuando (both accented, in a subordinate clause).
# ---------------------------------------------------------------------------

test_that("f_23: la evidencia contiene quien y cuando (acentuados, no relativos)", {
  skip_if(is.null(ud_model), "modelo UDPipe no disponible")
  skip_if_not(exists("biber_es_traced", envir = asNamespace("pseudobibeR.es")),
              "biber_es_traced() requerida")

  result <- trace_text("No sé quién llamó ni cuándo llegó.")
  ev_f23 <- result$evidence[result$evidence$feature == "f_23_wh_clause", ]

  expect_gte(nrow(ev_f23), 2L)
  tokens_lower <- tolower(ev_f23$token)
  expect_true("quién"  %in% tokens_lower, info = "quien ausente de evidencia f_23")
  expect_true("cuándo" %in% tokens_lower, info = "cuando ausente de evidencia f_23")
})

test_that("f_23: 'que' relativo (sin tilde) NO aparece en la evidencia", {
  skip_if(is.null(ud_model), "modelo UDPipe no disponible")
  skip_if_not(exists("biber_es_traced", envir = asNamespace("pseudobibeR.es")),
              "biber_es_traced() requerida")

  result <- trace_text("El equipo que fue asignado al proyecto comenzó ayer.")
  ev_f23 <- result$evidence[result$evidence$feature == "f_23_wh_clause", ]

  expect_equal(nrow(ev_f23), 0L,
               info = "evidencia f_23 contiene tokens cuando deberia estar vacia")
})

# ---------------------------------------------------------------------------
# f_24_infinitives -- independent annotations separate the rule from model errors.
# ---------------------------------------------------------------------------

test_that("f_24 evidence includes infinitive auxiliaries but not finite quiero", {
  tokens <- contract_clause(
    "infinitives", c("Quiero", "terminar", "el", "trabajo", "para", "poder", "descansar", "."),
    c("querer", "terminar", "el", "trabajo", "para", "poder", "descansar", "."),
    c("VERB", "VERB", "DET", "NOUN", "ADP", "AUX", "VERB", "PUNCT"),
    c(0, 1, 4, 2, 7, 7, 2, 1),
    c("root", "xcomp", "det", "obj", "mark", "aux", "advcl", "punct"),
    c("Mood=Ind|Person=1|Tense=Pres|VerbForm=Fin", "VerbForm=Inf", "PronType=Art",
      NA, NA, "VerbForm=Inf", "VerbForm=Inf", NA)
  )
  result <- pseudobibeR.es:::biber_es_traced(tokens, measure = "none", normalize = FALSE)
  ev_f24 <- result$evidence[result$evidence$feature == "f_24_infinitives", ]
  expect_equal(nrow(ev_f24), 3L)
  expect_setequal(ev_f24$token, c("terminar", "poder", "descansar"))
  expect_false("quiero" %in% ev_f24$token)
})

test_that("f_24: 'Se debe seguir' SI cuenta seguir (root|Inf con AUX|Fin hijo)", {
  skip_if(is.null(ud_model), "modelo UDPipe no disponible")
  skip_if_not(exists("biber_es_traced", envir = asNamespace("pseudobibeR.es")),
              "biber_es_traced() requerida")

  result <- trace_text("Se debe seguir el procedimiento.")
  ev_f24 <- result$evidence[result$evidence$feature == "f_24_infinitives", ]

  expect_gte(nrow(ev_f24), 1L)
  expect_true("seguir" %in% tolower(ev_f24$token),
              info = "seguir ausente: la rama root|Inf+AUX|Fin no funciona")
})

# ---------------------------------------------------------------------------
# f_47_hedges -- audit commit 8d285a5
# The bug: in the C locale, quanteda::tokens dropped the Encoding=UTF-8 mark
# and "quizas" was left as raw bytes that did not match the dictionary.
# Fix: force LC_CTYPE=UTF-8 in biber and biber_es_traced.
# Regression target: "Quizas el resultado depende" must count quizas
# even if the R session is in the C locale.
#
# NOTE: f_47 is dict-only in v1 -- it produces no evidence rows. We only
# check the count (the regression was in the COUNT, not in the evidence).
# ---------------------------------------------------------------------------

test_that("f_47: quizas se detecta correctamente (locale UTF-8 forzado)", {
  skip_if(is.null(ud_model), "modelo UDPipe no disponible")
  skip_if_not(exists("biber_es_traced", envir = asNamespace("pseudobibeR.es")),
              "biber_es_traced() requerida")

  result <- trace_text("Quizás el resultado depende de otros factores.")
  expect_gte(result$counts$f_47_hedges[[1]], 1L,
             label = "f_47 count en 'Quizas el resultado depende'")
})

test_that("f_47: tambien funciona con multiples hedges acentuados", {
  skip_if(is.null(ud_model), "modelo UDPipe no disponible")
  skip_if_not(exists("biber_es_traced", envir = asNamespace("pseudobibeR.es")),
              "biber_es_traced() requerida")

  result <- trace_text("Quizás y tal vez los resultados son válidos.")
  expect_gte(result$counts$f_47_hedges[[1]], 1L)
})
