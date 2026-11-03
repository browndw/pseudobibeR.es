# Every nonmetric feature must have one evidence row per raw occurrence.
# Only documented parser limitations can bypass a positive detection example.

library(testthat)

# ---------------------------------------------------------------------------
# Cases: one minimal sentence per feature that triggers count >= min_count.
# Examples taken from data-raw/spanish_examples.yaml where they exist; the
# rest are sentences built on purpose to trigger the feature.
# ---------------------------------------------------------------------------

cases <- list(
  # A. Tense and aspect
  f_01_past_tense              = list(text = "María llegó tarde a la reunión.",                              min_count = 1),
  f_02_perfect_aspect          = list(text = "He terminado el informe esta mañana.",                              min_count = 1),
  f_03_present_tense           = list(text = "El profesor explica la lección con claridad.",                       min_count = 1),
  # B. Adverbials
  f_04_place_adverbials        = list(text = "El gato está aquí, encima de la mesa.",                              min_count = 1),
  f_05_time_adverbials         = list(text = "Ayer llegaron tarde, pero hoy están puntuales.",                     min_count = 1),
  # C. Pronouns (only the detectable ones; the zero-output ones are not included)
  f_06_first_person_pronouns   = list(text = "Yo creo que nosotros podemos mejorar.",                              min_count = 1),
  f_07_second_person_pronouns  = list(text = "Tú debes presentar el informe mañana.",                              min_count = 1),
  f_08_third_person_pronouns   = list(text = "Él llegó primero y ella lo saludó.",                                 min_count = 1),
  f_10_demonstrative_pronoun   = list(text = "Esto es importante, pero aquello también lo es.",                    min_count = 1),
  f_11_indefinite_pronouns     = list(text = "Alguien dejó algo sobre la mesa.",                                   min_count = 1),
  # D. Questions
  f_13_wh_question             = list(text = "¿Quién llamó? ¿Qué hora es?",                                          min_count = 1),
  # E. Nominal forms
  f_14_nominalizations         = list(text = "La organización del trabajo mejora la productividad.",               min_count = 1),
  f_16_other_nouns             = list(text = "El gato come pescado en la cocina.",                                 min_count = 1),
  # F. Passives
  f_17_agentless_passives      = list(text = "La propuesta fue aprobada sin debate.",                              min_count = 1),
  f_18_by_passives             = list(text = "La novela fue escrita por García Márquez.",                          min_count = 1),
  # G. Stative forms
  f_19_be_main_verb            = list(text = "El libro es interesante.",                                           min_count = 1),
  f_20_existential_there       = list(text = "Hay tres errores en el documento.",                                  min_count = 1),
  # H. Subordination
  f_21_that_verb_comp          = list(text = "Creo que el proyecto tendrá éxito.",                                 min_count = 1),
  f_22_that_adj_comp           = list(text = "Es importante que todos participen.",                                min_count = 1),
  f_23_wh_clause               = list(text = "No sé quién llamó ni cuándo llegó.",                                 min_count = 1),
  f_24_infinitives             = list(text = "Quiero terminar el trabajo para poder descansar.",                   min_count = 1),
  f_25_present_participle      = list(text = "Caminando por el parque, encontré a mi vecino.",                     min_count = 1),
  f_26_past_participle         = list(text = "Terminado el examen, los alumnos salieron.",                         min_count = 1),
  f_27_past_participle_whiz    = list(text = "El informe redactado ayer es muy completo.",                         min_count = 1),
  f_29_that_subj               = list(text = "El estudiante que llegó tarde no pudo entrar.",                      min_count = 1),
  f_30_that_obj                = list(text = "El libro que María escribió es famoso.",                              min_count = 1),
  f_33_pied_piping             = list(text = "El asunto del que hablamos requiere atención.",                      min_count = 1),
  f_34_sentence_relatives      = list(text = "El resultado fue inesperado, lo que sorprendió a todos.",            min_count = 1),
  f_35_because                 = list(text = "No pudo asistir porque estaba enfermo.",                             min_count = 1),
  f_36_though                  = list(text = "Aunque llovía, salimos a caminar.",                                  min_count = 1),
  f_37_if                      = list(text = "Puedes hacerlo si quieres.",                                         min_count = 1),
  f_38_other_adv_sub           = list(text = "Cuando llegues, avísame.",                                           min_count = 1),
  # I. Prepositional phrases, adjectives and adverbs
  f_39_prepositions            = list(text = "El libro está sobre la mesa de la biblioteca.",                      min_count = 1),
  f_40_adj_attr                = list(text = "Un brillante estudiante presentó una nueva solución.",               min_count = 1),
  f_41_adj_pred                = list(text = "El resultado es positivo.",                                          min_count = 1),
  f_42_adverbs                 = list(text = "Habló rápidamente y respondió claramente.",                          min_count = 1),
  # K. Lexical classes
  f_45_conjuncts               = list(text = "Sin embargo, continuaron con el proyecto.",                          min_count = 1),
  f_46_downtoners              = list(text = "El error es apenas perceptible.",                                   min_count = 1),
  f_47_hedges                  = list(text = "Quizás el resultado depende de otros factores.",                     min_count = 1),
  f_48_amplifiers              = list(text = "El rendimiento mejoró enormemente con el nuevo algoritmo.",          min_count = 1),
  f_49_emphatics               = list(text = "De hecho, los resultados confirman la hipótesis.",                   min_count = 1),
  f_50_discourse_particles     = list(text = "Bueno, pues entonces, sigamos adelante.",                            min_count = 1),
  f_51_demonstratives          = list(text = "Este libro y aquella revista son útiles.",                           min_count = 1),
  # L. Modals
  f_52_modal_possibility       = list(text = "Podemos mejorar los resultados con más datos.",                      min_count = 1),
  f_53_modal_necessity         = list(text = "Debemos revisar los datos antes de publicar.",                       min_count = 1),
  f_54_modal_predictive        = list(text = "El equipo presentará los resultados el viernes.",                    min_count = 1),
  # M. Specialized verbs
  f_55_verb_public             = list(text = "El ministro afirmó que la reforma es necesaria.",                    min_count = 1),
  f_56_verb_private            = list(text = "Creo que el análisis es correcto.",                                  min_count = 1),
  f_57_verb_suasive            = list(text = "El comité recomendó revisar el protocolo.",                          min_count = 1),
  f_58_verb_seem               = list(text = "Parece que los resultados no son concluyentes.",                     min_count = 1),
  # N. Reduced forms
  f_63_split_auxiliary         = list(text = "Ha probablemente sido analizado por el equipo.",                     min_count = 1),
  # O. Coordination
  f_64_phrasal_coordination    = list(text = "Los estudiantes y los profesores asistieron.",                       min_count = 1),
  f_65_clausal_coordination    = list(text = "Los niños jugaban en el parque y los padres conversaban tranquilamente.", min_count = 1),
  # P. Negation
  f_66_neg_synthetic           = list(text = "Nadie sabe nada sobre ese asunto.",                                  min_count = 1),
  f_67_neg_analytic            = list(text = "No llegó.",                                                          min_count = 1)
)

# ---------------------------------------------------------------------------
# Structural validation: cases covers exactly the 55 .evidence_features
# ---------------------------------------------------------------------------

test_that("cases cubre exactamente los 55 .evidence_features", {
  evidence_pool <- pseudobibeR.es:::.evidence_features
  missing <- setdiff(evidence_pool, names(cases))
  extra   <- setdiff(names(cases), evidence_pool)
  expect_equal(missing, character(0),
               info = "rasgos en .evidence_features sin caso de prueba")
  expect_equal(extra, character(0),
               info = "casos de prueba para rasgos que no son evidence_features")
})

# ---------------------------------------------------------------------------
# UDPipe model setup (shared by all the tests in the file)
# ---------------------------------------------------------------------------

ud_model <- .fc_ud_or_null()

# ---------------------------------------------------------------------------
# Parametrized tests: one per feature
# ---------------------------------------------------------------------------

udpipe_limit  <- pseudobibeR.es:::.udpipe_limited_features

invariant_for <- function(feature) {
  if (feature %in% udpipe_limit) return("skip_udpipe")
  return("strict")
}

for (feat in names(cases)) {
  local({
    feature_name <- feat
    case         <- cases[[feat]]
    mode         <- invariant_for(feature_name)

    label <- switch(mode,
      strict         = paste0(feature_name, " [strict]: count == nrow(evidence)"),
      skip_udpipe    = paste0(feature_name, " [UDPipe-limited]: skipped (parser subreporta)")
    )

    test_that(label, {
      if (mode == "skip_udpipe") {
        skip("UDPipe-limited feature: parser subreporta en construcciones especificas (ver .udpipe_limited_features)")
      }
      skip_if_not_installed("udpipe")
      skip_if(is.null(ud_model), "modelo UDPipe no disponible")
      skip_if_not(exists("biber_es_traced", envir = asNamespace("pseudobibeR.es")),
                  "biber_es_traced() aun no implementada")

      parsed <- udpipe::udpipe_annotate(ud_model, x = case$text, doc_id = "t1")
      # normalize=FALSE so that counts are integers comparable to nrow(evidence).
      # With normalize=TRUE the counts are scaled to "per 1000 tokens" and break
      # the equality/inequality by definition; the evidence is always raw.
      result <- pseudobibeR.es:::biber_es_traced(parsed, measure = "none", normalize = FALSE)

      # Count invariant (independent of the mode)
      expect_true(feature_name %in% colnames(result$counts),
                  info = paste0(feature_name, " ausente de result$counts"))
      observed_count <- result$counts[[feature_name]][[1]]
      expect_gte(observed_count, case$min_count)

      # Evidence invariant according to the topology
      ev_rows <- sum(result$evidence$feature == feature_name)

      if (mode == "strict") {
        expect_equal(ev_rows, observed_count,
                     info = paste0(feature_name,
                                   " [strict]: count=", observed_count,
                                   " pero nrow(evidence)=", ev_rows))
      }
    })
  })
}
