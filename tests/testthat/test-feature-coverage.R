# Positive/negative coverage of every Spanish feature, plus the evidence
# invariant (one evidence row per raw occurrence).
# Cases derived from biber_espanol_completo.md and data-raw/spanish_examples.yaml.
#
# Each case: feature, named positive sentences (value = minimum count), and an
# optional negative sentence with its maximum count (NA when the feature is a
# residual or metric category without a clean negative). All sentences are
# parsed and traced in a single batch. Features in .udpipe_limited_features
# (f_26, f_27) keep only the positives that spanish-gsd parses reliably.

coverage_cases <- list(
  list("f_01_past_tense",
       c("Juan llegó tarde y habló con ella." = 1, "María llegó tarde a la reunión." = 1),
       "Juan habla mientras ella lee.", 0),
  list("f_02_perfect_aspect",
       c("He terminado el informe esta mañana." = 1),
       "Terminé el informe ayer.", 0),
  list("f_03_present_tense",
       c("María trabaja en la oficina central." = 1, "El profesor explica la lección con claridad." = 1),
       "María trabajó ayer en casa.", 0),
  list("f_04_place_adverbials",
       c("El perro está aquí y el gato allí afuera." = 1, "El gato está aquí, encima de la mesa." = 1),
       "Hoy comimos muy temprano.", 0),
  list("f_05_time_adverbials",
       c("Llegó ayer y se marchará mañana temprano." = 1, "Ayer llegaron tarde, pero hoy están puntuales." = 1),
       "El libro está aquí dentro.", 0),
  list("f_06_first_person_pronouns",
       c("Yo creo que nosotros ganaremos." = 1, "Yo creo que nosotros podemos mejorar." = 1),
       "Él dijo que ella vendría.", 0),
  list("f_07_second_person_pronouns",
       c("Tú sabes que usted tiene razón." = 1, "Tú debes presentar el informe mañana." = 1),
       "Yo sé que él viene.", 0),
  list("f_08_third_person_pronouns",
       c("Él la vio y ella le habló a ellos." = 1, "Él llegó primero y ella lo saludó." = 1),
       "Yo te di el libro a ti.", 0),
  list("f_09_pronoun_it",
       c("Llueve." = 1),
       "Llegó ayer. Quiere descansar.", 0),
  list("f_10_demonstrative_pronoun",
       c("Eso es importante. Esto me gusta mucho." = 1, "Esto es importante, pero aquello también lo es." = 1),
       "Este libro es mío y esa casa es tuya.", 0),
  list("f_11_indefinite_pronouns",
       c("Alguien llamó pero nadie respondió nada." = 1, "Alguien dejó algo sobre la mesa." = 1),
       "Un perro y una casa grande.", 0),
  list("f_12_proverb_do",
       c("Luis también lo hizo." = 1),
       "Hizo una mesa. Lo hizo llorar.", 0),
  list("f_13_wh_question",
       c("¿Qué quieres? ¿Dónde estás ahora?" = 1, "¿Quién llamó? ¿Qué hora es?" = 1),
       "La casa que compré es grande.", 0),
  list("f_14_nominalizations",
       c("La producción y la evaluación del conocimiento." = 1, "La organización del trabajo mejora la productividad." = 1),
       "El lado del grado del partido.", 0),
  list("f_15_gerunds",
       c("El saber es valioso." = 1),
       "Es importante descansar. Quiere estudiar.", 0),
  list("f_16_other_nouns",
       c("El perro corre en el parque con la pelota." = 2, "El gato come pescado en la cocina." = 1),
       NA, NA),
  list("f_17_agentless_passives",
       c("Se publicaron los resultados. La tarea fue realizada." = 1, "La propuesta fue aprobada sin debate." = 1),
       "Juan comió la manzana roja.", 0),
  list("f_18_by_passives",
       c("El libro fue escrito por María González." = 1, "La novela fue escrita por García Márquez." = 1),
       "La tarea fue realizada rápido.", 0),
  list("f_19_be_main_verb",
       c("El libro es interesante y la casa está limpia." = 1, "El libro es interesante." = 1),
       "El informe ha sido escrito hoy.", 0),
  list("f_20_existential_there",
       c("Hay un problema grave en el sistema." = 1, "Hay tres errores en el documento." = 1),
       "Hay que esperar el resultado final.", 0),
  list("f_21_that_verb_comp",
       c("Dijo que vendría mañana sin falta." = 1, "Creo que el proyecto tendrá éxito." = 1),
       "La casa que compré es vieja.", 0),
  list("f_22_that_adj_comp",
       c("Es importante que vengas pronto." = 1, "Es importante que todos participen." = 1),
       "Corrió porque tenía prisa.", 0),
  list("f_23_wh_clause",
       c("No sé qué quieres ni dónde vives." = 1, "No sé quién llamó ni cuándo llegó." = 1),
       "¿Qué quieres exactamente?", 0),
  list("f_24_infinitives",
       c("Quiero comer y necesito dormir ahora." = 1, "Quiero terminar el trabajo para poder descansar." = 1),
       "Como pan todos los días.", 0),
  list("f_25_present_participle",
       c("Caminando por la calle, pensó en todo." = 1, "Caminando por el parque, encontré a mi vecino." = 1),
       "Está hablando por teléfono ahora.", 0),
  list("f_26_past_participle",
       c("Terminada la reunión, todos se fueron." = 1),
       "Ha terminado la reunión temprano.", 0),
  list("f_27_past_participle_whiz",
       c("Los métodos utilizados fueron buenos." = 1),
       "Terminada la sesión, salieron.", 0),
  list("f_29_that_subj",
       c("El hombre que vino ayer era alto." = 1, "El estudiante que llegó tarde no pudo entrar." = 1),
       "Dijo que vino tarde.", 0),
  list("f_30_that_obj",
       c("El libro que María escribió es famoso." = 1),
       "Es importante que vengas.", 0),
  list("f_31_wh_subj",
       c("Los candidatos, quienes aprobaron el examen, recibieron una beca." = 1),
       "El libro que María escribió es famoso.", 0),
  list("f_32_wh_obj",
       c("El informe, el cual María revisó, fue publicado." = 1),
       "El asunto del cual hablamos es importante.", 0),
  list("f_33_pied_piping",
       c("La casa en la que vivo. La persona con quien hablé. El motivo por el cual vino. Los temas de los cuales hablamos." = 2, "El asunto del que hablamos requiere atención." = 1),
       "El libro que leí ayer.", 0),
  list("f_34_sentence_relatives",
       c("Llegó tarde, lo cual molestó a todos." = 1, "El resultado fue inesperado, lo que sorprendió a todos." = 1),
       "Lo que quieras está bien.", 0),
  list("f_35_because",
       c("No vino porque estaba enfermo." = 1, "No pudo asistir porque estaba enfermo." = 1),
       "Aunque llovía, salió igual.", 0),
  list("f_36_though",
       c("Aunque llovía, salió a la calle." = 1, "Aunque llovía, salimos a caminar." = 1),
       "Porque llovía, no salió.", 0),
  list("f_37_if",
       c("Si llueve mañana, no salgo." = 1, "Puedes hacerlo si quieres." = 1),
       "Preguntó si vendría a la fiesta.", 0),
  list("f_38_other_adv_sub",
       c("Mientras trabajaba, escuchaba música." = 1, "Cuando llegues, avísame." = 1),
       "Comió pan y bebió agua.", 0),
  list("f_39_prepositions",
       c("El libro de María está en la mesa con cuidado." = 3, "El libro está sobre la mesa de la biblioteca." = 1),
       NA, NA),
  list("f_40_adj_attr",
       c("Un libro interesante y una casa grande." = 1, "Un brillante estudiante presentó una nueva solución." = 1),
       "El libro es interesante.", 0),
  list("f_41_adj_pred",
       c("El libro es interesante y útil." = 1, "El resultado es positivo." = 1),
       "Un libro interesante llegó.", 0),
  list("f_42_adverbs",
       c("Habló rápidamente y actuó claramente." = 1, "Habló rápidamente y respondió claramente." = 1),
       NA, NA),
  list("f_43_type_token",
       c("El gato y el perro corren juntos felices." = 0),
       NA, NA),
  list("f_44_mean_word_length",
       c("La producción de conocimiento riguroso." = 1),
       NA, NA),
  list("f_45_conjuncts",
       c("Llegó tarde. Sin embargo, lo intentó. Además ganó." = 1, "Sin embargo, continuaron con el proyecto." = 1),
       "Salió sin nada en las manos.", 0),
  list("f_46_downtoners",
       c("Es casi imposible y apenas visible." = 1, "El error es apenas perceptible." = 1),
       "Es muy claro y totalmente cierto.", 0),
  list("f_47_hedges",
       c("Quizás venga. A lo mejor llega mañana." = 1, "Quizás el resultado depende de otros factores." = 1),
       "Vendrá seguro y sin dudas.", 0),
  list("f_48_amplifiers",
       c("Es muy bueno y totalmente claro." = 1, "El rendimiento mejoró enormemente con el nuevo algoritmo." = 1),
       "Es casi bueno y apenas claro.", 0),
  list("f_49_emphatics",
       c("Realmente lo hizo. De verdad que sí." = 1, "De hecho, los resultados confirman la hipótesis." = 1),
       "Lo hizo ayer por la tarde.", 0),
  list("f_50_discourse_particles",
       c("Bueno, o sea, es decir, llegó tarde." = 2, "Bueno, pues entonces, sigamos adelante." = 1),
       "El alumno bueno estudió mucho.", 0),
  list("f_51_demonstratives",
       c("Este libro y esa casa son nuevos." = 1, "Este libro y aquella revista son útiles." = 1),
       "Eso es mío y esto es tuyo.", 0),
  list("f_52_modal_possibility",
       c("Puedo ir y podríamos hablar luego." = 1, "Podemos mejorar los resultados con más datos." = 1),
       "Tiene mucho poder político.", 0),
  list("f_53_modal_necessity",
       c("Hay que esperar. Tengo que irme. Debe salir." = 2, "Debemos revisar los datos antes de publicar." = 1),
       "Tengo un perro y un gato.", 0),
  list("f_54_modal_predictive",
       c("Hablará mañana. Vamos a comer pronto." = 1, "El equipo presentará los resultados el viernes." = 1),
       "Habló ayer y comió tarde.", 0),
  list("f_55_verb_public",
       c("Dijo que sí y afirmó lo contrario." = 1, "El ministro afirmó que la reforma es necesaria." = 1),
       "Comió pan y durmió bien.", 0),
  list("f_56_verb_private",
       c("Creo que sé la respuesta y pienso mucho." = 1, "Creo que el análisis es correcto." = 1),
       "Corrió rápido y saltó alto.", 0),
  list("f_57_verb_suasive",
       c("Le pedí que viniera y recomendé esperar." = 1, "El comité recomendó revisar el protocolo." = 1),
       "Comió pan y bebió agua.", 0),
  list("f_58_verb_seem",
       c("Parece que va a llover pronto." = 1, "Parece que los resultados no son concluyentes." = 1),
       "Apareció en la reunión tarde.", 0),
  list("f_59_contractions",
       c("Toy aquí. Ontas? Ven pa’ca’rriba." = 3),
       "Al salir del taller, compró un toy.", 0),
  list("f_60_that_deletion",
       c("Pienso ir. Creo haber entendido." = 2, "Nosotros esperamos lleguen mañana." = 1),
       "Pienso que voy a ir. Puedo ir.", 0),
  list("f_61_stranded_preposition",
       c("Ese es el chico que hablé de él." = 1),
       "El chico que habló de él llegó.", 0),
  list("f_62_split_infinitive",
       c("Trato de realmente comprender el problema." = 1),
       "Lo hizo para no volver.", 0),
  list("f_63_split_auxiliary",
       c("Ha probablemente sido analizado el caso." = 1, "Ha probablemente sido analizado por el equipo." = 1),
       "Fue mostrado el resultado.", 0),
  list("f_64_phrasal_coordination",
       c("Juan y María vinieron temprano." = 1, "Los estudiantes y los profesores asistieron." = 1),
       "Vino pero se fue rápido.", 0),
  list("f_65_clausal_coordination",
       c("Llegó tarde. Y luego se fue." = 1, "Los niños jugaban en el parque y los padres conversaban tranquilamente." = 1),
       "Juan y María vinieron.", 0),
  list("f_66_neg_synthetic",
       c("Nadie vino y nada pasó. Nunca lo hizo." = 2, "Nadie sabe nada sobre ese asunto." = 1),
       "Vino alguien con algo.", 0),
  list("f_67_neg_analytic",
       c("No vino y no lo hizo nunca." = 1, "No llegó." = 1),
       "No obstante, finalmente llegó.", 0)

)

zero_output_probes <- c(
  "Es una prueba simple en español.",
  "Caminando rápidamente, vio que él lo hizo todo.",
  "It would be a sentence that triggers in English."
)

regression_texts <- c(
  "Comí pizza ayer.", "Llegaste temprano ayer.", "Comió pizza en el parque.",
  "O sea, llegó tarde.", "El equipo que fue asignado finalizó el proyecto.",
  "Quizás el resultado depende de otros factores.", "Se debe seguir el procedimiento.",
  "No sé qué quieres ni dónde vives.", "Prueba de contrato.",
  "El informe fue redactado por el equipo de investigación.", "En 2024 compré 35 libros."
)

coverage <- ud_batch(c(
  unlist(lapply(coverage_cases, function(x) names(x[[2]]))),
  na.omit(vapply(coverage_cases, function(x) as.character(x[[3]]), character(1))),
  zero_output_probes, regression_texts
))

test_that("every evidence feature has a positive coverage case", {
  covered <- vapply(coverage_cases, `[[`, character(1), 1)
  expect_equal(setdiff(pseudobibeR.es:::.evidence_features, covered), character(0))
})

for (cs in coverage_cases) {
  local({
    feat <- cs[[1]]; positives <- cs[[2]]; neg_t <- cs[[3]]; neg_max <- cs[[4]]

    test_that(paste0(feat, " — cobertura positiva"), {
      for (text in names(positives)) {
        val <- coverage$count(text, feat)
        expect_length(val, 1)
        if (feat == "f_43_type_token") {
          expect_gt(val, 0); expect_lte(val, 1)
        } else if (feat == "f_44_mean_word_length") {
          expect_gt(val, 3); expect_lt(val, 15)
        } else {
          expect_gte(val, positives[[text]], label = paste(feat, "en:", text))
        }
      }
    })

    if (!is.na(neg_t)) {
      test_that(paste0(feat, " — cobertura negativa"), {
        expect_lte(coverage$count(neg_t, feat), neg_max, label = paste(feat, "en:", neg_t))
      })
    }
  })
}

test_that("every count equals its evidence rows in every coverage sentence", {
  result <- coverage$result()
  for (feat in pseudobibeR.es:::.evidence_features) {
    rows <- result$evidence[result$evidence$feature == feat, ]
    expect_equal(
      tabulate(match(rows$doc_id, result$counts$doc_id), nbins = nrow(result$counts)),
      as.integer(result$counts[[feat]]),
      info = feat
    )
  }
})

# ── Specific critical tests requested by audit/FEATURE_AUDIT.md (brief) ───

test_that("f_06 — pro-drop: oración sin pronombre explícito cuenta 0", {
  # Spec §f_06: lexical list + morphological filter (Person=1) on PRON;
  # it must NOT fire on conjugated verbs. UDPipe spanish-gsd sometimes
  # mis-tags verbs as PRON|Person=1 (case "Comí"); the morphological
  # supplement must also filter by form/lemma in the known list.
  expect_equal(coverage$count("Comí pizza ayer.", "f_06_first_person_pronouns"), 0)
})

test_that("f_07 — verbal Person=2 alone must not count (regression-guard)", {
  expect_equal(coverage$count("Llegaste temprano ayer.", "f_07_second_person_pronouns"), 0,
               info = "Verbal Person=2 inflection alone must not count per spec §f_07")
})

test_that("f_08 — verbal Person=3 alone must not count (regression-guard)", {
  # No pronoun or possessive: 'Comió' (VERB, Person=3) must not inflate f_08.
  # (Phase 1: possessives such as 'sus' do count now; that is why they are avoided here.)
  expect_equal(coverage$count("Comió pizza en el parque.", "f_08_third_person_pronouns"), 0,
               info = "Verbal Person=3 inflection alone must not count per spec §f_08")
})

test_that("f_19 — 'O sea, ...' no cuenta sea como ser/estar léxico", {
  # Spec: the compound o_sea must absorb sea before f_19 sees it.
  expect_equal(coverage$count("O sea, llegó tarde.", "f_19_be_main_verb"), 0,
               info = "sea debe ser parte del compound o_sea (→ f_50)")
})

test_that("f_23 — relativa con 'que' sin tilde cuenta 0 (es f_29, no wh-clause)", {
  # Spec §f_23 Exclude: "Relative uses of the same words without accents".
  # UDPipe spanish-gsd marks relative "que" as ambivalent PronType=Int,Rel.
  # Fix: accent in the surface form + exclusion of acl/acl:relcl heads.
  expect_equal(coverage$count("El equipo que fue asignado finalizó el proyecto.",
                              "f_23_wh_clause"), 0)
})

test_that("f_47 — quizás detectado en locale C (regression-guard UTF-8)", {
  # Documented root cause: in the "C" locale, quanteda::tokens drops
  # Encoding="UTF-8" and the accents are left as raw bytes
  # ("quizás" -> "quiz<c3><a1>s"), breaking the match against the dict.
  # Fix: force LC_CTYPE=UTF-8 inside biber_fixture() with on.exit.
  expect_gte(coverage$count("Quizás el resultado depende de otros factores.", "f_47_hedges"), 1)
})

test_that("f_47 — biber restaura LC_CTYPE al salir", {
  old <- Sys.getlocale("LC_CTYPE")
  invisible(run_biber("Quizás venga."))
  expect_equal(Sys.getlocale("LC_CTYPE"), old,
               info = "biber debe restaurar el locale tras la llamada")
})

test_that("f_24 exposes the known root misparse rather than suppressing roots", {
  model <- .fc_ud()
  parsed <- as.data.frame(udpipe::udpipe_annotate(
    model, x = "Quiero terminar el trabajo para poder descansar."
  ))
  quiero <- tolower(parsed$token) == "quiero"
  expect_equal(sum(quiero), 1)
  expect_true(grepl("VerbForm=Inf", parsed$feats[quiero]))
  expect_equal(biber_fixture(parsed, measure = "none", normalize = FALSE)$f_24_infinitives, 4)

  parsed$feats[quiero] <- "Mood=Ind|Person=1|Number=Sing|Tense=Pres|VerbForm=Fin"
  expect_equal(biber_fixture(parsed, measure = "none", normalize = FALSE)$f_24_infinitives, 3)
  expect_setequal(audit_fixture(parsed, feature = "f_24_infinitives")$keyword,
                  c("terminar", "poder", "descansar"))
})

test_that("f_24 — perífrasis modal con AUX (root|Inf legítimo) sí cuenta", {
  # Regression guard: 'Se debe seguir' → 'seguir' is root|Inf but has
  # 'debe' (AUX|Fin) as a child → it counts as a legitimate infinitive.
  expect_gte(coverage$count("Se debe seguir el procedimiento.", "f_24_infinitives"), 1)
})

test_that("f_23 — interrogativa indirecta legítima sigue contando", {
  # Regression guard: the fix must not break the detection of accented wh-words
  # in argument function (ccomp/xcomp).
  expect_gte(coverage$count("No sé qué quieres ni dónde vives.", "f_23_wh_clause"), 1)
})

test_that("f_44 — caso corto sigue dentro del rango 4-6 (no inflado)", {
  # Defensive regression: 'Prueba de contrato.' gave 7 before the fix.
  val <- coverage$count("Prueba de contrato.", "f_44_mean_word_length")
  expect_gt(val, 4); expect_lt(val, 6)
})

# ── Zero-output: the remaining constant compatibility column ──────────────
# Any input must return exactly 0, including an English sentence that would
# trigger the feature in FR/EN.

test_that("f_28_present_participle_whiz — zero-output invariante con cualquier input", {
  for (probe in zero_output_probes) {
    row <- coverage$row(probe)
    expect_true("f_28_present_participle_whiz" %in% names(row))
    expect_equal(as.numeric(row$f_28_present_participle_whiz), 0,
                 info = paste("f_28 ≠ 0 en:", probe))
  }
})

# ── Regression §3.J f_44 (worked example) ─────────────────────────────────
# D-51: mean of nchar() over purely alphabetic word tokens, with no POS filter
# and no rounding. Historical bug: it filtered to NOUN/VERB/ADJ/ADV and
# returned ~8.75 on the worked example (it should be 5.22).

test_that("f_44 uses all alphabetic word tokens regardless of POS (D-51)", {
  val <- coverage$count("El informe fue redactado por el equipo de investigación.",
                        "f_44_mean_word_length")
  expect_equal(round(val, 2), 5.22, info = "47 letters / 9 alphabetic tokens = 5.22")
})

test_that("f_44 excludes numerals and is not rounded (D-51)", {
  expect_equal(coverage$count("En 2024 compré 35 libros.", "f_44_mean_word_length"), 14 / 3)
})

test_that("f_44 is invariant under normalize=TRUE (ratio, not count)", {
  texto <- "El informe fue redactado por el equipo de investigación."
  r_norm <- run_biber(texto, normalize = TRUE)
  expect_equal(coverage$count(texto, "f_44_mean_word_length"),
               as.numeric(r_norm$f_44_mean_word_length),
               info = "f_44 no debe re-escalarse por normalize=TRUE")
})
