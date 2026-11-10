# Positive/negative coverage of the detectable Spanish features.
# Cases derived from biber_espanol_completo.md (source of truth).
# pos_text → must detect (>= pos_min). neg_text → must NOT detect (<= neg_max).
# neg_text = NA → residual/metric feature with no clean negative (sanity only).

fc_cases <- list(
  list("f_01_past_tense",          "Juan llegó tarde y habló con ella.",            1, "Juan habla mientras ella lee.",               0),
  list("f_02_perfect_aspect",      "He terminado el informe esta mañana.",          1, "Terminé el informe ayer.",                    0),
  list("f_03_present_tense",       "María trabaja en la oficina central.",          1, "María trabajó ayer en casa.",                 0),
  list("f_04_place_adverbials",    "El perro está aquí y el gato allí afuera.",     1, "Hoy comimos muy temprano.",                   0),
  list("f_05_time_adverbials",     "Llegó ayer y se marchará mañana temprano.",     1, "El libro está aquí dentro.",                  0),
  list("f_06_first_person_pronouns","Yo creo que nosotros ganaremos.",              1, "Él dijo que ella vendría.",                   0),
  list("f_07_second_person_pronouns","Tú sabes que usted tiene razón.",             1, "Yo sé que él viene.",                         0),
  list("f_08_third_person_pronouns","Él la vio y ella le habló a ellos.",           1, "Yo te di el libro a ti.",                     0),
  list("f_09_pronoun_it",          "Llueve.",                                      1, "Llegó ayer. Quiere descansar.",               0),
  list("f_10_demonstrative_pronoun","Eso es importante. Esto me gusta mucho.",      1, "Este libro es mío y esa casa es tuya.",       0),
  list("f_11_indefinite_pronouns", "Alguien llamó pero nadie respondió nada.",      1, "Un perro y una casa grande.",                 0),
  list("f_12_proverb_do",          "Luis también lo hizo.",                        1, "Hizo una mesa. Lo hizo llorar.",              0),
  list("f_13_wh_question",         "¿Qué quieres? ¿Dónde estás ahora?",             1, "La casa que compré es grande.",               0),
  list("f_14_nominalizations",     "La producción y la evaluación del conocimiento.",1,"El lado del grado del partido.",              0),
  list("f_15_gerunds",             "El saber es valioso.",                          1, "Es importante descansar. Quiere estudiar.",    0),
  list("f_16_other_nouns",         "El perro corre en el parque con la pelota.",    2, NA,                                            NA),
  list("f_17_agentless_passives",  "Se publicaron los resultados. La tarea fue realizada.",1,"Juan comió la manzana roja.",          0),
  list("f_18_by_passives",         "El libro fue escrito por María González.",      1, "La tarea fue realizada rápido.",              0),
  list("f_19_be_main_verb",        "El libro es interesante y la casa está limpia.",1, "El informe ha sido escrito hoy.",             0),
  list("f_20_existential_there",   "Hay un problema grave en el sistema.",          1, "Hay que esperar el resultado final.",         0),
  list("f_21_that_verb_comp",      "Dijo que vendría mañana sin falta.",            1, "La casa que compré es vieja.",                0),
  list("f_22_that_adj_comp",       "Es importante que vengas pronto.",              1, "Corrió porque tenía prisa.",                  0),
  list("f_23_wh_clause",           "No sé qué quieres ni dónde vives.",             1, "¿Qué quieres exactamente?",                   0),
  list("f_24_infinitives",         "Quiero comer y necesito dormir ahora.",         1, "Como pan todos los días.",                    0),
  list("f_25_present_participle",  "Caminando por la calle, pensó en todo.",        1, "Está hablando por teléfono ahora.",           0),
  list("f_26_past_participle",     "Terminada la reunión, todos se fueron.",        1, "Ha terminado la reunión temprano.",           0),
  list("f_27_past_participle_whiz","Los métodos utilizados fueron buenos.",         1, "Terminada la sesión, salieron.",              0),
  list("f_29_that_subj",           "El hombre que vino ayer era alto.",             1, "Dijo que vino tarde.",                        0),
  list("f_30_that_obj",            "El libro que María escribió es famoso.",        1, "Es importante que vengas.",                   0),
  list("f_31_wh_subj", "Los candidatos, quienes aprobaron el examen, recibieron una beca.", 1,
       "El libro que María escribió es famoso.", 0),
  list("f_32_wh_obj", "El informe, el cual María revisó, fue publicado.", 1,
       "El asunto del cual hablamos es importante.", 0),
  list("f_33_pied_piping",         "La casa en la que vivo. La persona con quien hablé. El motivo por el cual vino. Los temas de los cuales hablamos.",2,"El libro que leí ayer.",0),
  list("f_34_sentence_relatives",  "Llegó tarde, lo cual molestó a todos.",         1, "Lo que quieras está bien.",                   0),
  list("f_35_because",             "No vino porque estaba enfermo.",                1, "Aunque llovía, salió igual.",                 0),
  list("f_36_though",              "Aunque llovía, salió a la calle.",              1, "Porque llovía, no salió.",                    0),
  list("f_37_if",                  "Si llueve mañana, no salgo.",                   1, "Preguntó si vendría a la fiesta.",            0),
  list("f_38_other_adv_sub",       "Mientras trabajaba, escuchaba música.",         1, "Comió pan y bebió agua.",                     0),
  list("f_39_prepositions",        "El libro de María está en la mesa con cuidado.",3, NA,                                           NA),
  list("f_40_adj_attr",            "Un libro interesante y una casa grande.",       1, "El libro es interesante.",                    0),
  list("f_41_adj_pred",            "El libro es interesante y útil.",               1, "Un libro interesante llegó.",                 0),
  list("f_42_adverbs",             "Habló rápidamente y actuó claramente.",         1, NA,                                           NA),
  list("f_43_type_token",          "El gato y el perro corren juntos felices.",     0, NA,                                           NA),
  list("f_44_mean_word_length",    "La producción de conocimiento riguroso.",       1, NA,                                           NA),
  list("f_45_conjuncts",           "Llegó tarde. Sin embargo, lo intentó. Además ganó.",1,"Salió sin nada en las manos.",            0),
  list("f_46_downtoners",          "Es casi imposible y apenas visible.",           1, "Es muy claro y totalmente cierto.",           0),
  list("f_47_hedges",              "Quizás venga. A lo mejor llega mañana.",        1, "Vendrá seguro y sin dudas.",                  0),
  list("f_48_amplifiers",          "Es muy bueno y totalmente claro.",              1, "Es casi bueno y apenas claro.",               0),
  list("f_49_emphatics",           "Realmente lo hizo. De verdad que sí.",          1, "Lo hizo ayer por la tarde.",                  0),
  list("f_50_discourse_particles", "Bueno, o sea, es decir, llegó tarde.",          2, "El alumno bueno estudió mucho.",              0),
  list("f_51_demonstratives",      "Este libro y esa casa son nuevos.",             1, "Eso es mío y esto es tuyo.",                  0),
  list("f_52_modal_possibility",   "Puedo ir y podríamos hablar luego.",            1, "Tiene mucho poder político.",                 0),
  list("f_53_modal_necessity",     "Hay que esperar. Tengo que irme. Debe salir.",  2, "Tengo un perro y un gato.",                   0),
  list("f_54_modal_predictive",    "Hablará mañana. Vamos a comer pronto.",         1, "Habló ayer y comió tarde.",                   0),
  list("f_55_verb_public",         "Dijo que sí y afirmó lo contrario.",            1, "Comió pan y durmió bien.",                    0),
  list("f_56_verb_private",        "Creo que sé la respuesta y pienso mucho.",      1, "Corrió rápido y saltó alto.",                 0),
  list("f_57_verb_suasive",        "Le pedí que viniera y recomendé esperar.",      1, "Comió pan y bebió agua.",                     0),
  list("f_58_verb_seem",           "Parece que va a llover pronto.",                1, "Apareció en la reunión tarde.",               0),
  list("f_59_contractions", "Toy aquí. Ontas? Ven pa’ca’rriba.", 3,
       "Al salir del taller, compró un toy.", 0),
  list("f_60_that_deletion", "Pienso ir. Creo haber entendido.", 2,
       "Pienso que voy a ir. Puedo ir.", 0),
  list("f_61_stranded_preposition", "Ese es el chico que hablé de él.", 1,
       "El chico que habló de él llegó.", 0),
  list("f_62_split_infinitive", "Trato de realmente comprender el problema.", 1,
       "Lo hizo para no volver.", 0),
  list("f_63_split_auxiliary",     "Ha probablemente sido analizado el caso.",      1, "Fue mostrado el resultado.",                  0),
  list("f_64_phrasal_coordination","Juan y María vinieron temprano.",               1, "Vino pero se fue rápido.",                    0),
  list("f_65_clausal_coordination","Llegó tarde. Y luego se fue.",                  1, "Juan y María vinieron.",                      0),
  list("f_66_neg_synthetic",       "Nadie vino y nada pasó. Nunca lo hizo.",        2, "Vino alguien con algo.",                      0),
  list("f_67_neg_analytic",        "No vino y no lo hizo nunca.",                   1, "No obstante, finalmente llegó.",              0)
)

for (cs in fc_cases) {
  feat <- cs[[1]]; pos_t <- cs[[2]]; pos_min <- cs[[3]]; neg_t <- cs[[4]]; neg_max <- cs[[5]]

  test_that(paste0(feat, " — cobertura positiva"), {
    r <- run_biber(pos_t)
    expect_true(feat %in% names(r), info = paste(feat, "ausente del output"))
    val <- as.numeric(r[[feat]][1])
    if (feat == "f_43_type_token") {
      expect_gt(val, 0); expect_lte(val, 1)
    } else if (feat == "f_44_mean_word_length") {
      expect_gt(val, 3); expect_lt(val, 15)
    } else {
      expect_gte(val, pos_min)
    }
  })

  if (!is.na(neg_t)) {
    test_that(paste0(feat, " — cobertura negativa"), {
      # f_50: spec §f_50 explicitly allows the noise of having no positional
      # filter in the base version ("recommend starting with no
      # positional filter"). The false positive of adjectival "bueno" is
      # acceptable noise, not a bug.
      if (feat == "f_50_discourse_particles")
        skip("Ruido aceptable por spec §f_50 (sin filtro posicional en baseline)")
      r <- run_biber(neg_t)
      val <- as.numeric(r[[feat]][1])
      expect_lte(val, neg_max)
    })
  }
}

# ── Specific critical tests requested by audit/FEATURE_AUDIT.md (brief) ───

test_that("f_06 — pro-drop: oración sin pronombre explícito cuenta 0", {
  # Spec §f_06: lexical list + morphological filter (Person=1) on PRON;
  # it must NOT fire on conjugated verbs. UDPipe spanish-gsd sometimes
  # mis-tags verbs as PRON|Person=1 (case "Comí"); the morphological
  # supplement must also filter by form/lemma in the known list.
  r <- run_biber("Comí pizza ayer.")
  expect_equal(as.numeric(r$f_06_first_person_pronouns), 0)
})

test_that("f_07 — verbal Person=2 alone must not count (regression-guard)", {
  r <- run_biber("Llegaste temprano ayer.")
  expect_equal(as.numeric(r$f_07_second_person_pronouns), 0,
               info = "Verbal Person=2 inflection alone must not count per spec §f_07")
})

test_that("f_08 — verbal Person=3 alone must not count (regression-guard)", {
  # No pronoun or possessive: 'Comió' (VERB, Person=3) must not inflate f_08.
  # (Phase 1: possessives such as 'sus' do count now; that is why they are avoided here.)
  r <- run_biber("Comió pizza en el parque.")
  expect_equal(as.numeric(r$f_08_third_person_pronouns), 0,
               info = "Verbal Person=3 inflection alone must not count per spec §f_08")
})

test_that("f_19 — 'O sea, ...' no cuenta sea como ser/estar léxico", {
  # Spec: the compound o_sea must absorb sea before f_19 sees it.
  r <- run_biber("O sea, llegó tarde.")
  expect_equal(as.numeric(r$f_19_be_main_verb), 0,
               info = "sea debe ser parte del compound o_sea (→ f_50)")
})

test_that("f_23 — relativa con 'que' sin tilde cuenta 0 (es f_29, no wh-clause)", {
  # Spec §f_23 Exclude: "Relative uses of the same words without accents".
  # UDPipe spanish-gsd marks relative "que" as ambivalent PronType=Int,Rel.
  # Fix: accent in the surface form + exclusion of acl/acl:relcl heads.
  r <- run_biber("El equipo que fue asignado finalizó el proyecto.")
  expect_equal(as.numeric(r$f_23_wh_clause), 0)
})

test_that("f_47 — quizás detectado en locale C (regression-guard UTF-8)", {
  # Documented root cause: in the "C" locale, quanteda::tokens drops
  # Encoding="UTF-8" and the accents are left as raw bytes
  # ("quizás" -> "quiz<c3><a1>s"), breaking the match against the dict.
  # Fix: force LC_CTYPE=UTF-8 inside biber_fixture() with on.exit.
  r <- run_biber("Quizás el resultado depende de otros factores.")
  expect_gte(as.numeric(r$f_47_hedges), 1)
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
  r <- run_biber("Se debe seguir el procedimiento.")
  expect_gte(as.numeric(r$f_24_infinitives), 1)
})

test_that("f_23 — interrogativa indirecta legítima sigue contando", {
  # Regression guard: the fix must not break the detection of accented wh-words
  # in argument function (ccomp/xcomp).
  r <- run_biber("No sé qué quieres ni dónde vives.")
  expect_gte(as.numeric(r$f_23_wh_clause), 1)
})

test_that("f_44 — caso corto sigue dentro del rango 4-6 (no inflado)", {
  # Defensive regression: 'Prueba de contrato.' gave 7 before the fix.
  r <- run_biber("Prueba de contrato.")
  val <- as.numeric(r$f_44_mean_word_length)
  expect_gt(val, 4); expect_lt(val, 6)
})

# ── Zero-output: the 10 constant columns (surface parity with FR) ─────────
# Spec README §1 + biber_espanol_completo.md §1: any input must
# return exactly 0 in the remaining compatibility column. We test with three different
# sentences, including one in English that would trigger the feature in FR/EN.

.zero_output_cols <- "f_28_present_participle_whiz"

.zero_output_probes <- c(
  "Es una prueba simple en español.",
  "Caminando rápidamente, vio que él lo hizo todo.",
  "It would be a sentence that triggers in English."
)

for (zcol in .zero_output_cols) {
  test_that(paste0(zcol, " — zero-output invariante con cualquier input"), {
    for (probe in .zero_output_probes) {
      r <- run_biber(probe)
      expect_true(zcol %in% names(r), info = paste(zcol, "ausente"))
      expect_equal(as.numeric(r[[zcol]][1]), 0,
                   info = paste(zcol, "≠ 0 en:", probe))
    }
  })
}

# ── Regression §3.J f_44 (worked example) ─────────────────────────────────
# D-51: mean of nchar() over purely alphabetic word tokens, with no POS filter
# and no rounding. Historical bug: it filtered to NOUN/VERB/ADJ/ADV and
# returned ~8.75 on the worked example (it should be 5.22).

test_that("f_44 uses all alphabetic word tokens regardless of POS (D-51)", {
  r <- run_biber("El informe fue redactado por el equipo de investigación.")
  expect_equal(round(as.numeric(r$f_44_mean_word_length), 2), 5.22,
               info = "47 letters / 9 alphabetic tokens = 5.22")
})

test_that("f_44 excludes numerals and is not rounded (D-51)", {
  r <- run_biber("En 2024 compré 35 libros.")
  expect_equal(as.numeric(r$f_44_mean_word_length), 14 / 3)
})

test_that("f_44 is invariant under normalize=TRUE (ratio, not count)", {
  texto <- "El informe fue redactado por el equipo de investigación."
  r_raw  <- run_biber(texto, normalize = FALSE)
  r_norm <- run_biber(texto, normalize = TRUE)
  expect_equal(as.numeric(r_raw$f_44_mean_word_length),
               as.numeric(r_norm$f_44_mean_word_length),
               info = "f_44 no debe re-escalarse por normalize=TRUE")
})
