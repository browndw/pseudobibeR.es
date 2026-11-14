# Regression cases from Hernan's review, phases 1-5
# (INSTRUCCIONES_CLAUDE_CODE_pseudobiber_es.md). Expected counts are verified
# empirically against spanish-gsd; the UD tags that support them are asserted
# in test-udpipe-tag-verification.R. Sentences avoid putting the target form in
# initial position (spanish-gsd mis-tags capitalized sentence-initial words).
# All sentences are parsed and traced in a single batch.
#
# nolint start: line_length_linter, object_name_linter

review <- ud_batch(c(
  # Phase 1
  "Ayer corrí hasta la esquina.", "Cuando era joven caminaba mucho.",
  "Yo quería que corriera más rápido.", "Ya había corrido dos kilómetros.",
  "Ella corre todos los días.", "Nuestro equipo revisó mi propuesta.",
  "Usted debe entregar su informe.", "Los resultados míos superan los tuyos.",
  "Vos tenés razón siempre.", "Ella se vio en el espejo.", "Se publicaron los informes.",
  "No corre rápidamente hoy.",
  # Phase 2
  "El libro que está en la mesa es mío.", "El libro que María escribió es famoso.",
  "La autora, quien presentó el proyecto, respondió.",
  "El informe, el cual revisamos ayer, ya se publicó.",
  "El método por el cual se analizaron los datos falló.",
  "La persona con quien Juan habló se fue.",
  "No entregó el informe, lo que retrasó el proceso.", "No entiendo lo que dices.",
  "La novela que leí era larga.",
  # Phase 3
  "Fumar perjudica la salud.", "Es importante descansar.", "Me gusta nadar en el mar.",
  "El fumar constante daña la salud.", "Quiere estudiar medicina.",
  "Prohibieron fumar en el bar.",
  # Phase 4
  "Elige cualquiera de los libros.", "No vino nadie a la fiesta.", "Algo falló en el motor.",
  "Alguien llamó pero nadie respondió nada.", "Lo quiero todo ahora.",
  "Es un libro cualquiera.", "Todo el día estuvo lloviendo.", "Casi termino el informe.",
  "Apenas se nota el cambio.", "Lo dejó en mitad de la sala.",
  "El pueblo está más allá del río.", "Hace poco llegaron los datos.",
  # Phase 5
  "Se venden casas baratas.", "Se entrevistó a los candidatos.",
  "Se recomienda leer el manual.", "La tarea fue realizada.", "Puede que llueva mañana.",
  "Puede hacerlo fácilmente.", "Lee y escribe muy bien.", "Juan y María vinieron temprano.",
  "Ella canta y él baila.", "Una respuesta no definitiva.", "No muy lejos de aquí.",
  "No corre rápido hoy.", "El no del comité fue claro.",
  "No saldré a menos que pare la lluvia.", "Salvo que llueva, saldremos.",
  "Si llueve no salgo.", "Cuando llegó, todos aplaudieron.",
  "La propuesta resulta adecuada.", "Parece difícil el examen.",
  "Resultó ganador del premio.", "El accidente resultó de una falla.",
  "Ha siempre sostenido su postura.", "Puede fácilmente hacerlo.",
  "Debe siempre revisar los datos.", "Podía yo saberlo.", "Llegó a causa de la lluvia.",
  "Lo resolvió por medio de un truco.", "Fue del pueblo al río."
))
n <- review$count

# ── Phase 1: f_01, f_06-f_08 and f_42 coverage ───────────────────────────────

test_that("f_01 cuenta pasados de indicativo y subjuntivo (perfecto simple + imperfecto)", {
  expect_equal(n("Ayer corrí hasta la esquina.", "f_01_past_tense"), 1)          # simple perfect
  expect_equal(n("Cuando era joven caminaba mucho.", "f_01_past_tense"), 2)       # imperfect ind. (era, caminaba)
  expect_equal(n("Yo quería que corriera más rápido.", "f_01_past_tense"), 2)     # + imperfect subj. (corriera)
  expect_equal(n("Ya había corrido dos kilómetros.", "f_01_past_tense"), 1)       # aux 'había' of the compound tense
  expect_equal(n("Ella corre todos los días.", "f_01_past_tense"), 0)             # present: does not count
})

test_that("f_06/f_07/f_08 incluyen posesivos ruteados por persona", {
  r1 <- "Nuestro equipo revisó mi propuesta."
  expect_equal(n(r1, "f_06_first_person_pronouns"), 2)   # nuestro + mi (Poss=Yes, Person=1)
  expect_equal(n(r1, "f_07_second_person_pronouns"), 0)
  expect_equal(n(r1, "f_08_third_person_pronouns"), 0)

  r2 <- "Usted debe entregar su informe."
  expect_equal(n(r2, "f_07_second_person_pronouns"), 1)  # usted
  expect_equal(n(r2, "f_08_third_person_pronouns"), 1)   # su -> Person=3 (morphological criterion)

  # Nominalized stressed possessives (NOUN, without Poss=Yes) captured by lemma.
  r3 <- "Los resultados míos superan los tuyos."
  expect_equal(n(r3, "f_06_first_person_pronouns"), 1)   # míos
  expect_equal(n(r3, "f_07_second_person_pronouns"), 1)  # tuyos

  # Voseo: 'vos' is mis-tagged as NOUN; it is recovered by token.
  expect_equal(n("Vos tenés razón siempre.", "f_07_second_person_pronouns"), 1)
})

test_that("LIMITACIÓN documentada: 'se' argumental no es separable de la pasiva refleja", {
  # 'Ella se vio' -> only 'ella' counts (=1). The argumental reflexive 'se' is NOT
  # counted because it is morphologically identical to the reflexive passive / impersonal
  # (see test-udpipe-tag-verification.R). The spec asked for 2; the deviation is
  # documented so as not to inflate f_08 in academic corpora full of impersonal 'se'.
  expect_equal(n("Ella se vio en el espejo.", "f_08_third_person_pronouns"), 1)
  # Reflexive passive: 'se' correctly NOT counted.
  expect_equal(n("Se publicaron los informes.", "f_08_third_person_pronouns"), 0)
  expect_equal(n("Se publicaron los informes.", "f_06_first_person_pronouns"), 0)
})

test_that("f_42 excluye tiempo pero conserva adverbios negativos y de modo", {
  expect_equal(n("No corre rápidamente hoy.", "f_42_adverbs"), 2)
})

# ── Phase 2: relatives follow the approved family/role partition ────────────
# Roleless pro-drop gaps are not automatically classified as subjects.

test_that("f_29: relativa de 'que' en función de sujeto", {
  expect_equal(n("El libro que está en la mesa es mío.", "f_29_that_subj"), 1)
})

test_that("f_30: relativa de 'que' objeto (requiere sujeto explícito en la relativa)", {
  expect_equal(n("El libro que María escribió es famoso.", "f_30_that_obj"), 1)
})

test_that("f_31/f_32: quien/el cual occupy their own subject/object bins", {
  r1 <- "La autora, quien presentó el proyecto, respondió."
  expect_equal(n(r1, "f_31_wh_subj"), 1)
  expect_equal(n(r1, "f_29_that_subj"), 0)

  r2 <- "El informe, el cual revisamos ayer, ya se publicó."
  expect_equal(n(r2, "f_31_wh_subj"), 0)
  expect_equal(n(r2, "f_32_wh_obj"), 1)
  expect_equal(n(r2, "f_29_that_subj"), 0)
})

test_that("f_33: pied-piping (preposición + relativo) — incluye 'a/con/por quien|cual'", {
  expect_equal(n("El método por el cual se analizaron los datos falló.", "f_33_pied_piping"), 1)
  expect_equal(n("La persona con quien Juan habló se fue.", "f_33_pied_piping"), 1)
})

test_that("f_34: relativa oracional (lo que / lo cual con antecedente proposicional)", {
  expect_equal(n("No entregó el informe, lo que retrasó el proceso.", "f_34_sentence_relatives"), 1)
  # Free relative without a propositional antecedent: NOT f_34.
  expect_equal(n("No entiendo lo que dices.", "f_34_sentence_relatives"), 0)
})

test_that("LÍMITE pro-drop: a roleless misparse is not guessed to be a subject", {
  # GSD tags lei as ADV and attaches roleless que to the predicate adjective.
  expect_equal(n("La novela que leí era larga.", "f_30_that_obj"), 0)
  expect_equal(n("La novela que leí era larga.", "f_29_that_subj"), 0)
})

# ── Phase 3: D-23 nominal infinitives, without moving infinitival clauses ────

test_that("f_15 distinguishes nominal infinitives from ordinary infinitival clauses", {
  # A preverbal subject infinitive is nominal (Smoking harms health).
  expect_equal(n("Fumar perjudica la salud.", "f_15_gerunds"), 1)
  expect_equal(n("Es importante descansar.", "f_15_gerunds"), 0)
  expect_equal(n("Me gusta nadar en el mar.", "f_15_gerunds"), 0)
  expect_equal(n("El fumar constante daña la salud.", "f_15_gerunds"), 1)
})

test_that("f_24 retains verbal infinitives, including clausal subjects", {
  # 'nadar' (formerly csubj -> f_15) now counts in f_24, as in the original design.
  expect_equal(n("Me gusta nadar en el mar.", "f_15_gerunds"), 0)
  expect_equal(n("Me gusta nadar en el mar.", "f_24_infinitives"), 1)
  # The cases that already counted in f_24 (xcomp) do not change:
  expect_equal(n("Quiere estudiar medicina.", "f_24_infinitives"), 1)
  expect_equal(n("Prohibieron fumar en el bar.", "f_24_infinitives"), 1)
})

# ── Phase 4: dictionaries and syntactic control ─────────────────────────────
# f_11 counts only the independent pronominal function (code-only, so the
# surface branch does not override the filter).

test_that("f_11 cuenta indefinidos en función pronominal independiente", {
  expect_equal(n("Elige cualquiera de los libros.", "f_11_indefinite_pronouns"), 1)
  expect_equal(n("No vino nadie a la fiesta.", "f_11_indefinite_pronouns"), 1)
  expect_equal(n("Algo falló en el motor.", "f_11_indefinite_pronouns"), 1)
  expect_equal(n("Alguien llamó pero nadie respondió nada.", "f_11_indefinite_pronouns"), 3)
  expect_equal(n("Lo quiero todo ahora.", "f_11_indefinite_pronouns"), 1)   # pronominal 'todo' (obj)
})

test_that("f_11 excluye el uso NO pronominal (adjetival y determinante)", {
  expect_equal(n("Es un libro cualquiera.", "f_11_indefinite_pronouns"), 0)      # ADJ/amod
  expect_equal(n("Todo el día estuvo lloviendo.", "f_11_indefinite_pronouns"), 0) # DET/det ('todo el día')
})

test_that("f_46/f_47: 'casi' cuenta en f_47 (hedges), no en f_46 (downtoners)", {
  expect_equal(n("Casi termino el informe.", "f_47_hedges"), 1)
  expect_equal(n("Casi termino el informe.", "f_46_downtoners"), 0)
  # 'apenas' stays in f_46:
  expect_equal(n("Apenas se nota el cambio.", "f_46_downtoners"), 1)
})

test_that("f_04/f_05: locuciones añadidas al diccionario se cuentan", {
  expect_equal(n("Lo dejó en mitad de la sala.", "f_04_place_adverbials"), 1)
  expect_equal(n("El pueblo está más allá del río.", "f_04_place_adverbials"), 1)
  expect_equal(n("Hace poco llegaron los datos.", "f_05_time_adverbials"), 1)
})

# ── Phase 5: rule refinements ───────────────────────────────────────────────
# Rules blocked by the model (f_18 obl:agent) are NOT forced; they are
# documented in test-udpipe-tag-verification.R.

test_that("f_17: la pasiva refleja cuenta; la impersonal con se se excluye", {
  expect_equal(n("Se publicaron los informes.", "f_17_agentless_passives"), 1)   # reflexive passive (nsubj)
  expect_equal(n("Se venden casas baratas.", "f_17_agentless_passives"), 1)       # reflexive passive
  expect_equal(n("Se entrevistó a los candidatos.", "f_17_agentless_passives"), 0) # impersonal (obj with 'a')
  expect_equal(n("Se recomienda leer el manual.", "f_17_agentless_passives"), 0)   # impersonal (xcomp)
  # The periphrastic passive is not affected:
  expect_equal(n("La tarea fue realizada.", "f_17_agentless_passives"), 1)
})

test_that("f_52: 'puede que' cuenta como posibilidad (correlato de may/might)", {
  expect_equal(n("Puede que llueva mañana.", "f_52_modal_possibility"), 1)
  # poder + infinitive still counts:
  expect_equal(n("Puede hacerlo fácilmente.", "f_52_modal_possibility"), 1)
})

test_that("f_64: cubre la coordinación de verbos (SV compartido), sin doblar f_65", {
  expect_equal(n("Lee y escribe muy bien.", "f_64_phrasal_coordination"), 1)   # verbs, shared subject
  expect_equal(n("Lee y escribe muy bien.", "f_65_clausal_coordination"), 0)
  expect_equal(n("Juan y María vinieron temprano.", "f_64_phrasal_coordination"), 1) # nouns
  # Clausal coordination (own subjects) -> f_65, not f_64:
  expect_equal(n("Ella canta y él baila.", "f_65_clausal_coordination"), 1)
  expect_equal(n("Ella canta y él baila.", "f_64_phrasal_coordination"), 0)
})

test_that("f_67: 'no' de foco sobre constituyentes no verbales cuenta; excluye sustantivo", {
  expect_equal(n("Una respuesta no definitiva.", "f_67_neg_analytic"), 1)  # focus on an ADJ
  expect_equal(n("No muy lejos de aquí.", "f_67_neg_analytic"), 1)         # focus on an ADV
  expect_equal(n("No corre rápido hoy.", "f_67_neg_analytic"), 1)          # preverbal (control)
  expect_equal(n("El no del comité fue claro.", "f_67_neg_analytic"), 0)   # 'no' as a noun
})

test_that("f_37: 'a menos que' y 'salvo que' cuentan como condicional (no en f_38)", {
  expect_equal(n("No saldré a menos que pare la lluvia.", "f_37_if"), 1)
  expect_equal(n("No saldré a menos que pare la lluvia.", "f_38_other_adv_sub"), 0)
  expect_equal(n("Salvo que llueva, saldremos.", "f_37_if"), 1)
  expect_equal(n("Salvo que llueva, saldremos.", "f_38_other_adv_sub"), 0)
  # conditional 'si' still counts; 'cuando' stays in f_38:
  expect_equal(n("Si llueve no salgo.", "f_37_if"), 1)
  expect_equal(n("Cuando llegó, todos aplaudieron.", "f_38_other_adv_sub"), 1)
})

test_that("f_58: 'resultar' solo en uso copulativo de apariencia; 'parecer' siempre", {
  expect_equal(n("La propuesta resulta adecuada.", "f_58_verb_seem"), 1)     # cop
  expect_equal(n("Parece difícil el examen.", "f_58_verb_seem"), 1)          # parecer
  expect_equal(n("Resultó ganador del premio.", "f_58_verb_seem"), 0)        # change of state
  expect_equal(n("El accidente resultó de una falla.", "f_58_verb_seem"), 0) # consequence
})

test_that("f_63: adverbio interpuesto (auxiliar y perífrasis modal); excluye sujeto", {
  expect_equal(n("Ha siempre sostenido su postura.", "f_63_split_auxiliary"), 1)  # perfect AUX
  expect_equal(n("Puede fácilmente hacerlo.", "f_63_split_auxiliary"), 1)       # modal periphrasis
  expect_equal(n("Debe siempre revisar los datos.", "f_63_split_auxiliary"), 1)   # modal periphrasis
  expect_equal(n("Podía yo saberlo.", "f_63_split_auxiliary"), 0)                 # intervening subject
})

test_that("f_39: una locución preposicional cuenta como UNA preposición", {
  # Unaccented phrases (locale-robust): 'a causa de' and 'por medio de'
  # have 2 ADPs but count 1. (Accented ones such as 'en relación con'
  # need a UTF-8 locale; see deaccent in features_modals_verbs.R.)
  expect_equal(n("Llegó a causa de la lluvia.", "f_39_prepositions"), 1)
  expect_equal(n("Lo resolvió por medio de un truco.", "f_39_prepositions"), 1)
  # Single prepositions (incl. the contractions al/del) count once each:
  expect_equal(n("Fue del pueblo al río.", "f_39_prepositions"), 2)
})

# nolint end
