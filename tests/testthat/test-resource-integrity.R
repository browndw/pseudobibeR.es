# Packaged resource integrity: YAML parsing and accent variants.

test_that("packaged inventories contain no YAML-coerced booleans or deparsed lists", {
  values <- c(unlist(word_lists, use.names = FALSE),
              unlist(as.list(dict), use.names = FALSE))
  expect_false(any(values %in% c("true", "false", "TRUE", "FALSE")))
  expect_false(any(grepl("^(c|list)\\(", values)))
  expect_true(all(vapply(word_lists, is.character, logical(1))))
  expect_true("no" %in% word_lists$negation_particles)
  expect_true("no" %in% word_lists$neg_analytic_adverbs)
})

test_that("the f_11 dictionary key matches its output column", {
  expect_true("f_11_indefinite_pronouns" %in% names(dict))
  expect_false("f_11_indefinite_pronoun" %in% names(dict))
})

test_that("accent twins are generated except for contrastive grammatical words", {
  contrastive <- word_lists$accent_contrastive_forms
  expect_true(all(c("el", "si", "mas", "que", "tu") %in% contrastive))
  expect_true(all(c("aquí", "aqui") %in% dict[["f_04_place_adverbials"]]))
  expect_true(all(c("quizás", "quizas") %in% dict[["f_47_hedges"]]))
  expect_true(all(c("con_relación_a", "con_relacion_a") %in% word_lists$prepositions_compound))
  # The tilde of ñ is a letter, not an accent: no ñ -> n twins.
  expect_true("mañana" %in% dict[["f_05_time_adverbials"]])
  expect_false("manana" %in% dict[["f_05_time_adverbials"]])
  # Inflected forms where the accent is morphological stay exact.
  expect_false(any(grepl("^(hable|confie)_", word_lists$resumptive_first_person_forms)))
})

test_that("unaccented input matches lexical inventories but not contrastive words", {
  tokens <- contract_clause(
    "plain", c("quizas", "vino", "aqui"), c("quizas", "venir", "aqui"),
    c("ADV", "VERB", "ADV"), c(2, 0, 2), c("advmod", "root", "advmod"),
    c(NA, "VerbForm=Fin|Person=3|Tense=Past|Mood=Ind", NA)
  )
  raw <- biber_fixture(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_47_hedges, 1)
  expect_equal(raw$f_04_place_adverbials, 1)
})

test_that("dictionary phrases with al/del match UDPipe's split contractions (D-55)", {
  parsed <- as.data.frame(udpipe::udpipe_annotate(
    .fc_ud(),
    x = c("Vive al lado de la iglesia.", "Viven al norte.", "Lo hizo del mismo modo."),
    doc_id = c("lado", "norte", "modo")
  ))
  raw <- biber_fixture(parsed, measure = "none", normalize = FALSE)
  expect_equal(raw$f_04_place_adverbials, c(1, 1, 0))
  expect_equal(raw$f_45_conjuncts, c(0, 0, 1))
})

test_that("f_50 counts discourse particles only clause-initially (D-56)", {
  parsed <- as.data.frame(udpipe::udpipe_annotate(
    .fc_ud(),
    x = c("Pues no lo s\u00e9.", "Lo hizo pues ten\u00eda tiempo.", "Est\u00e1 claro el problema.",
          "Claro que s\u00ed."),
    doc_id = c("pues_initial", "pues_medial", "claro_adj", "claro_initial")
  ))
  raw <- biber_fixture(parsed, measure = "none", normalize = FALSE)
  expect_equal(raw$f_50_discourse_particles, c(1, 0, 0, 1))
})
