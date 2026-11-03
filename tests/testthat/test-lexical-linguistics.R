test_that("nominalizations and other nouns partition UD noun words including hyphens", {
  tokens <- contract_clause(
    "nouns",
    c("producciones", "ciudades", "riqueza", "Concepcion", "coche-cama", "productiva"),
    c("producci\u00f3n", "ciudad", "riqueza", "concepci\u00f3n",
      "coche-cama", "productivo"),
    c("NOUN", "NOUN", "NOUN", "PROPN", "NOUN", "ADJ"),
    rep(0, 6), rep("root", 6)
  )
  raw <- biber(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_14_nominalizations, 2)
  expect_equal(raw$f_16_other_nouns, 3)
  expect_equal(raw$f_14_nominalizations + raw$f_16_other_nouns, 5)
  expect_setequal(audit_features(tokens, feature = "f_14_nominalizations")$keyword,
                  c("producciones", "riqueza"))
  expect_setequal(audit_features(tokens, feature = "f_16_other_nouns")$keyword,
                  c("ciudades", "Concepcion", "coche-cama"))
})

test_that("attributive adjectives retain hyphens and inherit coordination roles", {
  tokens <- contract_clause(
    "attributive",
    c("analisis", "teorico-practico", "y", "riguroso", "y", "util"),
    c("analisis", "teorico-practico", "y", "riguroso", "y", "util"),
    c("NOUN", "ADJ", "CCONJ", "ADJ", "CCONJ", "ADJ"),
    c(0, 1, 4, 2, 6, 4), c("root", "amod", "cc", "conj", "cc", "conj")
  )
  raw <- biber(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_40_adj_attr, 3)
  expect_equal(raw$f_41_adj_pred, 0)
  expect_setequal(audit_features(tokens, feature = "f_40_adj_attr")$keyword,
                  c("teorico-practico", "riguroso", "util"))
})

test_that("predicative adjectives and their conjuncts are not fallback attributives", {
  inverted <- contract_clause(
    "inverted", c("Es", "famoso", "Juan"), c("ser", "famoso", "Juan"),
    c("AUX", "ADJ", "PROPN"), c(2, 0, 2), c("cop", "root", "nsubj")
  )
  coordination <- contract_clause(
    "coordination", c("es", "interesante", "y", "util", "y", "claro"),
    c("ser", "interesante", "y", "util", "y", "claro"),
    c("AUX", "ADJ", "CCONJ", "ADJ", "CCONJ", "ADJ"),
    c(2, 0, 4, 2, 6, 4), c("cop", "root", "cc", "conj", "cc", "conj")
  )
  subject <- contract_clause(
    "subject", c("importante", "es", "urgente"), c("importante", "ser", "urgente"),
    c("ADJ", "AUX", "ADJ"), c(2, 0, 2), c("nsubj", "root", "attr")
  )
  tokens <- rbind(inverted, coordination, subject)
  raw <- biber(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_40_adj_attr, c(0, 0, 0))
  expect_equal(raw$f_41_adj_pred, c(1, 3, 1))
  expect_setequal(
    audit_features(tokens, feature = "f_41_adj_pred")$keyword,
    c("famoso", "interesante", "util", "claro", "urgente")
  )
  expect_equal(biber(tokens, measure = "none")$f_41_adj_pred,
               1000 * c(1, 3, 1) / raw$n_tokens)
  spacy <- dplyr::rename(tokens, pos = "upos", tag = "xpos", morph = "feats")
  class(spacy) <- c("spacyr_parsed", "data.frame")
  expect_equal(biber(spacy, measure = "none", normalize = FALSE), raw)
})

test_that("adjective coordination stays within sentences and documents", {
  tokens <- contract_clause(
    "one", c("casa", "grande", "util"), c("casa", "grande", "util"),
    c("NOUN", "ADJ", "ADJ"), c(0, 1, 2), c("root", "amod", "conj")
  )
  tokens$sentence_id[3] <- 2L
  other <- tokens[3, ]
  other$doc_id <- "other"
  raw <- biber(rbind(tokens, other), measure = "none", normalize = FALSE)
  expect_equal(raw$f_40_adj_attr, c(1, 0))
  expect_equal(raw$f_41_adj_pred, c(0, 0))
})

test_that("the preposition inventory drives complete single-occurrence phrase audits", {
  phrases <- c("a excepci\u00f3n de", "a causa de", "por medio de", "de acuerdo con",
               "en funci\u00f3n de")
  clauses <- lapply(seq_along(phrases), function(i) {
    words <- strsplit(phrases[i], " ", fixed = TRUE)[[1]]
    contract_clause(
      paste0("prep_", i), c(words, "Ana"), c(words, "Ana"),
      c("ADP", "NOUN", "ADP", "PROPN"), c(2, 4, 4, 0),
      c("case", "nmod", "case", "root")
    )
  })
  tokens <- do.call(rbind, clauses)
  raw <- biber(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_39_prepositions, rep(1, length(phrases)))
  audit <- audit_features(tokens, feature = "f_39_prepositions", window = 1)
  expect_setequal(audit$keyword, phrases)
  expect_equal(audit$left, rep("", length(phrases)))
  expect_equal(audit$right, rep("Ana", length(phrases)))
})

test_that("adverbial phrases reconcile lemma evidence and preserve total adverbs", {
  place <- contract_clause(
    "place", c("alrededor", "de", "aqui"), c("alrededor", "de", "aqu\u00ed"),
    c("ADV", "ADP", "ADV"), c(0, 3, 1), c("root", "case", "obl")
  )
  time <- contract_clause(
    "time", c("de", "vez", "en", "cuando", "hoy"),
    c("de", "vez", "en", "cuando", "hoy"), c("ADP", "NOUN", "ADP", "ADV", "ADV"),
    rep(0, 5), rep("dep", 5)
  )
  tokens <- rbind(place, time)
  raw <- biber(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_04_place_adverbials, c(2, 0))
  expect_equal(raw$f_05_time_adverbials, c(0, 2))
  expect_equal(raw$f_42_adverbs, c(2, 2))
  expect_setequal(audit_features(tokens, feature = "f_04_place_adverbials")$keyword,
                  c("alrededor de", "aqui"))
  expect_setequal(audit_features(tokens, feature = "f_05_time_adverbials")$keyword,
                  c("de vez en cuando", "hoy"))
})

test_that("dictionary phrases are contained-deduplicated and sentence-local", {
  degree <- contract_clause(
    "degree", c("Un", "poco"), c("uno", "poco"), c("DET", "ADV"),
    c(2, 0), c("det", "root")
  )
  connector <- contract_clause(
    "connector", c("Sin", "embargo"), c("sin", "embargo"), c("ADP", "NOUN"),
    c(2, 0), c("case", "root")
  )
  split <- connector
  split$doc_id <- "split"
  split$sentence_id <- c(1L, 2L)
  tokens <- rbind(degree, connector, split)
  raw <- biber(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_46_downtoners, c(1, 0, 0))
  expect_equal(raw$f_45_conjuncts, c(0, 1, 0))
  expect_equal(audit_features(tokens, feature = "f_46_downtoners")$keyword, "Un poco")
  expect_equal(audit_features(tokens, feature = "f_45_conjuncts")$keyword, "Sin embargo")
})

test_that("every lexical dictionary entry has an exact full-span occurrence", {
  for (feature in c(.dual_path_features, .dict_only_features)) {
    patterns <- dict[[feature]]
    clauses <- lapply(seq_along(patterns), function(i) {
      words <- strsplit(patterns[i], "_", fixed = TRUE)[[1]]
      contract_clause(paste0("entry_", i), words, words, rep("ADV", length(words)),
                       rep(0, length(words)), rep("dep", length(words)))
    })
    tokens <- prepare_parsed_tokens(do.call(rbind, clauses))
    result <- dictionary_evidence(tokens, feature)
    expect_equal(nrow(result$evidence), length(patterns), info = feature)
    expect_equal(tabulate(match(result$evidence$doc_id, unique(tokens$doc_id)),
                           nbins = length(patterns)), rep(1L, length(patterns)), info = feature)
    expect_setequal(result$evidence$token, gsub("_", " ", patterns, fixed = TRUE))
  }
})

test_that("packaged preposition and nominalization inventories match their sources", {
  skip_if_not_installed("yaml")
  path <- test_path("..", "..", "data-raw", "word_lists.yaml")
  skip_if_not(file.exists(path), "Development word-list source is not installed")
  source <- yaml::read_yaml(path)
  for (name in c("prepositions_compound", "nominalization_suffixes",
                "nominalization_stoplist")) {
    expected <- sort(unique(stringi::stri_trans_tolower(trimws(as.character(source[[name]])))))
    expect_equal(sort(word_lists[[name]]), expected, info = name)
  }
})

test_that("Spanish-GSD preserves coordinated adjective and prepositional contrasts", {
  model <- .fc_ud()
  texts <- c(
    attributive = "Una casa grande y luminosa.",
    predicative = "El libro es interesante y \u00fatil.",
    preposition = "A excepci\u00f3n de Ana, todos vinieron.",
    noun = "El coche-cama es c\u00f3modo."
  )
  tokens <- udpipe::udpipe_annotate(model, x = texts, doc_id = names(texts))
  raw <- biber(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$doc_id, names(texts))
  expect_equal(raw$f_40_adj_attr, c(2, 0, 0, 0))
  expect_equal(raw$f_41_adj_pred, c(0, 2, 0, 1))
  expect_equal(raw$f_39_prepositions[3], 1)
  expect_equal(raw$f_16_other_nouns[4], 1)
  audit <- audit_features(tokens, feature = "f_39_prepositions")
  expect_equal(audit$keyword[audit$doc_id == "preposition"], "A excepci\u00f3n de")
})
