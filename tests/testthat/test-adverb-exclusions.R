test_that("f_42 excludes exactly the five specified specialized adverb groups", {
  tokens <- contract_clause(
    "specialized",
    c("aqu\u00ed", "hoy", "ligeramente", "aproximadamente", "muy", "r\u00e1pidamente"),
    c("aqu\u00ed", "hoy", "ligeramente", "aproximadamente", "muy", "r\u00e1pidamente"),
    rep("ADV", 6), rep(0, 6), rep("root", 6)
  )
  excluded <- c("f_04_place_adverbials", "f_05_time_adverbials", "f_46_downtoners",
                "f_47_hedges", "f_48_amplifiers")
  spacy <- dplyr::rename(tokens, pos = "upos", tag = "xpos", morph = "feats")
  class(spacy) <- c("spacyr_parsed", "data.frame")
  for (input in list(tokens, spacy)) {
    raw <- biber_fixture(input, measure = "none", normalize = FALSE)
    expect_equal(unlist(raw[1, excluded], use.names = FALSE), rep(1, 5))
    expect_equal(raw$f_42_adverbs, 1)
    expect_equal(word_tokens(input), 6)
    expect_equal(biber_fixture(input, measure = "none")$f_42_adverbs, 1000 / 6)
    audit <- audit_fixture(input, feature = "f_42_adverbs")
    expect_equal(audit$keyword, "r\u00e1pidamente")
    expect_equal(sum(audit$count), raw$f_42_adverbs)
  }
})

test_that("phrase exclusions cover adverbs beyond non-adverb anchors and token gaps", {
  tokens <- contract_clause(
    "phrase", c("Un", "poco", "r\u00e1pidamente"), c("uno", "poco", "r\u00e1pidamente"),
    c("DET", "ADV", "ADV"), c(3, 0, 0), c("det", "root", "root")
  )
  tokens$token_id <- c("1", "3", "7")
  raw <- biber_fixture(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_46_downtoners, 1)
  expect_equal(raw$f_42_adverbs, 1)
  expect_equal(audit_fixture(tokens, feature = "f_46_downtoners")$keyword, "Un poco")
  audit <- audit_fixture(tokens, feature = "f_42_adverbs")
  expect_equal(audit$keyword, "r\u00e1pidamente")
  expect_equal(sum(audit$count), raw$f_42_adverbs)
})

test_that("multi-adverb and overlapping phrases use covered occurrences, not subtraction", {
  multiple <- contract_clause(
    "multiple", c("M\u00e1s", "o", "menos"), c("m\u00e1s", "o", "menos"),
    c("ADV", "CCONJ", "ADV"), rep(0, 3), rep("root", 3)
  )
  no_adverbs <- contract_clause(
    "no_adverbs", c("En", "cierta", "medida", "r\u00e1pidamente"),
    c("en", "cierto", "medida", "r\u00e1pidamente"),
    c("ADP", "ADJ", "NOUN", "ADV"), rep(0, 4), rep("root", 4)
  )
  raw <- biber_fixture(rbind(multiple, no_adverbs), measure = "none", normalize = FALSE)
  expect_equal(raw$f_46_downtoners, c(1, 1))
  expect_equal(raw$f_47_hedges, c(1, 1))
  expect_equal(raw$f_42_adverbs, c(0, 1))
  audit <- audit_fixture(rbind(multiple, no_adverbs), feature = "f_42_adverbs")
  expect_equal(audit$doc_id, "no_adverbs")
  expect_equal(audit$keyword, "r\u00e1pidamente")
  expect_equal(biber_fixture(multiple, measure = "none")$f_42_adverbs, 0)
  expect_equal(nrow(audit_fixture(multiple, feature = "f_42_adverbs")), 0L)
})

test_that("lemma-only adverbial detections exclude their actual token keys", {
  tokens <- contract_clause(
    "lemma", c("variante_lugar", "variante_tiempo", "r\u00e1pidamente"),
    c("aqu\u00ed", "hoy", "r\u00e1pidamente"), rep("ADV", 3),
    rep(0, 3), rep("root", 3)
  )
  raw <- biber_fixture(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_04_place_adverbials, 1)
  expect_equal(raw$f_05_time_adverbials, 1)
  expect_equal(raw$f_42_adverbs, 1)
  expect_equal(audit_fixture(tokens, feature = "f_04_place_adverbials")$keyword,
               "variante_lugar")
  expect_equal(audit_fixture(tokens, feature = "f_42_adverbs")$keyword,
               "r\u00e1pidamente")
})

test_that("exclusions stay within document and sentence boundaries", {
  phrase <- contract_clause(
    "a", c("M\u00e1s", "o", "menos"), c("m\u00e1s", "o", "menos"),
    c("ADV", "CCONJ", "ADV"), rep(0, 3), rep("root", 3)
  )
  separate <- phrase
  separate$sentence_id <- 2L
  separate$token[2] <- separate$lemma[2] <- "trabaja"
  separate$upos[2] <- separate$xpos[2] <- "VERB"
  other <- separate
  other$doc_id <- "b"
  other$sentence_id <- 1L
  tokens <- rbind(phrase, separate, other)
  raw <- biber_fixture(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$doc_id, c("a", "b"))
  expect_equal(raw$f_42_adverbs, c(2, 2))
  expect_equal(raw$f_46_downtoners, c(1, 0))
  expect_equal(raw$f_47_hedges, c(1, 0))
  audit <- audit_fixture(tokens, feature = "f_42_adverbs")
  expect_equal(audit$sentence_id[audit$doc_id == "a"], c(2L, 2L))
  expect_equal(audit$sentence_id[audit$doc_id == "b"], c(1L, 1L))
})

test_that("emphatic, conjunct, discourse, and negative adverbs are not extra exclusions", {
  tokens <- contract_clause(
    "retained",
    c("claramente", "entonces", "pues", "no", "corre", "mira", "bueno"),
    c("claramente", "entonces", "pues", "no", "correr", "mirar", "bueno"),
    c("ADV", "ADV", "ADV", "ADV", "VERB", "VERB", "INTJ"),
    c(5, 5, 5, 5, 0, 0, 0), c(rep("advmod", 4), rep("root", 3))
  )
  raw <- biber_fixture(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_49_emphatics, 1)
  expect_equal(raw$f_45_conjuncts, 1)
  expect_gte(raw$f_50_discourse_particles, 2)
  expect_equal(raw$f_67_neg_analytic, 1)
  expect_equal(raw$f_42_adverbs, 4)
  expect_equal(audit_fixture(tokens, feature = "f_42_adverbs")$keyword,
               c("claramente", "entonces", "pues", "no"))
})
