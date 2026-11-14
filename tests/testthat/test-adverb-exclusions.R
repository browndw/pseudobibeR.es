specialized_tokens <- function() {
  contract_clause(
    "specialized",
    c("aqu\u00ed", "hoy", "ligeramente", "aproximadamente", "muy", "r\u00e1pidamente"),
    c("aqu\u00ed", "hoy", "ligeramente", "aproximadamente", "muy", "r\u00e1pidamente"),
    rep("ADV", 6), rep(0, 6), rep("root", 6)
  )
}

phrase_tokens <- function() {
  tokens <- contract_clause(
    "phrase", c("Un", "poco", "r\u00e1pidamente"), c("uno", "poco", "r\u00e1pidamente"),
    c("DET", "ADV", "ADV"), c(3, 0, 0), c("det", "root", "root")
  )
  tokens$token_id <- c("1", "3", "7")
  tokens
}

multiple_tokens <- function() {
  contract_clause(
    "multiple", c("M\u00e1s", "o", "menos"), c("m\u00e1s", "o", "menos"),
    c("ADV", "CCONJ", "ADV"), rep(0, 3), rep("root", 3)
  )
}

no_adverbs_tokens <- function() {
  contract_clause(
    "no_adverbs", c("En", "cierta", "medida", "r\u00e1pidamente"),
    c("en", "cierto", "medida", "r\u00e1pidamente"),
    c("ADP", "ADJ", "NOUN", "ADV"), rep(0, 4), rep("root", 4)
  )
}

lemma_tokens <- function() {
  contract_clause(
    "lemma", c("variante_lugar", "variante_tiempo", "r\u00e1pidamente"),
    c("aqu\u00ed", "hoy", "r\u00e1pidamente"), rep("ADV", 3),
    rep(0, 3), rep("root", 3)
  )
}

retained_tokens <- function() {
  contract_clause(
    "retained",
    c("claramente", "entonces", "pues", "no", "corre", "mira", "bueno"),
    c("claramente", "entonces", "pues", "no", "correr", "mirar", "bueno"),
    c("ADV", "ADV", "ADV", "ADV", "VERB", "VERB", "INTJ"),
    c(5, 5, 5, 5, 0, 0, 0), c(rep("advmod", 4), rep("root", 3))
  )
}

# Pipeline calls have a large fixed cost: the independent UDPipe-schema
# fixtures above run as one corpus (one raw call, one audit call); counts and
# audit rows are per document, so each test reads its own documents.
adverb_batch <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      tokens <- rbind(specialized_tokens(), phrase_tokens(), multiple_tokens(),
                      no_adverbs_tokens(), lemma_tokens(), retained_tokens())
      features <- c("f_42_adverbs", "f_46_downtoners", "f_04_place_adverbials")
      cache <<- list(raw = biber_fixture(tokens, measure = "none", normalize = FALSE),
                     audit = audit_fixture(tokens, feature = features))
    }
    cache
  }
})

# Rows of a batched result for the documents of `tokens`, in result order.
adverb_rows <- function(tokens) {
  raw <- adverb_batch()$raw
  raw <- raw[raw$doc_id %in% tokens$doc_id, , drop = FALSE]
  rownames(raw) <- NULL
  raw
}

adverb_audit <- function(tokens, feature) {
  audit <- adverb_batch()$audit
  audit[audit$doc_id %in% tokens$doc_id & audit$feature == feature, ]
}

test_that("f_42 excludes exactly the five specified specialized adverb groups", {
  tokens <- specialized_tokens()
  excluded <- c("f_04_place_adverbials", "f_05_time_adverbials", "f_46_downtoners",
                "f_47_hedges", "f_48_amplifiers")
  spacy <- dplyr::rename(tokens, pos = "upos", tag = "xpos", morph = "feats")
  class(spacy) <- c("spacyr_parsed", "data.frame")
  for (input in list(tokens, spacy)) {
    batched <- !inherits(input, "spacyr_parsed")
    raw <- if (batched) {
      adverb_rows(input)
    } else {
      biber_fixture(input, measure = "none", normalize = FALSE)
    }
    expect_equal(unlist(raw[1, excluded], use.names = FALSE), rep(1, 5))
    expect_equal(raw$f_42_adverbs, 1)
    expect_equal(word_tokens(input), 6)
    expect_equal(biber_fixture(input, measure = "none")$f_42_adverbs, 1000 / 6)
    audit <- if (batched) {
      adverb_audit(input, "f_42_adverbs")
    } else {
      audit_fixture(input, feature = "f_42_adverbs")
    }
    expect_equal(audit$keyword, "r\u00e1pidamente")
    expect_equal(sum(audit$count), raw$f_42_adverbs)
  }
})

test_that("phrase exclusions cover adverbs beyond non-adverb anchors and token gaps", {
  tokens <- phrase_tokens()
  raw <- adverb_rows(tokens)
  expect_equal(raw$f_46_downtoners, 1)
  expect_equal(raw$f_42_adverbs, 1)
  expect_equal(adverb_audit(tokens, "f_46_downtoners")$keyword, "Un poco")
  audit <- adverb_audit(tokens, "f_42_adverbs")
  expect_equal(audit$keyword, "r\u00e1pidamente")
  expect_equal(sum(audit$count), raw$f_42_adverbs)
})

test_that("multi-adverb and overlapping phrases use covered occurrences, not subtraction", {
  multiple <- multiple_tokens()
  no_adverbs <- no_adverbs_tokens()
  raw <- adverb_rows(rbind(multiple, no_adverbs))
  expect_equal(raw$f_46_downtoners, c(1, 1))
  expect_equal(raw$f_47_hedges, c(1, 1))
  expect_equal(raw$f_42_adverbs, c(0, 1))
  audit <- adverb_audit(rbind(multiple, no_adverbs), "f_42_adverbs")
  expect_equal(audit$doc_id, "no_adverbs")
  expect_equal(audit$keyword, "r\u00e1pidamente")
  # Single-document calls, kept separate from the batched corpus.
  expect_equal(biber_fixture(multiple, measure = "none")$f_42_adverbs, 0)
  expect_equal(nrow(audit_fixture(multiple, feature = "f_42_adverbs")), 0L)
})

test_that("lemma-only adverbial detections exclude their actual token keys", {
  tokens <- lemma_tokens()
  raw <- adverb_rows(tokens)
  expect_equal(raw$f_04_place_adverbials, 1)
  expect_equal(raw$f_05_time_adverbials, 1)
  expect_equal(raw$f_42_adverbs, 1)
  expect_equal(adverb_audit(tokens, "f_04_place_adverbials")$keyword,
               "variante_lugar")
  expect_equal(adverb_audit(tokens, "f_42_adverbs")$keyword,
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
  tokens <- retained_tokens()
  raw <- adverb_rows(tokens)
  expect_equal(raw$f_49_emphatics, 1)
  expect_equal(raw$f_45_conjuncts, 1)
  # Mid-clause particles are not f_50 (D-56); f_42 still keeps the adverbs.
  expect_equal(raw$f_50_discourse_particles, 0)
  expect_equal(raw$f_67_neg_analytic, 1)
  expect_equal(raw$f_42_adverbs, 4)
  expect_equal(adverb_audit(tokens, "f_42_adverbs")$keyword,
               c("claramente", "entonces", "pues", "no"))
})
