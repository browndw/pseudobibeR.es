test_that("colloquial reductions use surfaces, not unreliable lemmas or POS gates", {
  tokens <- contract_clause(
    "forms", c("pa", "onde", "nde", "ontas", "ta", "tas", "toy", "t\u00e1bamos"),
    rep("incorrect", 8), c("ADP", "PROPN", "ADV", "ADJ", "INTJ", "NOUN", "VERB", "VERB"),
    rep(0, 8), rep("root", 8)
  )
  original <- tokens
  raw <- biber(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_59_contractions, 8)
  expect_equal(raw$n_tokens, 8)
  expect_equal(biber(tokens, measure = "none")$f_59_contractions, 1000)
  evidence <- pseudobibeR.es:::biber_es_traced(tokens, measure = "none")$evidence
  rows <- evidence[evidence$feature == "f_59_contractions", ]
  expect_equal(rows$lemma, rep("incorrect", 8))
  expect_equal(rows$upos, tokens$upos)
  expect_identical(tokens, original)
})

test_that("apostrophe variants and fragmented expressions count once with complete evidence", {
  joined <- contract_clause(
    "joined", "pa\u2019ca\u2019rriba", "incorrect", "NOUN", 0, "root"
  )
  split <- contract_clause(
    "split", c("pa", "\u2019", "ca", "'", "rriba"), rep("incorrect", 5),
    c("ADP", "PROPN", "NOUN", "PUNCT", "VERB"), rep(0, 5), rep("root", 5)
  )
  prefix <- contract_clause(
    "prefix", c("pa'", "rriba"), c("pa'", "rriba"), c("PUNCT", "NOUN"),
    c(0, 0), c("punct", "root")
  )
  tokens <- rbind(joined, split, prefix)
  for (input in list(tokens, structure(
    dplyr::rename(tokens, pos = "upos", tag = "xpos", morph = "feats"),
    class = c("spacyr_parsed", "data.frame")
  ))) {
    raw <- biber(input, measure = "none", normalize = FALSE)
    expect_equal(raw$f_59_contractions, c(1, 1, 1))
    expect_equal(raw$n_tokens, c(1, 4, 1))
    expect_equal(biber(input, measure = "none")$f_59_contractions, c(1000, 250, 1000))
    audit <- audit_features(input, feature = "f_59_contractions", window = 0)
    expect_equal(audit$keyword, c("pa\u2019ca\u2019rriba", "pa \u2019 ca ' rriba", "pa' rriba"))
    expect_equal(audit$count, c(1L, 1L, 1L))
  }
})

test_that("standard contractions, ordinary words and generic apostrophes are not f_59", {
  tokens <- contract_clause(
    "ordinary", c("al", "del", "para", "donde", "estoy", "estas", "est\u00e1s",
                  "O'Connor", "d'Artagnan", "pa'xyz"),
    rep("incorrect", 10), rep("PROPN", 10), rep(0, 10), rep("root", 10)
  )
  expect_equal(biber(tokens, measure = "none", normalize = FALSE)$f_59_contractions, 0)
})

test_that("nominal determiners and short nominal acronyms block obvious homographs", {
  noun <- contract_clause(
    "noun", c("un", "toy"), c("uno", "incorrect"), c("DET", "NOUN"),
    c(2, 0), c("det", "root")
  )
  acronym <- contract_clause("acronym", "TA", "incorrect", "PROPN", 0, "root")
  verb <- acronym
  verb$doc_id <- "uppercase_verb"
  verb$token <- "TOY"
  verb$upos <- verb$xpos <- "VERB"
  raw <- biber(rbind(noun, acronym, verb), measure = "none", normalize = FALSE)
  expect_equal(raw$f_59_contractions, c(0, 0, 1))
})

test_that("contraction matches respect sentence boundaries and noncontiguous IDs", {
  split <- contract_clause(
    "boundary", c("pa", "'", "que"), c("pa", "'", "que"),
    c("ADP", "PUNCT", "SCONJ"), c(0, 0, 0), rep("root", 3)
  )
  split$token_id <- c("3", "9", "20")
  split$sentence_id <- c(1L, 2L, 2L)
  traced <- pseudobibeR.es:::biber_es_traced(split, measure = "none", normalize = FALSE)
  rows <- traced$evidence[traced$evidence$feature == "f_59_contractions", ]
  expect_equal(nrow(rows), 1L)
  expect_equal(rows$token_id, 3L)
  expect_equal(rows$token, "pa")
  expect_equal(audit_features(split, feature = "f_59_contractions")$keyword, "pa")
})

test_that("a reduced spelling tagged PUNCT does not invent a denominator word", {
  tokens <- contract_clause("punct", "pa'", "pa'", "PUNCT", 0, "root")
  raw <- biber(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_59_contractions, 1)
  expect_equal(raw$n_tokens, 0)
  expect_warning(normalized <- biber(tokens, measure = "none"), "no word tokens")
  expect_true(is.na(normalized$f_59_contractions))
})

contraction_contract_texts <- function() {
  c(
    joined = "Ven pa\u2019c\u00e1 y pa'rriba.",
    spaced = "Ven pa\u2019 que hablemos y pa ti.",
    phrase = "Ontas? Ven pa\u2019ca\u2019rriba.",
    where = "Onde vives? Nde vives? Onde's la casa?",
    estar = "Toy aqu\u00ed. Ta lejos. Tas listo. T\u00e1bamos cansados.",
    para_forms = "Vamos pa'que, pa'ti y pa'dentro.",
    standard = "Al salir del taller, compr\u00f3 un toy.",
    names = "O'Connor y d'Artagnan hablaron.",
    full = "Estoy aqu\u00ed. Est\u00e1s listo. Est\u00e1 lejos. Est\u00e1bamos cansados."
  )
}

expect_contraction_baseline <- function(tokens) {
  traced <- pseudobibeR.es:::biber_es_traced(tokens, measure = "none", normalize = FALSE)
  expect_equal(traced$counts$doc_id, names(contraction_contract_texts()))
  expect_equal(traced$counts$f_59_contractions, c(2, 2, 2, 3, 4, 3, 0, 0, 0))
  rows <- traced$evidence[traced$evidence$feature == "f_59_contractions", ]
  expect_equal(anyDuplicated(rows[c("doc_id", "sentence_id", "token_id")]), 0L)
  audit <- audit_features(tokens, feature = "f_59_contractions")
  expect_equal(tabulate(match(audit$doc_id, traced$counts$doc_id), nbins = 9),
               traced$counts$f_59_contractions)
  expect_equal(traced$counts$f_60_that_deletion, rep(0, 9))
  expect_equal(traced$counts$f_61_stranded_preposition, rep(0, 9))
  expect_equal(traced$counts$f_62_split_infinitive, rep(0, 9))
}

test_that("Spanish-GSD recognizes reductions across native apostrophe splits", {
  texts <- contraction_contract_texts()
  tokens <- udpipe::udpipe_annotate(.fc_ud(), x = texts, doc_id = names(texts))
  expect_contraction_baseline(tokens)
})

test_that("native spaCy recognizes the same colloquial expression counts", {
  model <- Sys.getenv("PSEUDOBIBER_ES_SPACY_MODEL")
  skip_if(!nzchar(model), "Set PSEUDOBIBER_ES_SPACY_MODEL to enable live spaCy tests")
  skip_if_not_installed("spacyr")
  skip_if_not_installed("reticulate")
  python <- Sys.getenv("RETICULATE_PYTHON")
  if (!nzchar(python) || !file.exists(python)) {
    stop("Set RETICULATE_PYTHON to the Python executable in r-spacyr.")
  }
  spacyr::spacy_initialize(model = model)
  on.exit(spacyr::spacy_finalize(), add = TRUE)
  tokens <- withCallingHandlers(
    spacyr::spacy_parse(contraction_contract_texts(), dependency = TRUE, pos = TRUE,
                        tag = TRUE, entity = FALSE, additional_attributes = "morph"),
    warning = function(w) {
      if (startsWith(conditionMessage(w), "lemmatization may not work properly in model")) {
        invokeRestart("muffleWarning")
      }
    }
  )
  expect_contraction_baseline(tokens)
})

test_that("shipped contraction examples count reduced expressions, not formal al/del or a noun toy", {
  examples <- pseudobibeR.es::spanish_examples
  examples <- examples[examples$feature == "f_59_contractions", ]
  expect_equal(nrow(examples), 3L)
  expect_true(all(c(0, 1, 2) %in% examples$count))
  tokens <- udpipe::udpipe_annotate(
    .fc_ud(), x = examples$example, doc_id = paste0("example_", seq_len(nrow(examples)))
  )
  expect_equal(biber(tokens, measure = "none", normalize = FALSE)$f_59_contractions,
               examples$count)
})
