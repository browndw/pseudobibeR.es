interrupted_infinitive <- function(id, words = "realmente", pos = rep("ADV", length(words))) {
  n <- length(words) + 2L
  contract_clause(
    id, c("de", words, "comprender"), c("de", words, "comprender"),
    c("ADP", pos, "VERB"), c(rep(n, n - 1L), 0),
    c("mark", rep("advmod", length(words)), "root"),
    c(rep(NA_character_, n - 1L), "VerbForm=Inf")
  )
}

test_that("f62 counts a preposition interrupted by genuine adverbs once per infinitive", {
  single <- interrupted_infinitive("single")
  multiple <- interrupted_infinitive("multiple", c("muy", "bien"))
  long <- interrupted_infinitive("long", c("quiza", "realmente", "muy", "lentamente"))
  mixed <- interrupted_infinitive("mixed", c("no", "realmente"), c("PART", "ADV"))
  tokens <- rbind(single, multiple, long, mixed)
  original <- tokens
  for (input in list(tokens, structure(
    dplyr::rename(tokens, pos = "upos", tag = "xpos", morph = "feats"),
    class = c("spacyr_parsed", "data.frame")
  ))) {
    raw <- biber(input, measure = "none", normalize = FALSE)
    expect_equal(raw$f_62_split_infinitive, rep(1, 4))
    expect_equal(raw$f_24_infinitives, rep(1, 4))
    expect_equal(biber(input, measure = "none")$f_62_split_infinitive, 1000 / c(3, 4, 6, 4))
    audit <- audit_features(input, feature = "f_62_split_infinitive", window = 0)
    expect_equal(audit$keyword, c("de realmente comprender", "de muy bien comprender",
                                "de quiza realmente muy lentamente comprender",
                                "de no realmente comprender"))
    expect_equal(audit$token_count, c(3L, 4L, 6L, 4L))
  }
  expect_identical(tokens, original)
})

test_that("negation-only interruptions are excluded regardless of ADV/PART tagging", {
  words <- c("no", "nunca", "jam\u00e1s", "tampoco")
  inputs <- lapply(words, function(word) interrupted_infinitive(word, word))
  inputs[[length(inputs) + 1L]] <- interrupted_infinitive("particle", "no", "PART")
  inputs[[length(inputs) + 1L]] <- interrupted_infinitive("two", c("no", "nunca"))
  raw <- biber(do.call(rbind, inputs), measure = "none", normalize = FALSE)
  expect_equal(raw$f_62_split_infinitive, rep(0, length(inputs)))
})

test_that("prepositions must introduce the infinitive, directly or through an intervening adverb", {
  direct <- interrupted_infinitive("direct")
  indirect <- direct
  indirect$doc_id <- "adverb_link"
  indirect$head_token_id[1] <- "2"
  indirect$dep_rel[1] <- "case"
  wrong <- indirect
  wrong$doc_id <- "wrong_link"
  wrong$head_token_id[2] <- "0"
  tokens <- rbind(direct, indirect, wrong)
  expect_equal(biber(tokens, measure = "none", normalize = FALSE)$f_62_split_infinitive,
               c(1, 1, 0))
  chain <- interrupted_infinitive("chain", c("muy", "lentamente"))
  chain$head_token_id[1:3] <- c("2", "3", "4")
  chain$dep_rel[1] <- "case"
  expect_equal(biber(chain, measure = "none", normalize = FALSE)$f_62_split_infinitive, 1)
  chain$head_token_id[2] <- "2"
  expect_equal(biber(chain, measure = "none", normalize = FALSE)$f_62_split_infinitive, 0)
})

test_that("compound/copular infinitives and fixed introductory markers retain full matched spans", {
  compound <- contract_clause(
    "compound", c("de", "realmente", "haber", "comprendido"),
    c("de", "realmente", "haber", "comprender"), c("ADP", "ADV", "AUX", "VERB"),
    c(4, 4, 4, 0), c("mark", "advmod", "aux", "root"),
    c(NA, NA, "VerbForm=Inf", "VerbForm=Part")
  )
  copula <- compound
  copula$doc_id <- "copula"
  copula$token[3:4] <- copula$lemma[3:4] <- c("estar", "listo")
  copula$upos[4] <- copula$xpos[4] <- "ADJ"
  copula$dep_rel[3] <- "cop"
  copula$feats[4] <- NA_character_
  fixed <- contract_clause(
    "fixed", c("vamos", "a", "finalmente", "resolver"),
    c("vamos", "a", "finalmente", "resolver"), c("PART", "ADP", "ADV", "VERB"),
    c(4, 1, 4, 0), c("mark", "fixed", "advmod", "root"),
    c(NA, NA, NA, "VerbForm=Inf")
  )
  tokens <- rbind(compound, copula, fixed)
  expect_equal(biber(tokens, measure = "none", normalize = FALSE)$f_62_split_infinitive,
               c(1, 1, 1))
  expect_equal(audit_features(tokens, feature = "f_62_split_infinitive", window = 0)$keyword,
               c("de realmente haber", "de realmente estar", "a finalmente resolver"))
})

test_that("known adverb mistaggings use bounded lookup without retagging input", {
  better <- interrupted_infinitive("better", "mejor", "ADJ")
  better$dep_rel[2] <- "fixed"
  better$head_token_id[2] <- "1"
  even <- better
  even$doc_id <- "even"
  even$token[1:2] <- even$lemma[1:2] <- c("sin", "siquiera")
  even$upos[2] <- even$xpos[2] <- "NOUN"
  original <- rbind(better, even)
  expect_equal(biber(original, measure = "none", normalize = FALSE)$f_62_split_infinitive,
               c(1, 1))
  expect_identical(original$upos, c("ADP", "ADJ", "VERB", "ADP", "NOUN", "VERB"))
})

test_that("ordinary infinitives, subjects, punctuation and nominal infinitives are excluded", {
  plain <- interrupted_infinitive("plain", character())
  no_prep <- interrupted_infinitive("no_prep")
  no_prep$upos[1] <- no_prep$xpos[1] <- "VERB"
  subject <- interrupted_infinitive("subject", c("yo", "realmente"), c("PRON", "ADV"))
  punctuation <- interrupted_infinitive("punctuation", c(",", "realmente"), c("PUNCT", "ADV"))
  finite <- interrupted_infinitive("finite")
  finite$feats[3] <- "VerbForm=Fin"
  nominal <- contract_clause(
    "nominal", c("de", "realmente", "el", "comer"), c("de", "realmente", "el", "comer"),
    c("ADP", "ADV", "DET", "VERB"), c(4, 4, 4, 0), c("mark", "advmod", "det", "root"),
    c(NA, NA, "PronType=Art", "VerbForm=Inf")
  )
  tokens <- rbind(plain, no_prep, subject, punctuation, finite, nominal)
  expect_equal(biber(tokens, measure = "none", normalize = FALSE)$f_62_split_infinitive,
               rep(0, 6))
})

test_that("split spans honor document/sentence boundaries and noncontiguous token IDs", {
  tokens <- interrupted_infinitive("ids")
  tokens$token_id <- c("10", "25", "40")
  tokens$head_token_id <- c("40", "40", "0")
  expect_equal(audit_features(tokens, feature = "f_62_split_infinitive", window = 0)$keyword,
               "de realmente comprender")
  tokens$sentence_id[3] <- 2L
  expect_equal(biber(tokens, measure = "none", normalize = FALSE)$f_62_split_infinitive, 0)
  tokens$sentence_id <- 1L
  tokens$doc_id[3] <- "other"
  expect_equal(biber(tokens, measure = "none", normalize = FALSE)$f_62_split_infinitive,
               c(0, 0))
})

interrupted_infinitive_texts <- function() {
  c(
    de = "Trato de realmente comprender el problema.",
    para = "Lo hizo para mejor entender el problema.",
    sin = "Lo hizo sin siquiera mirar.",
    a = "Vamos a finalmente resolver el problema.",
    multiple = "Trato de muy lentamente comprender el problema.",
    neg_only = "Lo hizo para no volver.",
    never = "Lo hizo para nunca volver.",
    mixed = "Lo hizo para no realmente volver.",
    after = "Trato de comprender realmente.",
    no_prep = "Quiero realmente comprender.",
    nominal = "Habl\u00f3 del buen comer.",
    punctuation = "Trato de, realmente, comprender.",
    compound = "Trato de realmente haber comprendido.",
    split_aux = "Trato de haber realmente comprendido.",
    copula = "Trato de realmente estar listo."
  )
}

expect_interrupted_infinitive_baseline <- function(tokens, para, multiple) {
  original <- tokens
  traced <- pseudobibeR.es:::biber_es_traced(tokens, measure = "TTR", normalize = FALSE)
  expected <- c(1, para, 1, 1, multiple, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1)
  expect_equal(traced$counts$doc_id, names(interrupted_infinitive_texts()))
  expect_equal(traced$counts$f_62_split_infinitive, expected)
  evidence <- traced$evidence[traced$evidence$feature == "f_62_split_infinitive", ]
  expect_equal(anyDuplicated(evidence[c("doc_id", "sentence_id", "token_id")]), 0L)
  audit <- audit_features(tokens, feature = "f_62_split_infinitive", window = 0)
  expect_equal(tabulate(match(audit$doc_id, traced$counts$doc_id), nbins = 15), expected)
  expect_equal(audit$keyword, evidence$token)
  expect_equal(biber(tokens, measure = "TTR")$f_62_split_infinitive,
               1000 * expected / traced$counts$n_tokens)
  expect_identical(tokens, original)
}

test_that("native GSD detects interruptions and excludes negation-only cases", {
  texts <- interrupted_infinitive_texts()
  tokens <- udpipe::udpipe_annotate(.fc_ud(), x = texts, doc_id = names(texts))
  expect_interrupted_infinitive_baseline(tokens, 1, 1)
})

test_that("native spaCy detects core interruptions with explicit marker-attachment limits", {
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
    spacyr::spacy_parse(interrupted_infinitive_texts(), dependency = TRUE, pos = TRUE,
                        tag = TRUE, entity = FALSE, additional_attributes = "morph"),
    warning = function(w) {
      if (startsWith(conditionMessage(w), "lemmatization may not work properly in model")) {
        invokeRestart("muffleWarning")
      }
    }
  )
  expect_interrupted_infinitive_baseline(tokens, 0, 0)
})

test_that("shipped examples include genuine interruptions and exclude negation-only cases", {
  examples <- pseudobibeR.es::spanish_examples
  examples <- examples[examples$feature == "f_62_split_infinitive", ]
  expect_equal(nrow(examples), 5L)
  tokens <- udpipe::udpipe_annotate(
    .fc_ud(), x = examples$example, doc_id = paste0("example_", seq_len(nrow(examples)))
  )
  expect_equal(biber(tokens, measure = "none", normalize = FALSE)$f_62_split_infinitive,
               examples$count)
})
