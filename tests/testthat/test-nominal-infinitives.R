nominal_infinitive_texts <- function() {
  c(
    saber = "El saber es valioso.", fumar = "El fumar perjudica la salud.",
    comer = "El buen comer es un placer.", caminar = "El caminar diario es saludable.",
    dormir = "Me preocupa su dormir.", cantar = "El cantar de los p\u00e1jaros me despert\u00f3.",
    coordinated = "El comer y el beber son placeres.",
    verbal = "El fumar cigarrillos perjudica la salud.",
    bare = "Fumar perjudica la salud.", extraposed = "Es importante descansar.",
    suffix = "La mujer vino al taller.", plural = "Los saberes tradicionales son importantes."
  )
}

expect_nominal_infinitive_baseline <- function(tokens) {
  result <- pseudobibeR.es:::biber_es_traced(tokens, measure = "none", normalize = FALSE)
  # verbal (El fumar cigarrillos...) and bare (Fumar perjudica...) are preverbal
  # subject infinitives; extraposed descansar stays a verbal infinitive.
  expected <- c(rep(1, 6), 2, 1, 1, rep(0, 3))
  expect_equal(result$counts$doc_id, names(nominal_infinitive_texts()))
  expect_equal(result$counts$f_15_gerunds, expected)
  nominal <- result$evidence[result$evidence$feature == "f_15_gerunds", ]
  competing <- result$evidence[result$evidence$feature %in%
                                c("f_14_nominalizations", "f_16_other_nouns", "f_24_infinitives"), ]
  keys <- c("doc_id", "sentence_id", "token_id")
  expect_equal(nrow(dplyr::inner_join(nominal, competing, by = keys)), 0L)
  audit <- audit_fixture(tokens, feature = "f_15_gerunds")
  expect_equal(tabulate(match(audit$doc_id, result$counts$doc_id), nbins = nrow(result$counts)),
               expected)
  expect_equal(tolower(audit$keyword),
               c("saber", "fumar", "comer", "caminar", "dormir", "cantar", "comer", "beber",
                 "fumar", "fumar"))
}

test_that("noun-like infinitives move into f_15 without retagging or duplicate noun counts", {
  noun <- contract_clause(
    "noun", c("El", "comer"), c("el", "comero"), c("DET", "NOUN"),
    c(2, 0), c("det", "root"), c("PronType=Art", NA)
  )
  verbal_tag <- contract_clause(
    "verbal_tag", c("su", "dormir"), c("su", "dormir"), c("DET", "VERB"),
    c(2, 0), c("det", "root"), c("Poss=Yes", "VerbForm=Inf")
  )
  tokens <- rbind(noun, verbal_tag)
  original <- tokens
  spacy <- dplyr::rename(tokens, pos = "upos", tag = "xpos", morph = "feats")
  class(spacy) <- c("spacyr_parsed", "data.frame")
  for (input in list(tokens, spacy)) {
    raw <- biber_fixture(input, measure = "none", normalize = FALSE)
    expect_equal(raw$f_15_gerunds, c(1, 1))
    expect_equal(raw$f_14_nominalizations, c(0, 0))
    expect_equal(raw$f_16_other_nouns, c(0, 0))
    expect_equal(raw$f_24_infinitives, c(0, 0))
    expect_equal(word_tokens(input), c(2, 2))
    expect_equal(biber_fixture(input, measure = "none")$f_15_gerunds, c(500, 500))
  }
  result <- pseudobibeR.es:::biber_es_traced(tokens, measure = "none", normalize = FALSE)
  nominal <- result$evidence[result$evidence$feature == "f_15_gerunds", ]
  expect_equal(nominal$upos, c("NOUN", "VERB"))
  expect_equal(nominal$lemma, c("comero", "dormir"))
  expect_identical(tokens, original)
})

test_that("articles do not nominalize a verbal infinitive with objects or adverbial modification", {
  object <- contract_clause(
    "object", c("El", "fumar", "cigarrillos"), c("el", "fumar", "cigarrillo"),
    c("DET", "VERB", "NOUN"), c(2, 0, 2), c("det", "root", "obj"),
    c("PronType=Art", "VerbForm=Inf", NA)
  )
  adverb <- contract_clause(
    "adverb", c("El", "caminar", "lentamente"), c("el", "caminar", "lentamente"),
    c("DET", "VERB", "ADV"), c(2, 0, 2), c("det", "root", "advmod"),
    c("PronType=Art", "VerbForm=Inf", NA)
  )
  bare <- contract_clause(
    "bare", c("Fumar", "perjudica"), c("fumar", "perjudicar"), c("VERB", "VERB"),
    c(2, 0), c("csubj", "root"), c("VerbForm=Inf", "VerbForm=Fin")
  )
  raw <- biber_fixture(rbind(object, adverb, bare), measure = "none", normalize = FALSE)
  # A preverbal subject infinitive is nominal; a root infinitive stays verbal.
  expect_equal(raw$f_15_gerunds, c(0, 0, 1))
  expect_equal(raw$f_24_infinitives, c(1, 1, 0))
  expect_equal(raw$f_16_other_nouns[1], 1)
})

test_that("ordinary suffix lookalikes and plural nouns are not nominal infinitive forms", {
  tokens <- contract_clause(
    "lookalikes", c("el", "taller", "la", "mujer", "los", "saberes"),
    c("el", "taller", "el", "mujer", "el", "saber"),
    c("DET", "NOUN", "DET", "NOUN", "DET", "NOUN"),
    c(2, 0, 4, 0, 6, 0), c("det", "root", "det", "root", "det", "root")
  )
  raw <- biber_fixture(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_15_gerunds, 0)
  expect_equal(raw$f_16_other_nouns, 3)
})

test_that("productive verbal infinitives and accented -ir forms have nominal coverage", {
  tokens <- contract_clause(
    "productive", c("el", "tararear", "el", "re\u00edr"),
    c("el", "tararear", "el", "re\u00edr"), c("DET", "VERB", "DET", "NOUN"),
    c(2, 0, 4, 0), c("det", "root", "det", "root"),
    c(NA, "VerbForm=Inf", NA, NA)
  )
  raw <- biber_fixture(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_15_gerunds, 2)
  expect_equal(raw$f_24_infinitives, 0)
  expect_equal(raw$f_16_other_nouns, 0)
})

test_that("nominal coordination shares context but verbal arguments keep a group verbal", {
  tokens <- contract_clause(
    "coord", c("el", "comer", "y", "beber"), c("el", "comer", "y", "beber"),
    c("DET", "VERB", "CCONJ", "VERB"), c(2, 0, 4, 2),
    c("det", "root", "cc", "conj"), c(NA, "VerbForm=Inf", NA, "VerbForm=Inf")
  )
  raw <- biber_fixture(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_15_gerunds, 2)
  expect_equal(raw$f_24_infinitives, 0)
  object <- contract_clause("coord", "vino", "vino", "NOUN", 4, "obj")
  object$token_id <- "5"
  raw <- biber_fixture(rbind(tokens, object), measure = "none", normalize = FALSE)
  expect_equal(raw$f_15_gerunds, 0)
  expect_equal(raw$f_24_infinitives, 2)
})

test_that("genitive noun modifiers and nominal copular predicates remain eligible", {
  tokens <- contract_clause(
    "genitive", c("el", "cantar", "de", "p\u00e1jaros"),
    c("el", "cantar", "de", "p\u00e1jaro"), c("DET", "NOUN", "ADP", "NOUN"),
    c(2, 0, 4, 2), c("det", "root", "case", "obj")
  )
  predicate <- contract_clause(
    "predicate", c("pasi\u00f3n", "es", "el", "cantar"),
    c("pasi\u00f3n", "ser", "el", "cantar"),
    c("NOUN", "AUX", "DET", "VERB"), c(4, 4, 4, 0),
    c("nsubj", "cop", "det", "root"), c(NA, "VerbForm=Fin", NA, "VerbForm=Inf")
  )
  raw <- biber_fixture(rbind(tokens, predicate), measure = "none", normalize = FALSE)
  expect_equal(raw$f_15_gerunds, c(1, 1))
  expect_equal(raw$f_24_infinitives, c(0, 0))
  expect_equal(raw$f_16_other_nouns[1], 1)
})

test_that("recovered nominal infinitive subjects are not impersonal clausal subjects", {
  tokens <- contract_clause(
    "subject", c("Es", "agradable", "el", "saber"), c("ser", "agradable", "el", "saber"),
    c("AUX", "ADJ", "DET", "VERB"), c(2, 0, 4, 2),
    c("cop", "root", "det", "csubj"),
    c("VerbForm=Fin|Person=3|Number=Sing", NA, NA, "VerbForm=Inf")
  )
  raw <- biber_fixture(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_15_gerunds, 1)
  expect_equal(raw$f_24_infinitives, 0)
  expect_equal(raw$f_09_pronoun_it, 0)
})

test_that("nominal cues remain sentence/document-local and support word-ID gaps", {
  tokens <- contract_clause(
    "gap", c("el", "saber"), c("el", "saber"), c("DET", "NOUN"),
    c(7, 0), c("det", "root")
  )
  tokens$token_id <- c("2", "7")
  expect_equal(biber_fixture(tokens, measure = "none", normalize = FALSE)$f_15_gerunds, 1)
  tokens$sentence_id[1] <- 2L
  expect_equal(biber_fixture(tokens, measure = "none", normalize = FALSE)$f_15_gerunds, 0)
  tokens$sentence_id <- 1L
  tokens$doc_id[1] <- "unrelated"
  expect_true(all(biber_fixture(tokens, measure = "none", normalize = FALSE)$f_15_gerunds == 0))
})

test_that("Spanish-GSD recognizes the noun-like nominal infinitive baseline", {
  texts <- nominal_infinitive_texts()
  tokens <- as.data.frame(udpipe::udpipe_annotate(.fc_ud(), x = texts, doc_id = names(texts)))
  expect_nominal_infinitive_baseline(tokens)
})

test_that("shipped f_15 examples distinguish a nominal infinitive from a verbal subject", {
  examples <- pseudobibeR.es::spanish_examples
  examples <- examples[examples$feature == "f_15_gerunds", ]
  expect_true(all(c(0, 1) %in% examples$count))
  tokens <- udpipe::udpipe_annotate(
    .fc_ud(), x = examples$example, doc_id = paste0("example_", seq_len(nrow(examples)))
  )
  expect_equal(biber_fixture(tokens, measure = "none", normalize = FALSE)$f_15_gerunds,
               examples$count)
})
test_that("live Spanish spaCy recognizes the same noun-like nominal infinitive baseline", {
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
    spacyr::spacy_parse(nominal_infinitive_texts(), dependency = TRUE, pos = TRUE,
                        tag = TRUE, entity = FALSE, additional_attributes = "morph"),
    warning = function(w) {
      if (startsWith(conditionMessage(w), "lemmatization may not work properly in model")) {
        invokeRestart("muffleWarning")
      }
    }
  )
  expect_nominal_infinitive_baseline(tokens)
})
