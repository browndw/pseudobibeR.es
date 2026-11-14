proverb_texts <- function() {
  c(
    anaphor = "Ana abri\u00f3 la puerta y Luis tambi\u00e9n lo hizo.",
    bare = "Luis tambi\u00e9n lo hizo.",
    nominal = "La mesa la hizo Luis.", nominal_lo = "El barco lo hizo Luis.",
    causative = "Lo hizo llorar.", weather = "Hace fr\u00edo.",
    lexical = "Hizo una mesa.",
    infinitive = "Ana abri\u00f3 la puerta; Luis quiere hacerlo tambi\u00e9n.",
    gerund = "Ana abri\u00f3 la puerta; Luis est\u00e1 haci\u00e9ndolo tambi\u00e9n.",
    dative = "Le hizo una mesa.", reflexive = "Se hizo m\u00e9dico.",
    same_action = "Ana nad\u00f3 y Luis hizo lo mismo."
  )
}

expect_proverb_baseline <- function(tokens) {
  raw <- biber_fixture(tokens, measure = "none", normalize = FALSE)
  expected <- c(1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1)
  expect_equal(raw$doc_id, names(proverb_texts()))
  expect_equal(raw$f_12_proverb_do, expected)
  audit <- audit_fixture(tokens, feature = "f_12_proverb_do")
  expect_equal(tabulate(match(audit$doc_id, raw$doc_id), nbins = nrow(raw)), expected)
  expect_true(all(tolower(audit$keyword) %in%
                    c("hizo", "hacer", "hacerlo", "haci\u00e9ndo", "haci\u00e9ndolo")))
}

excluded_proverb_tokens <- function() {
  fin <- "Person=3|Number=Sing|VerbForm=Fin"
  base <- contract_clause(
    "base", c("Lo", "hizo"), c("\u00e9l", "hacer"), c("PRON", "VERB"),
    c(2, 0), c("obj", "root"), c("PronType=Prs", fin)
  )
  auxiliary <- base
  auxiliary$doc_id <- "auxiliary"
  auxiliary$upos[2] <- auxiliary$xpos[2] <- "AUX"
  auxiliary$dep_rel[2] <- "aux"
  copular <- base
  copular$doc_id <- "copular"
  copular$dep_rel[2] <- "cop"
  causative <- contract_clause(
    "causative", c("Lo", "hizo", "llorar"), c("\u00e9l", "hacer", "llorar"),
    c("PRON", "VERB", "VERB"), c(2, 0, 2), c("obj", "root", "xcomp"),
    c(NA, fin, "VerbForm=Inf")
  )
  reflexive <- contract_clause(
    "reflexive", c("Se", "lo", "hizo"), c("se", "\u00e9l", "hacer"),
    c("PRON", "PRON", "VERB"), c(3, 3, 0), c("expl:pv", "obj", "root"),
    c("Reflex=Yes", NA, fin)
  )
  nominal <- contract_clause(
    "nominal", c("barco", "lo", "hizo"), c("barco", "\u00e9l", "hacer"),
    c("NOUN", "PRON", "VERB"), c(3, 3, 0), c("iobj", "obj", "root"),
    c(NA, NA, fin)
  )
  feminine <- base
  feminine$doc_id <- "feminine"
  feminine$token[1] <- "La"
  rbind(auxiliary, copular, causative, reflexive, nominal, feminine)
}

enclitic_tokens <- function() {
  integrated <- contract_clause(
    "integrated", "hacerlo", "hacer \u00e9l", "VERB", 0, "root", "VerbForm=Inf"
  )
  gerund <- contract_clause(
    "gerund", c("haci\u00e9ndo", "lo"), c("haci\u00e9ndo", "\u00e9l"),
    c("VERB", "PRON"), c(0, 1), c("root", "obj"), c("VerbForm=Ger", NA)
  )
  different <- integrated
  different$doc_id <- "different"
  different$token <- "verlo"
  different$lemma <- "ver \u00e9l"
  rbind(integrated, gerund, different)
}

same_tokens <- function() {
  contract_clause(
    "same", c("hizo", "lo", "mismo"), c("hacer", "\u00e9l", "mismo"),
    c("VERB", "PRON", "NOUN"), c(0, 3, 1), c("root", "det", "obj"),
    c("VerbForm=Fin", NA, NA)
  )
}

# The duplicate fixture is a separate document in the batch, so it gets its
# own doc_id (counts are per document).
duplicate_same_tokens <- function() {
  duplicate <- rbind(same_tokens(), contract_clause("same", "lo", "\u00e9l", "PRON", 1, "obj"))
  duplicate$token_id[4] <- "4"
  duplicate$doc_id <- "same_duplicate"
  duplicate
}

# Pipeline calls have a large fixed cost: the independent UDPipe-schema
# fixtures above run as one corpus (one raw call, one audit call); counts and
# audit rows are per document, so each test reads its own documents.
proverb_batch <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      tokens <- rbind(excluded_proverb_tokens(), enclitic_tokens(), same_tokens(),
                      duplicate_same_tokens())
      cache <<- list(raw = biber_fixture(tokens, measure = "none", normalize = FALSE),
                     audit = audit_fixture(tokens, feature = "f_12_proverb_do"))
    }
    cache
  }
})

# Rows of a batched result for the documents of `tokens`, in result order.
proverb_rows <- function(tokens) {
  raw <- proverb_batch()$raw
  raw <- raw[raw$doc_id %in% tokens$doc_id, , drop = FALSE]
  rownames(raw) <- NULL
  raw
}

proverb_audit <- function(tokens) {
  audit <- proverb_batch()$audit
  audit[audit$doc_id %in% tokens$doc_id, ]
}

test_that("f_12 counts the lexical predicate, preserving pronouns and word populations", {
  fin <- "Person=3|Number=Sing|VerbForm=Fin"
  tokens <- contract_clause(
    "action", c("Lo", "hizo"), c("\u00e9l", "hacer"), c("PRON", "VERB"),
    c(2, 0), c("obj", "root"), c("Case=Acc|Gender=Masc|PronType=Prs", fin)
  )
  spacy <- dplyr::rename(tokens, pos = "upos", tag = "xpos", morph = "feats")
  class(spacy) <- c("spacyr_parsed", "data.frame")
  original <- tokens
  for (input in list(tokens, spacy)) {
    raw <- biber_fixture(input, measure = "none", normalize = FALSE)
    expect_equal(raw$f_12_proverb_do, 1)
    expect_equal(raw$f_08_third_person_pronouns, 1)
    expect_equal(word_tokens(input), 2)
    expect_equal(biber_fixture(input, measure = "none")$f_12_proverb_do, 500)
    expect_equal(audit_fixture(input, feature = "f_12_proverb_do")$keyword, "hizo")
  }
  expect_identical(tokens, original)
})

test_that("f_12 excludes auxiliary/copular, causative, reflexive and nominal-object hacer", {
  raw <- proverb_rows(excluded_proverb_tokens())
  expect_equal(raw$f_12_proverb_do, rep(0, 6))
  expect_equal(raw$f_16_other_nouns[raw$doc_id == "nominal"], 1)
})

test_that("split and integrated enclitics recover only the approved hacer representation", {
  tokens <- enclitic_tokens()
  raw <- proverb_rows(tokens)
  expect_equal(raw$f_12_proverb_do, c(1, 1, 0))
  expect_equal(word_tokens(tokens), c(1, 2, 1))
})

test_that("lo mismo is action-proform evidence, not an instruction to consume its noun", {
  tokens <- same_tokens()
  raw <- proverb_rows(tokens)
  expect_equal(raw$f_12_proverb_do, 1)
  expect_equal(raw$f_14_nominalizations + raw$f_16_other_nouns, 1)
  expect_equal(proverb_audit(tokens)$keyword, "hizo")
  duplicate <- duplicate_same_tokens()
  expect_equal(proverb_rows(duplicate)$f_12_proverb_do, 1)
})

test_that("proverb links tolerate word-ID gaps and remain document/sentence-local", {
  tokens <- contract_clause(
    "gap", c("lo", "hizo"), c("\u00e9l", "hacer"), c("PRON", "VERB"),
    c(7, 0), c("obj", "root"), c(NA, "VerbForm=Fin")
  )
  tokens$token_id <- c("2", "7")
  expect_equal(biber_fixture(tokens, measure = "none", normalize = FALSE)$f_12_proverb_do, 1)
  tokens$sentence_id[1] <- 2L
  expect_equal(biber_fixture(tokens, measure = "none", normalize = FALSE)$f_12_proverb_do, 0)
  tokens$sentence_id <- 1L
  tokens$doc_id[1] <- "unrelated"
  expect_true(all(biber_fixture(tokens, measure = "none", normalize = FALSE)$f_12_proverb_do == 0))
  tokens$doc_id <- "gap"
  tokens$token_id[1] <- "7.1"
  expect_equal(biber_fixture(tokens, measure = "none", normalize = FALSE)$f_12_proverb_do, 0)
})

test_that("Spanish-GSD recognizes the bounded pro-verb baseline", {
  texts <- proverb_texts()
  tokens <- as.data.frame(udpipe::udpipe_annotate(.fc_ud(), x = texts, doc_id = names(texts)))
  expect_proverb_baseline(tokens)
})

test_that("shipped Spanish f_12 examples include a pro-verb and a causative contrast", {
  examples <- pseudobibeR.es::spanish_examples
  examples <- examples[examples$feature == "f_12_proverb_do", ]
  expect_true(all(c(0, 1) %in% examples$count))
  tokens <- udpipe::udpipe_annotate(
    .fc_ud(), x = examples$example, doc_id = paste0("example_", seq_len(nrow(examples)))
  )
  expect_equal(biber_fixture(tokens, measure = "none", normalize = FALSE)$f_12_proverb_do,
               examples$count)
})
test_that("live Spanish spaCy recognizes the same pro-verb baseline", {
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
    spacyr::spacy_parse(proverb_texts(), dependency = TRUE, pos = TRUE,
                        tag = TRUE, entity = FALSE, additional_attributes = "morph"),
    warning = function(w) {
      if (startsWith(conditionMessage(w), "lemmatization may not work properly in model")) {
        invokeRestart("muffleWarning")
      }
    }
  )
  expect_proverb_baseline(tokens)
})
