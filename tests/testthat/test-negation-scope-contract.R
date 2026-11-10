negation_scope_texts <- function() {
  c(
    verb = "No vino.",
    noun = "Necesitamos un t\u00e9cnico, no un m\u00e9dico.",
    proper = "Fue Juan, no Pedro.",
    pronoun = "Fue ella, no t\u00fa.",
    number = "Necesitamos tres, no dos.",
    prep = "Vive en Madrid, no en Toledo.",
    quantifier = "No todos vinieron.",
    adjective = "Una respuesta no definitiva.",
    adverb = "No muy lejos de aqu\u00ed.",
    connector = "No obstante, lleg\u00f3.",
    nominal_pp = "El no del comit\u00e9 fue claro.",
    nominal_adj = "Recibimos un no rotundo.",
    nominal_plural = "Los noes fueron claros.",
    prefix_noun = "Es un no fumador.",
    prefix_abstract = "Defiende la no violencia.",
    prefix_adj = "Una persona no residente lleg\u00f3.",
    response = "No.",
    response_clause = "No, no ir\u00e9.",
    parenthetical = "No, en ning\u00fan caso, iremos."
  )
}

expect_negation_scope <- function(tokens) {
  raw <- biber_fixture(tokens, measure = "none", normalize = FALSE)
  expected <- c(rep(1, 9), rep(0, 4), rep(1, 3), 0, 1, 1)
  expect_equal(raw$doc_id, names(negation_scope_texts()))
  expect_equal(raw$f_67_neg_analytic, expected)
  expect_equal(raw$f_16_other_nouns[raw$doc_id == "noun"], 2)
  expect_equal(raw$f_66_neg_synthetic[raw$doc_id == "parenthetical"], 1)
  audit <- audit_fixture(tokens, feature = "f_67_neg_analytic")
  expect_true(all(tolower(audit$keyword) == "no"))
  expect_equal(tabulate(match(audit$doc_id, raw$doc_id), nbins = nrow(raw)), expected)
}

test_that("analytic no can scope over nominal and function-word heads in both schemas", {
  heads <- c(NOUN = "libro", PROPN = "Ana", PRON = "t\u00fa", NUM = "dos",
             DET = "todos", ADP = "por", VERB = "vino", AUX = "es",
             ADJ = "grande", ADV = "bien", SCONJ = "porque")
  clauses <- lapply(seq_along(heads), function(i) {
    contract_clause(names(heads)[i], c("No", heads[i]), c("no", tolower(heads[i])),
                     c("ADV", names(heads)[i]), c(2, 0), c("advmod", "root"))
  })
  tokens <- do.call(rbind, clauses)
  spacy <- dplyr::rename(tokens, pos = "upos", tag = "xpos", morph = "feats")
  spacy$pos[spacy$lemma == "no"] <- "PART"
  spacy$dep_rel[spacy$lemma == "no"] <- "neg"
  class(spacy) <- c("spacyr_parsed", "data.frame")
  for (input in list(tokens, spacy)) {
    raw <- biber_fixture(input, measure = "none", normalize = FALSE)
    expect_equal(raw$f_67_neg_analytic, rep(1, length(heads)))
    expect_equal(raw$f_66_neg_synthetic, rep(0, length(heads)))
    expect_equal(raw$f_16_other_nouns[1:2], c(1, 1))
    expect_equal(word_tokens(input), rep(2, length(heads)))
    expect_equal(biber_fixture(input, measure = "none")$f_67_neg_analytic,
                 rep(500, length(heads)))
    expect_equal(audit_fixture(input, feature = "f_67_neg_analytic")$keyword,
                 rep("No", length(heads)))
  }
  expect_equal(biber_fixture(tokens, measure = "none", normalize = FALSE)$f_42_adverbs[1:2],
               c(1, 1))
  expect_equal(biber_fixture(spacy, measure = "none", normalize = FALSE)$f_42_adverbs[1:2],
               c(0, 0))
})

test_that("no obstante and identifiable nominal no are not analytic negators", {
  connector <- contract_clause(
    "connector", c("No", "obstante", "lleg\u00f3"), c("no", "obstante", "llegar"),
    c("ADV", "ADP", "VERB"), c(3, 3, 0), c("advmod", "case", "root")
  )
  nominal_pp <- contract_clause(
    "nominal_pp", c("El", "no", "de", "el", "comit\u00e9", "fue", "claro"),
    c("el", "no", "de", "el", "comit\u00e9", "ser", "claro"),
    c("DET", "ADV", "ADP", "DET", "NOUN", "AUX", "ADJ"),
    c(5, 5, 5, 5, 7, 7, 0), c("det", "advmod", "case", "det", "nsubj", "cop", "root")
  )
  nominal_adj <- contract_clause(
    "nominal_adj", c("Recibimos", "un", "no", "rotundo"),
    c("recibir", "uno", "no", "rotundo"),
    c("VERB", "DET", "ADV", "ADJ"), c(0, 4, 4, 1),
    c("root", "det", "advmod", "obj")
  )
  quoted <- contract_clause(
    "quoted", c("Dijo", "\"", "no", "\""), c("decir", "\"", "no", "\""),
    c("VERB", "PUNCT", "ADV", "PUNCT"), c(0, 1, 1, 1),
    c("root", "punct", "advmod", "punct")
  )
  tokens <- rbind(connector, nominal_pp, nominal_adj, quoted)
  raw <- biber_fixture(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_67_neg_analytic, rep(0, 4))
  expect_equal(raw$f_42_adverbs, rep(1, 4))
  expect_equal(raw$f_45_conjuncts[1], 1)
  expect_equal(raw$f_66_neg_synthetic, rep(0, 4))
  expect_equal(nrow(audit_fixture(tokens, feature = "f_67_neg_analytic")), 0)
})

test_that("nominalization guards do not reject adjectival or nominal no modifiers", {
  prefix <- contract_clause(
    "prefix", c("Es", "un", "no", "fumador"), c("ser", "uno", "no", "fumador"),
    c("AUX", "DET", "ADV", "ADJ"), c(4, 4, 4, 0),
    c("cop", "det", "advmod", "root")
  )
  attributive <- contract_clause(
    "attributive", c("Un", "no", "categ\u00f3rico", "rechazo"),
    c("uno", "no", "categ\u00f3rico", "rechazo"),
    c("DET", "ADV", "ADJ", "NOUN"), c(4, 3, 4, 0),
    c("det", "advmod", "amod", "root")
  )
  raw <- biber_fixture(rbind(prefix, attributive), measure = "none", normalize = FALSE)
  expect_equal(raw$f_67_neg_analytic, c(1, 1))
  expect_equal(raw$f_16_other_nouns, c(0, 1))
})

test_that("reply prefixes are excluded without discarding the following negator", {
  repeated <- contract_clause(
    "repeated", c("No", ",", "no", ",", "no", "ir\u00e9"),
    c("no", ",", "no", ",", "no", "ir"), c("ADV", "PUNCT", "ADV", "PUNCT", "ADV", "VERB"),
    c(6, 6, 6, 6, 6, 0), c("advmod", "punct", "advmod", "punct", "advmod", "root")
  )
  response <- contract_clause(
    "response", c("No", ",", "lo", "siento"), c("no", ",", "lo", "sentir"),
    c("ADV", "PUNCT", "PRON", "VERB"), c(4, 4, 4, 0),
    c("advmod", "punct", "obj", "root")
  )
  parenthetical <- contract_clause(
    "parenthetical", c("No", ",", "en", "ning\u00fan", "caso", ",", "iremos"),
    c("no", ",", "en", "ninguno", "caso", ",", "ir"),
    c("ADV", "PUNCT", "ADP", "DET", "NOUN", "PUNCT", "VERB"),
    c(7, 7, 5, 5, 7, 7, 0), c("advmod", "punct", "case", "det", "obl", "punct", "root")
  )
  tokens <- rbind(repeated, response, parenthetical)
  raw <- biber_fixture(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_67_neg_analytic, c(1, 0, 1))
  expect_equal(raw$f_42_adverbs, c(3, 1, 1))
  expect_equal(raw$f_66_neg_synthetic, c(0, 0, 1))
  audit <- audit_fixture(tokens, feature = "f_67_neg_analytic")
  expect_equal(audit$doc_id, c("repeated", "parenthetical"))
  expect_equal(audit$keyword, c("no", "No"))
})

test_that("negation context is sentence/document-local and handles token-ID gaps", {
  preceding <- contract_clause("preceding", "un", "uno", "DET", 0, "root")
  scope <- contract_clause(
    "scope", c("No", "rotundo"), c("no", "rotundo"), c("ADV", "ADJ"),
    c(3, 0), c("advmod", "root")
  )
  scope$token_id <- c("1", "3")
  second <- scope
  second$doc_id <- "preceding"
  second$sentence_id <- 2L
  raw <- biber_fixture(rbind(preceding, scope, second), measure = "none", normalize = FALSE)
  expect_equal(raw$doc_id, c("preceding", "scope"))
  expect_equal(raw$f_67_neg_analytic, c(1, 1))
})

test_that("Spanish-GSD recognizes approved negation scopes and exclusions", {
  texts <- negation_scope_texts()
  tokens <- as.data.frame(udpipe::udpipe_annotate(
    .fc_ud(), x = texts, doc_id = names(texts)
  ))
  expect_negation_scope(tokens)
})

test_that("analytic no requires a real nonself lexical head", {
  make_case <- function(id, head, pos = "VERB", word = "vino") {
    contract_clause(id, c("No", word), c("no", word), c("ADV", pos),
                    c(head, 0), c("advmod", "root"))
  }
  tokens <- do.call(rbind, list(
    make_case("valid", 2), make_case("missing", 9), make_case("root", 0),
    make_case("self", 1), make_case("punctuation", 2, "PUNCT", "."),
    make_case("symbol", 2, "SYM", "+"), make_case("reply_chain", 2, "ADV", "no")
  ))
  raw <- biber_fixture(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_67_neg_analytic, c(1, rep(0, 6)))
  expect_equal(audit_fixture(tokens, feature = "f_67_neg_analytic")$doc_id, "valid")
})

test_that("nominal no cues preserve genuine attributive negation", {
  modifiers <- c("rotundo", "tajante", "categ\u00f3rico", "contundente", "terminante")
  nominal <- lapply(modifiers, function(word) {
    contract_clause(word, c("Recibimos", "un", "no", word),
                    c("recibir", "uno", "no", word), c("VERB", "DET", "ADV", "ADJ"),
                    c(0, 4, 4, 1), c("root", "det", "advmod", "obj"))
  })
  attributive <- lapply(modifiers, function(word) {
    contract_clause(paste0("attributive_", word), c("Un", "no", word, "rechazo"),
                    c("uno", "no", word, "rechazo"), c("DET", "ADV", "ADJ", "NOUN"),
                    c(4, 3, 4, 0), c("det", "advmod", "amod", "root"))
  })
  direct_article <- contract_clause(
    "direct_article", c("Recibimos", "un", "no"), c("recibir", "uno", "no"),
    c("VERB", "DET", "ADV"), c(0, 3, 1), c("root", "det", "advmod")
  )
  prenominal <- contract_clause(
    "prenominal", c("Recibimos", "un", "rotundo", "no"),
    c("recibir", "uno", "rotundo", "no"), c("VERB", "DET", "ADJ", "ADV"),
    c(0, 3, 1, 3), c("root", "det", "obj", "advmod")
  )
  tokens <- do.call(rbind, c(nominal, attributive, list(direct_article, prenominal)))
  raw <- biber_fixture(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_67_neg_analytic, c(rep(0, 5), rep(1, 5), 0, 0))
  expect_equal(raw$f_42_adverbs, rep(1, 12))
})

test_that("quoted standalone no is distinct from a quoted negative clause", {
  pairs <- list(c("\"", "\""), c("'", "'"), c("\u00ab", "\u00bb"),
                c("\u201c", "\u201d"), c("\u2018", "\u2019"))
  mentions <- lapply(seq_along(pairs), function(i) {
    contract_clause(paste0("mention_", i), c("Dijo", pairs[[i]][1], "no", pairs[[i]][2]),
                    c("decir", pairs[[i]][1], "no", pairs[[i]][2]),
                    c("VERB", "PUNCT", "ADV", "PUNCT"),
                    c(0, 1, 1, 1), c("root", "punct", "advmod", "punct"))
  })
  clause <- contract_clause(
    "clause", c("\"", "No", "vino", "\""), c("\"", "no", "venir", "\""),
    c("PUNCT", "ADV", "VERB", "PUNCT"), c(3, 3, 0, 3),
    c("punct", "advmod", "root", "punct")
  )
  tokens <- do.call(rbind, c(mentions, list(clause)))
  expect_equal(biber_fixture(tokens, measure = "none", normalize = FALSE)$f_67_neg_analytic,
               c(rep(0, 5), 1))
  expect_equal(audit_fixture(tokens, feature = "f_67_neg_analytic")$keyword, "No")
})

test_that("connective exclusions and comma context use local word positions", {
  no <- contract_clause("no", c("No", "vino"), c("no", "venir"),
                        c("ADV", "VERB"), c(4, 0), c("advmod", "root"))
  no$token_id <- c("2", "4")
  obstante <- contract_clause("obstante", "obstante", "obstante", "ADV", 0, "root")
  second <- obstante
  second$doc_id <- "no"
  second$sentence_id <- 2L
  reply <- contract_clause(
    "reply", c("No", ",", "no", "ir\u00e9"), c("no", ",", "no", "ir"),
    c("ADV", "PUNCT", "ADV", "VERB"), c(12, 12, 12, 0),
    c("advmod", "punct", "advmod", "root")
  )
  reply$token_id <- c("2", "4", "9", "12")
  tokens <- rbind(no, obstante, second, reply)
  expect_equal(biber_fixture(tokens, measure = "none", normalize = FALSE)$f_67_neg_analytic,
               c(1, 0, 1))
  expect_equal(audit_fixture(tokens, feature = "f_67_neg_analytic")$keyword, c("No", "no"))
})

test_that("live Spanish spaCy recognizes the same negation scope contract", {
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
    spacyr::spacy_parse(
      negation_scope_texts(), dependency = TRUE, tag = TRUE, pos = TRUE,
      entity = FALSE, additional_attributes = "morph"
    ),
    warning = function(w) {
      if (startsWith(conditionMessage(w), "lemmatization may not work properly in model")) {
        invokeRestart("muffleWarning")
      }
    }
  )
  expect_negation_scope(tokens)
})
