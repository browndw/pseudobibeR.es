reduction_clause <- function(id, form = "Inf", governor = "pensar") {
  contract_clause(
    id, c("pienso", "ir"), c(governor, "ir"), c("VERB", "VERB"),
    c(0, 1), c("root", "xcomp"), c("VerbForm=Fin", paste0("VerbForm=", form))
  )
}

test_that("f60 includes infinitival and unmarked finite complement reductions", {
  infinitive <- reduction_clause("infinitive")
  finite <- reduction_clause("finite", "Fin", "esperar")
  compound <- contract_clause(
    "compound", c("creo", "haber", "entendido"), c("creer", "haber", "entender"),
    c("VERB", "AUX", "VERB"), c(0, 3, 1), c("root", "aux", "xcomp"),
    c("VerbForm=Fin", "VerbForm=Inf", "VerbForm=Part")
  )
  copula <- contract_clause(
    "copula", c("pienso", "estar", "listo"), c("pensar", "estar", "listo"),
    c("VERB", "AUX", "ADJ"), c(0, 3, 1), c("root", "cop", "xcomp"),
    c("VerbForm=Fin", "VerbForm=Inf", NA)
  )
  finite_compound <- compound
  finite_compound$doc_id <- "finite_compound"
  finite_compound$lemma[1] <- "esperar"
  finite_compound$feats[2] <- "VerbForm=Fin"
  tokens <- rbind(infinitive, finite, compound, copula, finite_compound)
  original <- tokens
  for (input in list(tokens, structure(
    dplyr::rename(tokens, pos = "upos", tag = "xpos", morph = "feats"),
    class = c("spacyr_parsed", "data.frame")
  ))) {
    raw <- biber(input, measure = "none", normalize = FALSE)
    expect_equal(raw$f_60_that_deletion, rep(1, 5))
    expect_equal(raw$f_24_infinitives, c(1, 0, 1, 1, 0))
    expect_equal(biber(input, measure = "none")$f_60_that_deletion,
                 1000 / c(2, 2, 3, 3, 3))
    audit <- audit_features(input, feature = "f_60_that_deletion", window = 0)
    expect_equal(audit$keyword, c("ir", "ir", "entendido", "listo", "entendido"))
  }
  expect_identical(tokens, original)
})

test_that("f60 is not every infinitive, nonfinite clause or markerless clause", {
  governors <- c("poder", "deber", "ir", "intentar", "empezar")
  excluded <- lapply(governors, function(x) reduction_clause(x, governor = x))
  gerund <- reduction_clause("gerund", "Ger")
  participle <- reduction_clause("participle", "Part")
  unknown <- reduction_clause("unknown")
  unknown$feats[2] <- NA_character_
  root <- reduction_clause("root")
  root$dep_rel[2] <- "root"
  subject <- reduction_clause("subject")
  subject$dep_rel[2] <- "csubj"
  relative <- reduction_clause("relative", "Fin")
  relative$dep_rel[2] <- "acl:relcl"
  adjunct <- reduction_clause("adjunct")
  adjunct$dep_rel[2] <- "advcl"
  auxiliary <- reduction_clause("auxiliary")
  auxiliary$upos[1] <- auxiliary$xpos[1] <- "AUX"
  tokens <- do.call(rbind, c(excluded, list(
    gerund, participle, unknown, root, subject, relative, adjunct, auxiliary
  )))
  expect_equal(biber(tokens, measure = "none", normalize = FALSE)$f_60_that_deletion,
               rep(0, length(excluded) + 8))
})

test_that("overt clause introducers and indirect questions exclude reductions", {
  marked <- contract_clause(
    "marked", c("pienso", "que", "viene"), c("pensar", "que", "venir"),
    c("VERB", "SCONJ", "VERB"), c(0, 3, 1), c("root", "mark", "ccomp"),
    c("VerbForm=Fin", NA, "VerbForm=Fin")
  )
  preposition <- marked
  preposition$doc_id <- "preposition"
  preposition$token[2] <- preposition$lemma[2] <- "en"
  preposition$upos[2] <- preposition$xpos[2] <- "ADP"
  preposition$feats[3] <- "VerbForm=Inf"
  question <- marked
  question$doc_id <- "question"
  question$token[2] <- question$lemma[2] <- "c\u00f3mo"
  question$upos[2] <- question$xpos[2] <- "ADV"
  question$dep_rel[2] <- "advmod"
  question$feats[2] <- "PronType=Int"
  question$feats[3] <- "VerbForm=Inf"
  tokens <- rbind(marked, preposition, question)
  expect_equal(biber(tokens, measure = "none", normalize = FALSE)$f_60_that_deletion,
               rep(0, 3))
  marked$upos[2] <- marked$xpos[2] <- "PRON"
  marked$dep_rel[2] <- "obj"
  marked$feats[2] <- "PronType=Rel"
  expect_equal(biber(marked, measure = "none", normalize = FALSE)$f_60_that_deletion, 0)
  marked$token[2] <- marked$lemma[2] <- "quien"
  expect_equal(biber(marked, measure = "none", normalize = FALSE)$f_60_that_deletion, 0)
  expect_equal(biber(question, measure = "none", normalize = FALSE)$f_23_wh_clause, 1)
  question$upos[2] <- question$xpos[2] <- "PRON"
  question$feats[2] <- "PronType=Ind"
  expect_equal(biber(question, measure = "none", normalize = FALSE)$f_60_that_deletion, 0)
})

test_that("coordinated complements inherit governors and overt introducers", {
  tokens <- contract_clause(
    "coordination", c("creo", "saber", "y", "entender"),
    c("creer", "saber", "y", "entender"), c("VERB", "VERB", "CCONJ", "VERB"),
    c(0, 1, 4, 2), c("root", "xcomp", "cc", "conj"),
    c("VerbForm=Fin", "VerbForm=Inf", NA, "VerbForm=Inf")
  )
  marked <- contract_clause(
    "marked_coordination", c("creo", "que", "viene", "y", "sale"),
    c("creer", "que", "venir", "y", "salir"),
    c("VERB", "SCONJ", "VERB", "CCONJ", "VERB"), c(0, 3, 1, 5, 3),
    c("root", "mark", "ccomp", "cc", "conj"),
    c("VerbForm=Fin", NA, "VerbForm=Fin", NA, "VerbForm=Fin")
  )
  mixed <- marked
  mixed$doc_id <- "own_marker"
  mixed$head_token_id[2] <- "5"
  mixed$feats[3] <- "VerbForm=Inf"
  raw <- biber(rbind(tokens, marked, mixed), measure = "none", normalize = FALSE)
  expect_equal(raw$f_60_that_deletion, c(2, 0, 1))
  expect_equal(nrow(audit_features(tokens, feature = "f_60_that_deletion")), 2L)
})

test_that("nominal infinitives and nested overt complements do not inflate f60", {
  nominal <- contract_clause(
    "nominal", c("creo", "el", "comer"), c("creer", "el", "comer"),
    c("VERB", "DET", "VERB"), c(0, 3, 1), c("root", "det", "xcomp"),
    c("VerbForm=Fin", "PronType=Art", "VerbForm=Inf")
  )
  nested <- contract_clause(
    "nested", c("creo", "saber", "que", "viene"),
    c("creer", "saber", "que", "venir"), c("VERB", "VERB", "SCONJ", "VERB"),
    c(0, 1, 4, 2), c("root", "xcomp", "mark", "ccomp"),
    c("VerbForm=Fin", "VerbForm=Inf", NA, "VerbForm=Fin")
  )
  raw <- biber(rbind(nominal, nested), measure = "none", normalize = FALSE)
  expect_equal(raw$f_60_that_deletion, c(0, 1))
  expect_equal(raw$f_15_gerunds, c(1, 0))
})

test_that("complement links stay within documents and sentences and use supplied IDs", {
  tokens <- reduction_clause("noncontiguous")
  tokens$token_id <- c("10", "30")
  tokens$head_token_id[2] <- "10"
  expect_equal(biber(tokens, measure = "none", normalize = FALSE)$f_60_that_deletion, 1)
  traced <- pseudobibeR.es:::biber_es_traced(tokens, measure = "none")
  expect_equal(traced$evidence$token_id[traced$evidence$feature == "f_60_that_deletion"],
               30L)
  separate <- tokens
  separate$sentence_id[2] <- 2L
  expect_equal(biber(separate, measure = "none", normalize = FALSE)$f_60_that_deletion, 0)
  separate$sentence_id <- 1L
  separate$doc_id <- c("governor", "complement")
  expect_equal(biber(separate, measure = "none", normalize = FALSE)$f_60_that_deletion,
               c(0, 0))
})

reduced_complement_texts <- function() {
  c(
    simple = "Pienso ir.",
    compound = "Creo haber entendido.",
    finite = "Espero est\u00e9n bien.",
    marked = "Pienso que voy a ir.",
    modal = "Puedo ir.",
    copula = "Pienso estar listo.",
    marked_copula = "Espero que est\u00e9n bien.",
    finite_compound = "Espero hayan llegado.",
    coordination = "Creo saberlo y entenderlo.",
    marked_coordination = "Creo que lo s\u00e9 y entiendo.",
    mixed = "Creo ir y que Mar\u00eda venga.",
    question = "Pienso c\u00f3mo resolverlo.",
    preposition = "Pienso en ir.",
    other = "Intento comprender.",
    noun = "El comer es saludable.",
    nested = "Creo saber que viene.",
    finite_positive = "Nosotros esperamos lleguen ma\u00f1ana.",
    modalized_finite = "Esperamos puedan ayudar.",
    finite_compound_positive = "Nosotros esperamos hayan llegado.",
    finite_subject = "Yo espero puedas ayudarme."
  )
}

expect_reduced_complement_baseline <- function(tokens, finite_tail) {
  traced <- pseudobibeR.es:::biber_es_traced(tokens, measure = "TTR", normalize = FALSE)
  expected <- c(1, 1, 0, 0, 0, 1, 0, 0, 2, 0, 1, 0, 0, 0, 0, 1, finite_tail)
  expect_equal(traced$counts$doc_id, names(reduced_complement_texts()))
  expect_equal(traced$counts$f_60_that_deletion, expected)
  rows <- traced$evidence[traced$evidence$feature == "f_60_that_deletion", ]
  expect_equal(anyDuplicated(rows[c("doc_id", "sentence_id", "token_id")]), 0L)
  audit <- audit_features(tokens, feature = "f_60_that_deletion")
  expect_equal(audit$doc_id, rows$doc_id)
  expect_equal(tabulate(match(audit$doc_id, traced$counts$doc_id), nbins = 20), expected)
  normalized <- biber(tokens, measure = "TTR", normalize = TRUE)
  expect_equal(normalized$f_60_that_deletion, 1000 * expected / traced$counts$n_tokens)
}

test_that("native GSD captures bounded reductions without repairing lost complement analyses", {
  texts <- reduced_complement_texts()
  tokens <- udpipe::udpipe_annotate(.fc_ud(), x = texts, doc_id = names(texts))
  expect_reduced_complement_baseline(tokens, rep(1, 4))
})

test_that("native spaCy captures the same core reductions with explicit finite parser limits", {
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
    spacyr::spacy_parse(reduced_complement_texts(), dependency = TRUE, pos = TRUE,
                        tag = TRUE, entity = FALSE, additional_attributes = "morph"),
    warning = function(w) {
      if (startsWith(conditionMessage(w), "lemmatization may not work properly in model")) {
        invokeRestart("muffleWarning")
      }
    }
  )
  expect_reduced_complement_baseline(tokens, c(1, 0, 0, 0))
})

test_that("shipped reduction examples distinguish complements from overt clauses and modals", {
  examples <- pseudobibeR.es::spanish_examples
  examples <- examples[examples$feature == "f_60_that_deletion", ]
  expect_equal(nrow(examples), 5L)
  tokens <- udpipe::udpipe_annotate(
    .fc_ud(), x = examples$example, doc_id = paste0("example_", seq_len(nrow(examples)))
  )
  expect_equal(biber(tokens, measure = "none", normalize = FALSE)$f_60_that_deletion,
               examples$count)
})
