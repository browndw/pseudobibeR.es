relative_features <- c("f_29_that_subj", "f_30_that_obj", "f_31_wh_subj",
                       "f_32_wh_obj", "f_33_pied_piping", "f_34_sentence_relatives")

test_that("relative marker family and role define four exclusive bins", {
  subject <- contract_clause(
    "que_subject", c("informe", "que", "llego"), c("informe", "que", "llegar"),
    c("NOUN", "PRON", "VERB"), c(0, 3, 1), c("root", "nsubj", "acl:relcl"),
    c("Number=Sing", "PronType=Rel", "VerbForm=Fin")
  )
  object <- subject
  object$doc_id <- "que_object"
  object$dep_rel[2] <- "obj"
  wh_subject <- subject
  wh_subject$doc_id <- "wh_subject"
  wh_subject$token[2] <- wh_subject$lemma[2] <- "quien"
  wh_object <- object
  wh_object$doc_id <- "wh_object"
  wh_object$token[2] <- wh_object$lemma[2] <- "cual"
  tokens <- rbind(subject, object, wh_subject, wh_object)
  original <- tokens
  for (input in list(tokens, structure(
    dplyr::rename(tokens, pos = "upos", tag = "xpos", morph = "feats"),
    class = c("spacyr_parsed", "data.frame")
  ))) {
    raw <- biber(input, measure = "none", normalize = FALSE)
    expect_equal(unname(as.matrix(raw[relative_features[1:4]])), diag(4))
    expect_equal(rowSums(raw[relative_features]), rep(1, 4))
    expect_equal(biber(input, measure = "none")$f_31_wh_subj, c(0, 0, 1000 / 3, 0))
    for (feature in relative_features[1:4]) {
      expect_equal(nrow(audit_features(input, feature = feature)), 1L)
    }
  }
  expect_identical(tokens, original)
})

test_that("a conflicting explicit subject repairs a relative incorrectly tagged nsubj", {
  tokens <- contract_clause(
    "conflict", c("informe", "cual", "Maria", "reviso"),
    c("informe", "cual", "Maria", "revisar"), c("NOUN", "PRON", "PROPN", "VERB"),
    c(0, 4, 4, 1), c("root", "nsubj", "nsubj", "acl:relcl"),
    c("Number=Sing", "PronType=Rel|Number=Sing", NA, "VerbForm=Fin|Person=3|Number=Sing")
  )
  raw <- biber(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_31_wh_subj, 0)
  expect_equal(raw$f_32_wh_obj, 1)
  evidence <- pseudobibeR.es:::biber_es_traced(tokens, measure = "none")$evidence
  expect_equal(evidence$upos[evidence$feature == "f_32_wh_obj"], "PRON")
  expect_equal(tokens$dep_rel[2], "nsubj")
})

test_that("singular relative and antecedent agreement can resolve a plural pro-drop predicate", {
  tokens <- contract_clause(
    "drop", c("informe", "cual", "revisamos"), c("informe", "cual", "revisar"),
    c("NOUN", "PRON", "VERB"), c(0, 3, 1), c("root", "nsubj", "acl:relcl"),
    c("Number=Sing", "PronType=Rel|Number=Sing", "VerbForm=Fin|Person=1|Number=Plur")
  )
  plural <- tokens
  plural$doc_id <- "plural_subject"
  plural$token[1:2] <- c("candidatos", "quienes")
  plural$lemma[1:2] <- c("candidato", "quien")
  plural$feats[1:2] <- c("Number=Plur", "PronType=Rel|Number=Plur")
  raw <- biber(rbind(tokens, plural), measure = "none", normalize = FALSE)
  expect_equal(raw$f_31_wh_subj, c(0, 1))
  expect_equal(raw$f_32_wh_obj, c(1, 0))
})

test_that("coordinated subjects block disagreement and duplicate-subject repairs", {
  tokens <- contract_clause(
    "coordinated", c("informe", "cual", "y", "yo", "revisamos"),
    c("informe", "cual", "y", "yo", "revisar"),
    c("NOUN", "PRON", "CCONJ", "PRON", "VERB"), c(0, 5, 4, 2, 1),
    c("root", "nsubj", "cc", "conj", "acl:relcl"),
    c("Number=Sing", "PronType=Rel|Number=Sing", NA, "Person=1",
      "VerbForm=Fin|Person=1|Number=Plur")
  )
  conflict <- tokens
  conflict$doc_id <- "coordinated_conflict"
  conflict$head_token_id[4] <- 5
  conflict$dep_rel[4] <- "nsubj"
  raw <- biber(rbind(tokens, conflict), measure = "none", normalize = FALSE)
  expect_equal(raw$f_31_wh_subj, c(1, 1))
  expect_equal(raw$f_32_wh_obj, c(0, 0))
})

test_that("roleless que needs positive subject evidence, not just absence of another subject", {
  tokens <- contract_clause(
    "intransitive", c("hombre", "que", "vino"), c("hombre", "que", "venir"),
    c("NOUN", "SCONJ", "VERB"), c(0, 3, 1), c("root", "mark", "acl:relcl"),
    c(NA, NA, "VerbForm=Fin|Person=3|Number=Sing")
  )
  unknown <- tokens
  unknown$doc_id <- "unknown"
  unknown$token[3] <- "compro"
  unknown$lemma[3] <- "comprar"
  raw <- biber(rbind(tokens, unknown), measure = "none", normalize = FALSE)
  expect_equal(raw$f_29_that_subj, c(1, 0))
  expect_equal(raw$f_30_that_obj, c(0, 0))
})

test_that("finite appos predicates with an explicit relative pronoun recover relative attachments", {
  tokens <- contract_clause(
    "appos", c("informe", "cual", "revisamos"), c("informe", "cual", "revisar"),
    c("NOUN", "PRON", "VERB"), c(0, 3, 1), c("root", "obj", "appos"),
    c("Number=Sing", "PronType=Rel", "VerbForm=Fin|Person=1|Number=Plur")
  )
  interrogative <- tokens
  interrogative$doc_id <- "interrogative"
  interrogative$feats[2] <- "PronType=Int"
  raw <- biber(rbind(tokens, interrogative), measure = "none", normalize = FALSE)
  expect_equal(raw$f_32_wh_obj, c(1, 0))
})

test_that("pied-piping stays exclusive even when the relative carries a subject role", {
  tokens <- contract_clause(
    "pied", c("informe", "de", "el", "cual", "habla"),
    c("informe", "de", "el", "cual", "hablar"),
    c("NOUN", "ADP", "DET", "PRON", "VERB"), c(0, 4, 4, 5, 1),
    c("root", "case", "det", "nsubj", "acl:relcl"),
    c(NA, NA, NA, "PronType=Rel", "VerbForm=Fin")
  )
  traced <- pseudobibeR.es:::biber_es_traced(tokens, measure = "none", normalize = FALSE)
  expect_equal(as.numeric(as.matrix(traced$counts[relative_features])), c(0, 0, 0, 0, 1, 0))
  expect_equal(traced$evidence$feature[traced$evidence$feature %in% relative_features],
               "f_33_pied_piping")
})

test_that("relative classification respects sentence identity and noncontiguous word IDs", {
  subject <- contract_clause(
    "boundary", c("informe", "cual", "llego"), c("informe", "cual", "llegar"),
    c("NOUN", "PRON", "VERB"), c(0, 3, 1), c("root", "nsubj", "acl:relcl"),
    c("Number=Sing", "PronType=Rel", "VerbForm=Fin")
  )
  subject$token_id <- c("10", "20", "40")
  subject$head_token_id <- c("0", "40", "10")
  object <- contract_clause(
    "boundary", c("libro", "que", "Maria", "escribio"),
    c("libro", "que", "Maria", "escribir"), c("NOUN", "SCONJ", "PROPN", "VERB"),
    c(0, 4, 4, 1), c("root", "mark", "nsubj", "acl:relcl")
  )
  object$sentence_id <- 2L
  object$token_id <- c("10", "20", "30", "40")
  object$head_token_id <- c("0", "40", "40", "10")
  traced <- pseudobibeR.es:::biber_es_traced(
    rbind(subject, object), measure = "none", normalize = FALSE
  )
  expect_equal(traced$counts$f_31_wh_subj, 1)
  expect_equal(traced$counts$f_30_that_obj, 1)
  rows <- traced$evidence[traced$evidence$feature == "f_31_wh_subj", ]
  expect_equal(rows$token_id, 20L)
  expect_equal(rows$sentence_id, 1L)
})

relative_partition_texts <- function() {
  c(
    que_subject = "El estudiante que lleg\u00f3 tarde no pudo entrar.",
    que_object = "El libro que Mar\u00eda escribi\u00f3 es famoso.",
    wh_subject = "Los candidatos, quienes aprobaron el examen, recibieron una beca.",
    wh_object = "El informe, el cual Mar\u00eda revis\u00f3, fue publicado.",
    wh_drop = "El informe, el cual revisamos ayer, fue publicado.",
    pied = "El informe del cual Mar\u00eda habl\u00f3 fue publicado.",
    free = "Quien apruebe el examen recibir\u00e1 una beca.",
    question = "No s\u00e9 qui\u00e9n aprob\u00f3 el examen.",
    sentential = "Lleg\u00f3 tarde, lo cual molest\u00f3 a todos."
  )
}

expect_relative_partition_baseline <- function(tokens) {
  traced <- pseudobibeR.es:::biber_es_traced(tokens, measure = "none", normalize = FALSE)
  expected <- matrix(0, nrow = 9, ncol = 6)
  expected[cbind(c(1:6, 9), c(1, 2, 3, 4, 4, 5, 6))] <- 1
  expect_equal(traced$counts$doc_id, names(relative_partition_texts()))
  expect_equal(unname(as.matrix(traced$counts[relative_features])), expected)
  rows <- traced$evidence[traced$evidence$feature %in% relative_features, ]
  expect_equal(anyDuplicated(rows[c("doc_id", "sentence_id", "token_id")]), 0L)
  for (feature in relative_features) {
    audit <- audit_features(tokens, feature = feature)
    expect_equal(tabulate(match(audit$doc_id, traced$counts$doc_id), nbins = 9),
                 expected[, match(feature, relative_features)])
  }
}

test_that("Spanish-GSD supports the four-way relative partition and exclusive pied-piping", {
  texts <- relative_partition_texts()
  tokens <- udpipe::udpipe_annotate(.fc_ud(), x = texts, doc_id = names(texts))
  expect_relative_partition_baseline(tokens)
})

test_that("native spaCy supports the same relative partition", {
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
    spacyr::spacy_parse(relative_partition_texts(), dependency = TRUE, pos = TRUE,
                        tag = TRUE, entity = FALSE, additional_attributes = "morph"),
    warning = function(w) {
      if (startsWith(conditionMessage(w), "lemmatization may not work properly in model")) {
        invokeRestart("muffleWarning")
      }
    }
  )
  expect_relative_partition_baseline(tokens)
})

test_that("shipped wh-relative examples distinguish nominal relatives from questions and pied-piping", {
  examples <- pseudobibeR.es::spanish_examples
  examples <- examples[examples$feature %in% c("f_31_wh_subj", "f_32_wh_obj"), ]
  expect_equal(nrow(examples), 4L)
  for (feature in c("f_31_wh_subj", "f_32_wh_obj")) {
    subset <- examples[examples$feature == feature, ]
    expect_true(all(c(0, 1) %in% subset$count))
    tokens <- udpipe::udpipe_annotate(
      .fc_ud(), x = subset$example, doc_id = paste0("example_", seq_len(nrow(subset)))
    )
    expect_equal(biber(tokens, measure = "none", normalize = FALSE)[[feature]], subset$count)
  }
})
