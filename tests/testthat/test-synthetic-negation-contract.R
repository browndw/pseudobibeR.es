synthetic_negation_texts <- function() {
  c(
    determiner = "Ning\u00fan alumno vino.",
    determiner_concord = "No vino ning\u00fan alumno.",
    pronoun = "Nadie vino.",
    pronoun_concord = "No vino nadie.",
    thing = "Nada cambi\u00f3.",
    thing_concord = "No cambi\u00f3 nada.",
    temporal = "Nunca vino.",
    temporal_concord = "No vino nunca.",
    jamais = "Jam\u00e1s vino.",
    additive = "Tampoco vino.",
    additive_concord = "No vino tampoco.",
    coordination = "Ni Juan ni Ana vinieron.",
    focus = "Ni siquiera vino.",
    analytic = "No vino.",
    nominal_nada = "La nada no me preocupa.",
    nominal_nadie = "Es un nadie."
  )
}

expect_synthetic_negation_contract <- function(tokens) {
  raw <- biber_fixture(tokens, measure = "none", normalize = FALSE)
  expected66 <- c(rep(1, 11), 2, 1, 0, 0, 0)
  expected67 <- c(0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0)
  expect_equal(raw$doc_id, names(synthetic_negation_texts()))
  expect_equal(raw$f_66_neg_synthetic, expected66)
  expect_equal(raw$f_67_neg_analytic, expected67)
  audit <- audit_fixture(tokens, feature = "f_66_neg_synthetic")
  expect_equal(tabulate(match(audit$doc_id, raw$doc_id), nbins = nrow(raw)), expected66)
  expect_true(all(tolower(audit$keyword) %in%
                    c("ning\u00fan", "nadie", "nada", "nunca", "jam\u00e1s", "tampoco", "ni")))
}

negative_forms <- c("ning\u00fan", "ninguno", "ninguna", "ningunos", "ningunas",
                    "nadie", "nada", "nunca", "jam\u00e1s", "tampoco", "ni")

concord_tokens <- function(forms = negative_forms) {
  poses <- c(rep("DET", 5), "PRON", "PRON", "ADV", "ADV", "ADV", "CCONJ")
  do.call(rbind, lapply(seq_along(forms), function(i) {
    contract_clause(paste0("form_", i), c("No", "vino", forms[i]),
                    c("no", "venir", forms[i]), c("ADV", "VERB", poses[i]),
                    c(2, 0, 2), c("advmod", "root", "advmod"))
  }))
}

nominal_use_tokens <- function() {
  nominal_nada <- contract_clause(
    "nominal_nada", c("La", "nada", "preocupa"), c("el", "nada", "preocupar"),
    c("DET", "PRON", "VERB"), c(2, 3, 0), c("det", "nsubj", "root")
  )
  nominal_nadie <- contract_clause(
    "nominal_nadie", c("Es", "un", "nadie"), c("ser", "uno", "nadie"),
    c("AUX", "DET", "PRON"), c(3, 3, 0), c("cop", "det", "root")
  )
  genuine <- contract_clause(
    "genuine", c("La", "ninguna", "importancia"), c("el", "ninguno", "importancia"),
    c("DET", "DET", "NOUN"), c(3, 3, 0), c("det", "det", "root")
  )
  rbind(nominal_nada, nominal_nadie, genuine)
}

nadie_case <- function(id, word, relation, head = 2, head_pos = "VERB") {
  contract_clause(id, c(word, "vino"), c(tolower(word), "venir"),
                  c("PROPN", head_pos), c(head, 0), c(relation, "root"))
}

nadie_tokens <- function() {
  do.call(rbind, list(
    nadie_case("subject", "Nadie", "nsubj"),
    nadie_case("passive_subject", "Nadie", "nsubj:pass"),
    nadie_case("other_name", "Nada", "nsubj"),
    nadie_case("object", "Nadie", "obj"),
    nadie_case("missing", "Nadie", "nsubj", 9),
    nadie_case("nonpredicate", "Nadie", "nsubj", head_pos = "NOUN")
  ))
}

nadie_control_tokens <- function() {
  control <- nadie_tokens()
  control$lemma[control$lemma == "nadie"] <- "nombre"
  control
}

# Pipeline calls have a large fixed cost: the independent UDPipe-schema
# fixtures above run as one raw-count corpus and one audit corpus; counts and
# audit rows are per document, so each test reads its own documents. The
# Nadie control (raw) and its original (audit) share doc_ids, so they sit in
# different corpora.
negation_batch <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      shared <- rbind(concord_tokens(), nominal_use_tokens())
      cache <<- list(
        raw = biber_fixture(rbind(shared, nadie_control_tokens()),
                            measure = "none", normalize = FALSE),
        audit = audit_fixture(rbind(shared, nadie_tokens()), feature = "f_66_neg_synthetic")
      )
    }
    cache
  }
})

# Rows of a batched result for the documents of `tokens`, in result order.
negation_rows <- function(tokens) {
  raw <- negation_batch()$raw
  raw <- raw[raw$doc_id %in% tokens$doc_id, , drop = FALSE]
  rownames(raw) <- NULL
  raw
}

negation_audit <- function(tokens) {
  audit <- negation_batch()$audit
  audit[audit$doc_id %in% tokens$doc_id, ]
}

test_that("synthetic lexical negatives retain per-word concord counts in both schemas", {
  forms <- negative_forms
  tokens <- concord_tokens(forms)
  spacy <- dplyr::rename(tokens, pos = "upos", tag = "xpos", morph = "feats")
  class(spacy) <- c("spacyr_parsed", "data.frame")
  for (input in list(tokens, spacy)) {
    batched <- !inherits(input, "spacyr_parsed")
    raw <- if (batched) {
      negation_rows(input)
    } else {
      biber_fixture(input, measure = "none", normalize = FALSE)
    }
    expect_equal(raw$f_66_neg_synthetic, rep(1, length(forms)))
    expect_equal(raw$f_67_neg_analytic, rep(1, length(forms)))
    expect_equal(word_tokens(input), rep(3, length(forms)))
    expect_equal(biber_fixture(input, measure = "none")$f_66_neg_synthetic,
                 rep(1000 / 3, length(forms)), tolerance = 0.001)
    audit <- if (batched) {
      negation_audit(input)
    } else {
      audit_fixture(input, feature = "f_66_neg_synthetic")
    }
    expect_equal(audit$keyword, forms)
  }
})

test_that("ordinary nominal uses are excluded without discarding negative determiners", {
  tokens <- nominal_use_tokens()
  raw <- negation_rows(tokens)
  expect_equal(raw$f_66_neg_synthetic, c(0, 0, 1))
  expect_equal(negation_audit(tokens)$keyword, "ninguna")
})

test_that("Nadie recovery is restricted to the demonstrated verbal subject pattern", {
  tokens <- nadie_tokens()
  raw <- biber_fixture(tokens, measure = "TTR", normalize = FALSE)
  expect_equal(raw$f_66_neg_synthetic, c(1, 1, 0, 0, 0, 0))
  control <- nadie_control_tokens()
  control_raw <- negation_rows(control)
  keep <- setdiff(names(control_raw), "f_66_neg_synthetic")
  expect_equal(raw[keep], control_raw[keep])
  audit <- negation_audit(tokens)
  expect_equal(audit$keyword, c("Nadie", "Nadie"))
  expect_true(all(tokens$upos[tokens$lemma == "nadie"] == "PROPN"))
})

test_that("recovered and ordinary lexical markers retain input occurrence order", {
  make_case <- function(id) {
    contract_clause(id, c("Nadie", "dijo", "nada"), c("nadie", "decir", "nada"),
                    c("PROPN", "VERB", "PRON"), c(2, 0, 2), c("nsubj", "root", "obj"))
  }
  tokens <- rbind(make_case("z"), make_case("a"))
  audit <- audit_fixture(tokens, feature = "f_66_neg_synthetic")
  expect_equal(audit$doc_id, c("z", "z", "a", "a"))
  expect_equal(audit$keyword, c("Nadie", "nada", "Nadie", "nada"))
  expect_equal(biber_fixture(tokens, measure = "none", normalize = FALSE)$f_66_neg_synthetic, c(2, 2))
})

test_that("nominal-use guards are sentence-local, direction-sensitive and gap-safe", {
  article <- contract_clause("other", "La", "el", "DET", 3, "det")
  subject <- contract_clause("subject", c("Nadie", "vino"), c("nadie", "venir"),
                             c("PROPN", "VERB"), c(7, 0), c("nsubj", "root"))
  subject$token_id <- c("3", "7")
  second <- subject
  second$doc_id <- "other"
  second$sentence_id <- 2L
  after <- contract_clause(
    "after", c("Nadie", "vino", "la"), c("nadie", "venir", "el"),
    c("PROPN", "VERB", "DET"), c(2, 0, 1), c("nsubj", "root", "det")
  )
  expect_equal(biber_fixture(rbind(article, subject, second, after),
                     measure = "none", normalize = FALSE)$f_66_neg_synthetic, c(1, 1, 1))
})

test_that("idiom disambiguation and new negative classes remain outside this pass", {
  texts <- c("Nada m\u00e1s llegar, llam\u00f3.", "Antes que nada, revisa el informe.",
             "Est\u00e1 m\u00e1s feliz que nunca.", "Ni bien lleg\u00f3, comenz\u00f3 el trabajo.",
             "Vino sin dinero.")
  tokens <- as.data.frame(udpipe::udpipe_annotate(
    .fc_ud(), x = texts, doc_id = paste0("deferred_", seq_along(texts))
  ))
  expect_equal(biber_fixture(tokens, measure = "none", normalize = FALSE)$f_66_neg_synthetic,
               c(1, 1, 1, 1, 0))
})

test_that("Spanish-GSD meets the ordinary synthetic-negation contract", {
  texts <- synthetic_negation_texts()
  tokens <- as.data.frame(udpipe::udpipe_annotate(.fc_ud(), x = texts, doc_id = names(texts)))
  expect_synthetic_negation_contract(tokens)
})

test_that("live Spanish spaCy meets the ordinary synthetic-negation contract", {
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
    spacyr::spacy_parse(synthetic_negation_texts(), dependency = TRUE, pos = TRUE,
                        tag = TRUE, entity = FALSE, additional_attributes = "morph"),
    warning = function(w) {
      if (startsWith(conditionMessage(w), "lemmatization may not work properly in model")) {
        invokeRestart("muffleWarning")
      }
    }
  )
  expect_synthetic_negation_contract(tokens)
})
