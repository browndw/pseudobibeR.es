impersonal_predicate_texts <- function() {
  c(
    rain = "Llueve.", snow = "Nieva.",
    weather_aux = "Ha estado lloviendo.", weather_inf = "Puede llover.",
    cold = "Hace fr\u00edo.", heat = "Hace calor.",
    eval_inf = "Es importante descansar.", eval_que = "Es probable que llegue.",
    appear = "Parece que vendr\u00e1.", result = "Resulta que vendr\u00e1.",
    nominal = "Juan es importante.", null_personal = "Lleg\u00f3 ayer.",
    personal_comp = "Quiere descansar.", raising_personal = "Parece querer salir.",
    fronted = "Descansar es importante.", personal_weather = "Llueven cr\u00edticas.",
    personal_faire = "Hace pan.", existential = "Hay problemas.",
    se = "Se trabaja bien.", lo = "Lo somos."
  )
}

expect_impersonal_predicates <- function(tokens) {
  raw <- biber(tokens, measure = "none", normalize = FALSE)
  expected <- c(rep(1, 10), rep(0, 10))
  expect_equal(raw$doc_id, names(impersonal_predicate_texts()))
  expect_equal(raw$f_09_pronoun_it, expected)
  audit <- audit_features(tokens, feature = "f_09_pronoun_it")
  expect_equal(tabulate(match(audit$doc_id, raw$doc_id), nbins = nrow(raw)), expected)
  expect_equal(tolower(audit$keyword),
               c("llueve", "nieva", "lloviendo", "llover", "hace", "hace",
                 "importante", "probable", "parece", "resulta"))
  expect_equal(raw$f_20_existential_there[raw$doc_id == "existential"], 1)
}

test_that("impersonal f_09 counts real predicate anchors rather than null pronouns", {
  fin <- "Mood=Ind|Number=Sing|Person=3|VerbForm=Fin"
  rain <- contract_clause("rain", "Llueve", "llover", "VERB", 0, "root", fin)
  copular <- contract_clause(
    "copular", c("Es", "importante", "descansar"), c("ser", "importante", "descansar"),
    c("AUX", "ADJ", "VERB"), c(2, 0, 2), c("cop", "root", "csubj"),
    c(fin, NA, "VerbForm=Inf")
  )
  clausal <- contract_clause(
    "clausal", c("Parece", "que", "vendr\u00e1"), c("parecer", "que", "venir"),
    c("VERB", "SCONJ", "VERB"), c(0, 3, 1), c("root", "mark", "ccomp"),
    c(fin, NA, fin)
  )
  tokens <- rbind(rain, copular, clausal)
  spacy <- dplyr::rename(tokens, pos = "upos", tag = "xpos", morph = "feats")
  class(spacy) <- c("spacyr_parsed", "data.frame")
  for (input in list(tokens, spacy)) {
    raw <- biber(input, measure = "TTR", normalize = FALSE)
    expect_equal(raw$f_09_pronoun_it, c(1, 1, 1))
    expect_equal(raw$n_tokens, c(1, 3, 3))
    expect_equal(raw$f_08_third_person_pronouns, c(0, 0, 0))
    expect_equal(biber(input, measure = "none")$f_09_pronoun_it,
                 1000 / c(1, 3, 3), tolerance = 0.001)
    expect_equal(audit_features(input, feature = "f_09_pronoun_it")$keyword,
                 c("Llueve", "importante", "Parece"))
  }
})

test_that("weather constructions do not duplicate auxiliary chains or shared complements", {
  fin <- "Mood=Ind|Number=Sing|Person=3|VerbForm=Fin"
  chain <- contract_clause(
    "chain", c("Ha", "estado", "lloviendo"), c("haber", "estar", "llover"),
    c("AUX", "AUX", "VERB"), c(3, 3, 0), c("aux", "aux", "root"),
    c(fin, "VerbForm=Part", "VerbForm=Ger")
  )
  cold <- contract_clause(
    "cold", c("Hace", "fr\u00edo"), c("hacer", "fr\u00edo"),
    c("VERB", "NOUN"), c(0, 1), c("root", "obl"), c(fin, NA)
  )
  repeated <- contract_clause(
    "repeated", c("Es", "importante", "descansar", "comer"),
    c("ser", "importante", "descansar", "comer"), c("AUX", "ADJ", "VERB", "VERB"),
    c(2, 0, 2, 2), c("cop", "root", "csubj", "csubj"),
    c(fin, NA, "VerbForm=Inf", "VerbForm=Inf")
  )
  raw <- biber(rbind(chain, cold, repeated), measure = "none", normalize = FALSE)
  expect_equal(raw$f_09_pronoun_it, c(1, 1, 1))
})

test_that("nominal subjects, personal pro-drop and generic complements do not count", {
  fin <- "Mood=Ind|Number=Sing|Person=3|VerbForm=Fin"
  personal <- contract_clause("personal", "Lleg\u00f3", "llegar", "VERB", 0, "root", fin)
  generic <- contract_clause(
    "generic", c("Quiere", "descansar"), c("querer", "descansar"),
    c("VERB", "VERB"), c(0, 1), c("root", "xcomp"), c(fin, "VerbForm=Inf")
  )
  raising <- generic
  raising$doc_id <- "raising"
  raising$token[1] <- "Parece"
  raising$lemma[1] <- "parecer"
  subject <- contract_clause(
    "subject", c("Juan", "hace", "fr\u00edo"), c("Juan", "hacer", "fr\u00edo"),
    c("PROPN", "VERB", "NOUN"), c(2, 0, 2), c("nsubj", "root", "obj"),
    c(NA, fin, NA)
  )
  fronted <- contract_clause(
    "fronted", c("Descansar", "es", "importante"), c("descansar", "ser", "importante"),
    c("VERB", "AUX", "ADJ"), c(3, 3, 0), c("csubj", "cop", "root"),
    c("VerbForm=Inf", fin, NA)
  )
  adjective <- contract_clause(
    "adjective", c("Est\u00e1", "seguro", "que", "vendr\u00e1"),
    c("estar", "seguro", "que", "venir"), c("AUX", "ADJ", "SCONJ", "VERB"),
    c(2, 0, 4, 2), c("cop", "root", "mark", "ccomp"), c(fin, NA, NA, fin)
  )
  se <- contract_clause(
    "se", c("Se", "parece", "que", "vendr\u00e1"), c("se", "parecer", "que", "venir"),
    c("PRON", "VERB", "SCONJ", "VERB"), c(2, 0, 4, 2),
    c("expl:pv", "root", "mark", "ccomp"), c(NA, fin, NA, fin)
  )
  hay <- contract_clause("hay", "Hay", "haber", "VERB", 0, "root", fin)
  tokens <- rbind(personal, generic, raising, subject, fronted, adjective, se, hay)
  raw <- biber(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_09_pronoun_it, rep(0, 8))
  expect_equal(raw$f_20_existential_there[8], 1)
  expect_equal(nrow(audit_features(tokens, feature = "f_09_pronoun_it")), 0L)
})

test_that("predicate links are sentence-local and tolerate noncontiguous word IDs", {
  fin <- "Mood=Ind|Number=Sing|Person=3|VerbForm=Fin"
  tokens <- contract_clause(
    "gap", c("Es", "importante", "descansar"), c("ser", "importante", "descansar"),
    c("AUX", "ADJ", "VERB"), c(4, 0, 4), c("cop", "root", "csubj"),
    c(fin, NA, "VerbForm=Inf")
  )
  tokens$token_id <- c("1", "4", "9")
  expect_equal(biber(tokens, measure = "none", normalize = FALSE)$f_09_pronoun_it, 1)
  tokens$sentence_id[3] <- 2L
  expect_equal(biber(tokens, measure = "none", normalize = FALSE)$f_09_pronoun_it, 0)
})

test_that("UD empty nodes are not counted or invented as f_09 occurrences", {
  fin <- "Mood=Ind|Number=Sing|Person=3|VerbForm=Fin"
  tokens <- contract_clause("empty", c("Llueve", "pro"), c("llover", "pro"),
                            c("VERB", "PRON"), c(0, 1), c("root", "nsubj"), c(fin, NA))
  tokens$token_id <- c("1", "1.1")
  raw <- biber(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_09_pronoun_it, 1)
  expect_equal(raw$n_tokens, 1)
  expect_equal(audit_features(tokens, feature = "f_09_pronoun_it")$keyword, "Llueve")
})

test_that("closed weather recovery preserves original annotations and excludes nominal roots", {
  tokens <- contract_clause("root", "Llueve", "Llueve", "PROPN", 0, "root")
  original <- tokens
  result <- pseudobibeR.es:::biber_es_traced(tokens, measure = "none", normalize = FALSE)
  evidence <- result$evidence[result$evidence$feature == "f_09_pronoun_it", ]
  expect_equal(result$counts$f_09_pronoun_it, 1)
  expect_equal(result$counts$f_16_other_nouns, 1)
  expect_equal(evidence$upos, "PROPN")
  expect_equal(evidence$lemma, "llueve")
  expect_equal(evidence$token, "llueve")
  expect_equal(evidence$token_id, 1L)
  expect_identical(tokens, original)
  nominal <- contract_clause(
    "nominal", c("El", "Llueve"), c("el", "Llueve"), c("DET", "PROPN"),
    c(2, 0), c("det", "root")
  )
  expect_equal(biber(nominal, measure = "none", normalize = FALSE)$f_09_pronoun_it, 0)
})

test_that("selected impersonal verbs require their own postposed clausal links", {
  fin <- "Mood=Ind|Number=Sing|Person=3|VerbForm=Fin"
  lemmas <- c("parecer", "resultar", "ocurrir", "suceder", "bastar", "convenir")
  forms <- c("Parece", "Resulta", "Ocurre", "Sucede", "Basta", "Conviene")
  tokens <- do.call(rbind, lapply(seq_along(lemmas), function(i) {
    contract_clause(
      lemmas[i], c(forms[i], "que", "vendr\u00e1"), c(lemmas[i], "que", "venir"),
      c("VERB", "SCONJ", "VERB"), c(0, 3, 1), c("root", "mark", "ccomp"),
      c(fin, NA, fin)
    )
  }))
  expect_equal(biber(tokens, measure = "none", normalize = FALSE)$f_09_pronoun_it,
               rep(1, 6))
  tokens$dep_rel[tokens$dep_rel == "ccomp"] <- "csubj"
  expect_equal(biber(tokens, measure = "none", normalize = FALSE)$f_09_pronoun_it,
               rep(1, 6))
  tokens$sentence_id[tokens$token == "que"] <- 2L
  expect_equal(biber(tokens, measure = "none", normalize = FALSE)$f_09_pronoun_it,
               rep(0, 6))
  tokens$sentence_id <- 1L
  tokens$doc_id[tokens$token == "que"] <-
    paste0(tokens$doc_id[tokens$token == "que"], "_unrelated")
  expect_true(all(biber(tokens, measure = "none", normalize = FALSE)$f_09_pronoun_it == 0))
  infinitival <- contract_clause(
    "infinitival", c("Conviene", "descansar"), c("convenir", "descansar"),
    c("VERB", "VERB"), c(0, 1), c("root", "xcomp"), c(fin, "VerbForm=Inf")
  )
  expect_equal(biber(infinitival, measure = "none", normalize = FALSE)$f_09_pronoun_it, 1)
})

test_that("weather predicates respect controlling subjects and finite auxiliary agreement", {
  fin <- "Mood=Ind|Number=Sing|Person=3|VerbForm=Fin"
  controlled <- contract_clause(
    "controlled", c("Juan", "puede", "tronar"), c("Juan", "poder", "tronar"),
    c("PROPN", "VERB", "VERB"), c(2, 0, 2), c("nsubj", "root", "xcomp"),
    c(NA, fin, "VerbForm=Inf")
  )
  embedded <- contract_clause(
    "embedded", c("Juan", "dice", "que", "llueve"), c("Juan", "decir", "que", "llover"),
    c("PROPN", "VERB", "SCONJ", "VERB"), c(2, 0, 4, 2),
    c("nsubj", "root", "mark", "ccomp"), c(NA, fin, NA, fin)
  )
  first_person <- contract_clause(
    "first_person", c("He", "tronado"), c("haber", "tronar"), c("AUX", "VERB"),
    c(2, 0), c("aux", "root"),
    c("Number=Sing|Person=1|VerbForm=Fin", "VerbForm=Part")
  )
  nested <- contract_clause(
    "nested", c("Puedo", "empezar", "a", "tronar"), c("poder", "empezar", "a", "tronar"),
    c("VERB", "VERB", "ADP", "VERB"), c(0, 1, 4, 2),
    c("root", "xcomp", "mark", "xcomp"),
    c("Number=Sing|Person=1|VerbForm=Fin", "VerbForm=Inf", NA, "VerbForm=Inf")
  )
  raw <- biber(rbind(controlled, embedded, first_person, nested),
               measure = "none", normalize = FALSE)
  expect_equal(raw$f_09_pronoun_it, c(0, 1, 0, 0))
})

test_that("Spanish-GSD identifies the approved impersonal predicate baseline", {
  texts <- impersonal_predicate_texts()
  tokens <- as.data.frame(udpipe::udpipe_annotate(.fc_ud(), x = texts, doc_id = names(texts)))
  expect_impersonal_predicates(tokens)
})

test_that("live Spanish spaCy identifies the same impersonal predicate baseline", {
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
    spacyr::spacy_parse(impersonal_predicate_texts(), dependency = TRUE, pos = TRUE,
                        tag = TRUE, entity = FALSE, additional_attributes = "morph"),
    warning = function(w) {
      if (startsWith(conditionMessage(w), "lemmatization may not work properly in model")) {
        invokeRestart("muffleWarning")
      }
    }
  )
  expect_impersonal_predicates(tokens)
})
