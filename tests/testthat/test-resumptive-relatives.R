resumptive_clause <- function(id = "resumptive") {
  contract_clause(
    id, c("chico", "que", "habl\u00e9", "de", "\u00e9l"),
    c("chico", "que", "hablar", "de", "\u00e9l"),
    c("NOUN", "PRON", "VERB", "ADP", "PRON"), c(0, 3, 1, 5, 3),
    c("root", "obj", "acl:relcl", "case", "obl"),
    c("Gender=Masc|Number=Sing", "PronType=Rel",
      "VerbForm=Fin|Person=1|Number=Sing", NA,
      "PronType=Prs|Person=3|Gender=Masc|Number=Sing")
  )
}

test_that("bounded resumptive PP relatives count once and leave the gap-relative bins", {
  direct <- resumptive_clause()
  roleless <- direct
  roleless$doc_id <- "roleless"
  roleless$upos[2] <- roleless$xpos[2] <- "SCONJ"
  roleless$dep_rel[2] <- "mark"
  roleless$feats[2] <- NA_character_
  female <- direct
  female$doc_id <- "confiar"
  female$token[c(1, 3:5)] <- c("mujer", "confiamos", "en", "ella")
  female$lemma[c(1, 3:5)] <- c("mujer", "confiar", "en", "\u00e9l")
  female$feats[c(1, 5)] <- gsub("Masc", "Fem", female$feats[c(1, 5)])
  dependency <- direct
  dependency$doc_id <- "depender"
  dependency$token[3] <- "depend\u00ed"
  dependency$lemma[3] <- "depender"
  tokens <- rbind(direct, roleless, female, dependency)
  original <- tokens
  for (input in list(tokens, structure(
    dplyr::rename(tokens, pos = "upos", tag = "xpos", morph = "feats"),
    class = c("spacyr_parsed", "data.frame")
  ))) {
    raw <- biber(input, measure = "none", normalize = FALSE)
    expect_equal(raw$f_61_stranded_preposition, rep(1, 4))
    expect_equal(rowSums(raw[c("f_29_that_subj", "f_30_that_obj", "f_31_wh_subj",
                              "f_32_wh_obj", "f_33_pied_piping", "f_34_sentence_relatives")]),
                 rep(0, 4))
    expect_equal(biber(input, measure = "none")$f_61_stranded_preposition, rep(200, 4))
    audit <- audit_features(input, feature = "f_61_stranded_preposition", window = 0)
    expect_equal(audit$keyword, c("de \u00e9l", "de \u00e9l", "en ella", "de \u00e9l"))
    expect_equal(lengths(strsplit(audit$keyword, " ", fixed = TRUE)), rep(2L, 4))
  }
  expect_identical(tokens, original)
})

test_that("ordinary subject/object relatives and unrelated PP pronouns are not resumption", {
  subject <- resumptive_clause("subject")
  subject$token[3] <- "habl\u00f3"
  subject$dep_rel[2] <- "nsubj"
  subject$feats[3] <- "VerbForm=Fin|Person=3|Number=Sing"
  object <- resumptive_clause("object")
  object$token[3] <- "vi"
  object$lemma[3] <- "ver"
  object$token[4] <- object$lemma[4] <- "con"
  language <- object
  language$doc_id <- "language"
  language$token[1] <- language$lemma[1] <- "idioma"
  language$token[3] <- "habl\u00e9"
  language$lemma[3] <- "hablar"
  gender <- resumptive_clause("gender")
  gender$feats[1] <- "Gender=Fem|Number=Sing"
  number <- resumptive_clause("number")
  number$feats[1] <- "Gender=Masc|Number=Plur"
  pronoun <- resumptive_clause("first_pronoun")
  pronoun$token[5] <- pronoun$lemma[5] <- "m\u00ed"
  pronoun$feats[5] <- "PronType=Prs|Person=1"
  tokens <- rbind(subject, object, language, gender, number, pronoun)
  raw <- biber(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_61_stranded_preposition, rep(0, 6))
  expect_equal(raw$f_29_that_subj[1], 1)
  expect_equal(raw$f_30_that_obj[2:3], c(1, 1))
})

test_that("a finite external subject and a projected finite auxiliary license resumption", {
  external <- contract_clause(
    "external", c("chico", "que", "Maria", "hablo", "de", "\u00e9l"),
    c("chico", "que", "Maria", "hablar", "de", "\u00e9l"),
    c("NOUN", "PRON", "PROPN", "VERB", "ADP", "PRON"),
    c(0, 4, 4, 1, 6, 4), c("root", "obj", "nsubj", "acl:relcl", "case", "obj"),
    c("Gender=Masc|Number=Sing", "PronType=Rel", NA,
      "VerbForm=Fin|Person=3|Number=Sing", NA,
      "PronType=Prs|Person=3|Gender=Masc|Number=Sing")
  )
  compound <- external
  compound$doc_id <- "compound"
  compound$token[3:4] <- c("he", "hablado")
  compound$lemma[3:4] <- c("haber", "hablar")
  compound$upos[3] <- compound$xpos[3] <- "AUX"
  compound$dep_rel[3] <- "aux"
  compound$feats[3:4] <- c("VerbForm=Fin|Person=1|Number=Sing", "VerbForm=Part")
  tokens <- rbind(external, compound)
  expect_equal(biber(tokens, measure = "none", normalize = FALSE)$f_61_stranded_preposition,
               c(1, 1))
})

test_that("pied-piping remains exclusive and unchanged even with a later pronoun", {
  tokens <- contract_clause(
    "pied", c("chico", "de", "que", "hable", "de", "\u00e9l"),
    c("chico", "de", "que", "hablar", "de", "\u00e9l"),
    c("NOUN", "ADP", "PRON", "VERB", "ADP", "PRON"),
    c(0, 3, 4, 1, 6, 4), c("root", "case", "obl", "acl:relcl", "case", "obl"),
    c("Gender=Masc|Number=Sing", NA, "PronType=Rel", "VerbForm=Fin|Person=1",
      NA, "PronType=Prs|Person=3|Gender=Masc|Number=Sing")
  )
  raw <- biber(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_33_pied_piping, 1)
  expect_equal(raw$f_61_stranded_preposition, 0)
})

test_that("overt direct objects, coordinated relative subjects and non-relative clauses are excluded", {
  object <- contract_clause(
    "overt_object", c("chico", "que", "habl\u00e9", "ingles", "de", "\u00e9l"),
    c("chico", "que", "hablar", "ingles", "de", "\u00e9l"),
    c("NOUN", "PRON", "VERB", "NOUN", "ADP", "PRON"),
    c(0, 3, 1, 3, 6, 3), c("root", "obj", "acl:relcl", "obj", "case", "obl"),
    c("Gender=Masc|Number=Sing", "PronType=Rel", "VerbForm=Fin|Person=1", NA,
      NA, "PronType=Prs|Person=3|Gender=Masc|Number=Sing")
  )
  coordinated <- object
  coordinated$doc_id <- "coordinated_subject"
  coordinated$token[3:4] <- c("hablamos", "yo")
  coordinated$dep_rel[2] <- "nsubj"
  coordinated$dep_rel[4] <- "conj"
  coordinated$head_token_id[4] <- "2"
  coordinated$upos[4] <- coordinated$xpos[4] <- "PRON"
  non_relative <- resumptive_clause("complement")
  non_relative$dep_rel[3] <- "ccomp"
  fronted <- resumptive_clause("fronted_pp")
  fronted$token_id <- c("1", "2", "5", "3", "4")
  tokens <- rbind(object, coordinated, non_relative, fronted)
  expect_equal(biber(tokens, measure = "none", normalize = FALSE)$f_61_stranded_preposition,
               rep(0, 4))
})

test_that("lookup-only first-person recovery does not rewrite source annotations", {
  tokens <- resumptive_clause("misparsed")
  tokens$lemma[3] <- "habl\u00e9"
  tokens$upos[3] <- tokens$xpos[3] <- "NOUN"
  tokens$feats[3] <- "Gender=Masc|Number=Sing"
  original <- tokens
  expect_equal(biber(tokens, measure = "none", normalize = FALSE)$f_61_stranded_preposition, 1)
  expect_identical(tokens, original)
})

test_that("resumption counts constructions, not repeated pronouns, and respects supplied boundaries", {
  repeated <- contract_clause(
    "repeated", c("chico", "que", "habl\u00e9", "de", "\u00e9l", "y", "de", "\u00e9l"),
    c("chico", "que", "hablar", "de", "\u00e9l", "y", "de", "\u00e9l"),
    c("NOUN", "PRON", "VERB", "ADP", "PRON", "CCONJ", "ADP", "PRON"),
    c(0, 3, 1, 5, 3, 8, 8, 3),
    c("root", "obj", "acl:relcl", "case", "obl", "cc", "case", "obl"),
    c("Gender=Masc|Number=Sing", "PronType=Rel", "VerbForm=Fin|Person=1", NA,
      "PronType=Prs|Person=3|Gender=Masc|Number=Sing", NA, NA,
      "PronType=Prs|Person=3|Gender=Masc|Number=Sing")
  )
  expect_equal(biber(repeated, measure = "none", normalize = FALSE)$f_61_stranded_preposition, 1)
  expect_equal(nrow(audit_features(repeated, feature = "f_61_stranded_preposition")), 1L)
  tokens <- resumptive_clause("ids")
  tokens$token_id <- c("10", "20", "40", "60", "80")
  tokens$head_token_id <- c("0", "40", "10", "80", "40")
  expect_equal(audit_features(tokens, feature = "f_61_stranded_preposition")$keyword, "de \u00e9l")
  tokens$sentence_id[4:5] <- 2L
  expect_equal(biber(tokens, measure = "none", normalize = FALSE)$f_61_stranded_preposition, 0)
  tokens$sentence_id <- 1L
  tokens$doc_id[4:5] <- "other"
  expect_equal(biber(tokens, measure = "none", normalize = FALSE)$f_61_stranded_preposition,
               c(0, 0))
})

resumptive_relative_texts <- function() {
  c(
    proposed = "Ese es el chico que habl\u00e9 de \u00e9l.",
    plural = "Los chicos que habl\u00e9 de ellos llegaron.",
    female = "La mujer que confi\u00e9 en ella lleg\u00f3.",
    dependence = "El equipo que depend\u00ed de \u00e9l gan\u00f3.",
    external = "El chico que Mar\u00eda habl\u00f3 de \u00e9l lleg\u00f3.",
    compound = "El chico que he hablado de \u00e9l lleg\u00f3.",
    subject = "El chico que habl\u00f3 de \u00e9l lleg\u00f3.",
    object = "El chico que vi con \u00e9l lleg\u00f3.",
    language = "El idioma que habl\u00e9 con \u00e9l era espa\u00f1ol.",
    gender = "La mujer que habl\u00e9 de \u00e9l lleg\u00f3.",
    number = "Los chicos que habl\u00e9 de \u00e9l llegaron.",
    pied = "El chico del que habl\u00e9 lleg\u00f3.",
    pied_repeat = "El chico del que habl\u00e9 de \u00e9l lleg\u00f3.",
    first_pronoun = "El chico que habl\u00e9 de m\u00ed lleg\u00f3.",
    complement = "Dijo que habl\u00e9 de \u00e9l.",
    no_relative = "Habl\u00e9 de \u00e9l ayer.",
    repeated = "El chico que habl\u00e9 de \u00e9l y de \u00e9l lleg\u00f3.",
    second_person = "El chico que hablaste de \u00e9l lleg\u00f3.",
    explicit_first = "El chico que yo habl\u00e9 de \u00e9l lleg\u00f3.",
    confiam = "La mujer que confiamos en ella lleg\u00f3."
  )
}

expect_resumptive_relative_baseline <- function(tokens) {
  original <- tokens
  traced <- pseudobibeR.es:::biber_es_traced(tokens, measure = "TTR", normalize = FALSE)
  expected <- c(rep(1, 6), rep(0, 10), 1, 0, 1, 1)
  expect_equal(traced$counts$doc_id, names(resumptive_relative_texts()))
  expect_equal(traced$counts$f_61_stranded_preposition, expected)
  evidence <- traced$evidence[traced$evidence$feature == "f_61_stranded_preposition", ]
  expect_equal(anyDuplicated(evidence[c("doc_id", "sentence_id", "token_id")]), 0L)
  audit <- audit_features(tokens, feature = "f_61_stranded_preposition", window = 0)
  expect_equal(tabulate(match(audit$doc_id, traced$counts$doc_id), nbins = 20), expected)
  expect_equal(audit$keyword, evidence$token)
  bins <- c("f_29_that_subj", "f_30_that_obj", "f_31_wh_subj", "f_32_wh_obj",
            "f_33_pied_piping", "f_34_sentence_relatives")
  expect_equal(unname(rowSums(traced$counts[expected == 1, bins])), rep(0, sum(expected)))
  expect_equal(traced$counts$f_33_pied_piping[12:13], c(1, 1))
  expect_equal(biber(tokens, measure = "TTR")$f_61_stranded_preposition,
               1000 * expected / traced$counts$n_tokens)
  expect_identical(tokens, original)
}

test_that("native GSD distinguishes resumptive and competing/ordinary relatives", {
  texts <- resumptive_relative_texts()
  tokens <- udpipe::udpipe_annotate(.fc_ud(), x = texts, doc_id = names(texts))
  expect_resumptive_relative_baseline(tokens)
})

test_that("native spaCy supports the same bounded scope without rewriting its annotations", {
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
    spacyr::spacy_parse(resumptive_relative_texts(), dependency = TRUE, pos = TRUE,
                        tag = TRUE, entity = FALSE, additional_attributes = "morph"),
    warning = function(w) {
      if (startsWith(conditionMessage(w), "lemmatization may not work properly in model")) {
        invokeRestart("muffleWarning")
      }
    }
  )
  expect_resumptive_relative_baseline(tokens)
})

test_that("shipped f61 examples distinguish resumption, ordinary relatives and pied-piping", {
  examples <- pseudobibeR.es::spanish_examples
  examples <- examples[examples$feature == "f_61_stranded_preposition", ]
  expect_equal(nrow(examples), 6L)
  tokens <- udpipe::udpipe_annotate(
    .fc_ud(), x = examples$example, doc_id = paste0("example_", seq_len(nrow(examples)))
  )
  expect_equal(biber(tokens, measure = "none", normalize = FALSE)$f_61_stranded_preposition,
               examples$count)
})
