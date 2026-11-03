test_that("modals recognize passive chains and require their periphrastic markers", {
  possibility <- contract_clause(
    "possibility", c("puede", "ser", "revisado"), c("poder", "ser", "revisar"),
    c("AUX", "AUX", "VERB"), c(3, 3, 0), c("aux", "aux:pass", "root"),
    c("VerbForm=Fin", "VerbForm=Inf", "VerbForm=Part")
  )
  obligation <- contract_clause(
    "obligation", c("tengo", "que", "revisar"), c("tener", "que", "revisar"),
    c("VERB", "SCONJ", "VERB"), c(0, 3, 1), c("root", "mark", "xcomp"),
    c("VerbForm=Fin", NA, "VerbForm=Inf")
  )
  possession <- obligation
  possession$doc_id <- "possession"
  possession$token[2] <- possession$lemma[2] <- "para"
  passive <- contract_clause(
    "passive", c("ha", "de", "ser", "revisado"),
    c("haber", "de", "ser", "revisar"), c("AUX", "ADP", "AUX", "VERB"),
    c(4, 3, 4, 0), c("aux", "mark", "aux:pass", "root"),
    c("VerbForm=Fin", NA, "VerbForm=Inf", "VerbForm=Part")
  )
  split_marker <- obligation
  split_marker$doc_id <- "split_marker"
  split_marker$sentence_id[2] <- 2L
  tokens <- rbind(possibility, obligation, possession, passive, split_marker)
  raw <- biber(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_52_modal_possibility, c(1, 0, 0, 0, 0))
  expect_equal(raw$f_53_modal_necessity, c(0, 1, 0, 1, 0))
  expect_equal(raw$f_02_perfect_aspect, rep(0, 5))
  audit <- audit_features(tokens, feature = "f_53_modal_necessity")
  expect_setequal(audit$keyword, c("tengo", "ha"))
  headed <- passive
  headed$head_token_id[c(1, 2, 4)] <- c("0", "4", "1")
  headed$dep_rel[c(1, 4)] <- c("root", "ccomp")
  expect_equal(biber(headed, measure = "none", normalize = FALSE)$f_53_modal_necessity, 1)
  expect_equal(biber(headed, measure = "none", normalize = FALSE)$f_02_perfect_aspect, 0)
  spacy <- dplyr::rename(tokens, pos = "upos", tag = "xpos", morph = "feats")
  class(spacy) <- c("spacyr_parsed", "data.frame")
  expect_equal(biber(spacy, measure = "none", normalize = FALSE), raw)
})

test_that("puede que accepts an explicitly subjunctive complement clause", {
  tokens <- contract_clause(
    "possible", c("puede", "que", "llueva"), c("poder", "que", "llover"),
    c("VERB", "SCONJ", "VERB"), c(0, 3, 1), c("root", "mark", "ccomp"),
    c("VerbForm=Fin", NA, "Mood=Sub|VerbForm=Fin")
  )
  expect_equal(biber(tokens, measure = "none", normalize = FALSE)$f_52_modal_possibility, 1)
  expect_equal(audit_features(tokens, feature = "f_52_modal_possibility")$keyword, "puede")
  perfect <- contract_clause(
    "perfect", c("puede", "que", "haya", "sido", "revisado"),
    c("poder", "que", "haber", "ser", "revisar"),
    c("VERB", "SCONJ", "AUX", "AUX", "VERB"), c(0, 5, 5, 5, 1),
    c("root", "mark", "aux", "aux:pass", "ccomp"),
    c("VerbForm=Fin", NA, "Mood=Sub|VerbForm=Fin", "VerbForm=Part", "VerbForm=Part")
  )
  raw <- biber(perfect, measure = "none", normalize = FALSE)
  expect_equal(raw$f_52_modal_possibility, 1)
  expect_equal(raw$f_02_perfect_aspect, 1)
})

test_that("modal copular complements work with either dependency direction", {
  headed <- contract_clause(
    "copular", c("puede", "ser", "util"), c("poder", "ser", "util"),
    c("VERB", "AUX", "ADJ"), c(0, 3, 1), c("root", "cop", "xcomp"),
    c("VerbForm=Fin", "VerbForm=Inf", NA)
  )
  auxiliary <- headed
  auxiliary$head_token_id[c(1, 3)] <- c("3", "0")
  auxiliary$dep_rel[c(1, 3)] <- c("aux", "root")
  raw <- biber(headed, measure = "none", normalize = FALSE)
  expect_equal(raw$f_52_modal_possibility, 1)
  expect_equal(biber(auxiliary, measure = "none", normalize = FALSE), raw)
})

test_that("predictive ir a requires a and supports both dependency directions", {
  auxiliary <- contract_clause(
    "auxiliary", c("va", "a", "trabajar"), c("ir", "a", "trabajar"),
    c("AUX", "ADP", "VERB"), c(3, 3, 0), c("aux", "mark", "root"),
    c("Tense=Pres|VerbForm=Fin", NA, "VerbForm=Inf")
  )
  headed <- auxiliary
  headed$doc_id <- "headed"
  headed$upos[1] <- headed$xpos[1] <- "VERB"
  headed$head_token_id[c(1, 3)] <- c("0", "1")
  headed$dep_rel[c(1, 3)] <- c("root", "xcomp")
  missing <- auxiliary
  missing$doc_id <- "missing"
  missing$token[2] <- missing$lemma[2] <- "para"
  past <- auxiliary
  past$doc_id <- "past"
  past$feats[1] <- "Tense=Past|VerbForm=Fin"
  future <- contract_clause("future", "vendra", "venir", "VERB", 0, "root",
                            "Tense=Fut|VerbForm=Fin")
  conditional <- future
  conditional$doc_id <- "conditional"
  conditional$feats <- "Mood=Cnd|VerbForm=Fin"
  tokens <- rbind(auxiliary, headed, missing, past, future, conditional)
  raw <- biber(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_54_modal_predictive, c(1, 1, 0, 0, 1, 1))
  expect_equal(nrow(audit_features(tokens, feature = "f_54_modal_predictive")), 4L)
})

test_that("pronominal lexical entries are detected without surface conjugation patches", {
  realization <- contract_clause(
    "realization", c("se", "dio", "cuenta"), c("el", "dar", "cuenta"),
    c("PRON", "VERB", "NOUN"), c(2, 0, 2), c("iobj", "root", "obj"),
    c("Reflex=Yes|Person=3", "VerbForm=Fin|Person=3", NA)
  )
  literal <- realization
  literal$doc_id <- "literal"
  literal$feats[1] <- "Person=3"
  appearance <- contract_clause(
    "appearance", c("se", "ve", "adecuada"), c("el", "ver", "adecuado"),
    c("PRON", "VERB", "ADJ"), c(2, 3, 0), c("iobj", "cop", "root"),
    c("Reflex=Yes|Person=3", "VerbForm=Fin|Person=3", NA)
  )
  perception <- appearance
  perception$doc_id <- "perception"
  perception$head_token_id <- c("2", "0", "2")
  perception$dep_rel <- c("iobj", "root", "obj")
  perception$upos[3] <- perception$xpos[3] <- "NOUN"
  perception$token[3] <- perception$lemma[3] <- "edificio"
  tokens <- rbind(realization, literal, appearance, perception)
  raw <- biber(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_56_verb_private, c(1, 0, 0, 1))
  expect_equal(raw$f_58_verb_seem, c(0, 0, 1, 0))
  expect_setequal(audit_features(tokens, feature = "f_56_verb_private")$keyword,
                  c("dio", "ve"))
  expect_equal(audit_features(tokens, feature = "f_58_verb_seem")$doc_id, "appearance")
  appearance$head_token_id[1] <- "3"
  expect_equal(biber(appearance, measure = "none", normalize = FALSE)$f_58_verb_seem, 1)
  first_person <- contract_clause(
    "first_person", c("me", "veo", "cansado"), c("yo", "ver", "cansado"),
    c("PRON", "VERB", "ADJ"), c(2, 0, 2), c("iobj", "root", "xcomp"),
    c("Reflex=Yes|Person=1", "VerbForm=Fin|Person=1", NA)
  )
  raw_first_person <- biber(first_person, measure = "none", normalize = FALSE)
  expect_equal(raw_first_person$f_58_verb_seem, 1)
  expect_equal(raw_first_person$f_56_verb_private, 0)
  realization$sentence_id[1] <- 2L
  expect_equal(biber(realization, measure = "none", normalize = FALSE)$f_56_verb_private, 0)
})

test_that("specialized verb classes use lemmas and POS with exact occurrence evidence", {
  tokens <- contract_clause(
    "classes", c("dijo", "pienso", "recomendo", "parece", "poder"),
    c("decir", "pensar", "recomendar", "parecer", "poder"),
    c("VERB", "VERB", "VERB", "AUX", "NOUN"), rep(0, 5), rep("root", 5)
  )
  raw <- biber(tokens, measure = "none", normalize = FALSE)
  features <- c("f_55_verb_public", "f_56_verb_private", "f_57_verb_suasive", "f_58_verb_seem")
  for (feature in features) {
    expect_equal(raw[[feature]], 1)
    expect_equal(nrow(audit_features(tokens, feature = feature)), 1L)
  }
  spacy <- dplyr::rename(tokens, pos = "upos", tag = "xpos", morph = "feats")
  class(spacy) <- c("spacyr_parsed", "data.frame")
  expect_equal(biber(spacy, measure = "none", normalize = FALSE), raw)
})

test_that("Spanish-GSD preserves modal chains and exposes mislemmatized podria", {
  model <- .fc_ud()
  parsed <- as.data.frame(udpipe::udpipe_annotate(
    model,
    x = c("El informe puede ser revisado.", "El informe ha de ser revisado.",
          "Hay que revisar el informe.", "Tiene que ser revisado el informe.",
          "El informe va a ser revisado.", "Ella se dio cuenta del error.",
          "La propuesta se ve adecuada.", "Podr\u00eda f\u00e1cilmente resolverse."),
    doc_id = c("possibility", "necessity", "impersonal", "headed", "future",
               "realization", "appearance", "mislemma")
  ))
  raw <- biber(parsed, measure = "none", normalize = FALSE)
  expect_equal(raw$f_52_modal_possibility, c(1, 0, 0, 0, 0, 0, 0, 0))
  expect_equal(raw$f_53_modal_necessity, c(0, 1, 1, 1, 0, 0, 0, 0))
  expect_equal(raw$f_54_modal_predictive, c(0, 0, 0, 0, 1, 0, 0, 1))
  expect_equal(raw$f_56_verb_private, c(0, 0, 0, 0, 0, 1, 0, 0))
  expect_equal(raw$f_58_verb_seem, c(0, 0, 0, 0, 0, 0, 1, 0))
  expect_equal(raw$f_63_split_auxiliary, rep(0, 8))
  mislemma <- parsed[parsed$doc_id == "mislemma", ]
  expect_equal(mislemma$lemma[1], "podrer")
  mislemma$lemma[1] <- "poder"
  corrected <- biber(mislemma, measure = "none", normalize = FALSE)
  expect_equal(corrected$f_52_modal_possibility, 1)
  expect_equal(corrected$f_63_split_auxiliary, 1)
})
