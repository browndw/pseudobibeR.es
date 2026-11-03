test_that("que complement clauses do not absorb purpose or conditional clauses", {
  complement <- contract_clause(
    "complement", c("dijo", "que", "viene"), c("decir", "que", "venir"),
    c("VERB", "SCONJ", "VERB"), c(0, 3, 1), c("root", "mark", "ccomp")
  )
  purpose <- contract_clause(
    "purpose", c("trabaja", "para", "que", "vivan"),
    c("trabajar", "para", "que", "vivir"), c("VERB", "ADP", "SCONJ", "VERB"),
    c(0, 4, 4, 1), c("root", "mark", "mark", "advcl")
  )
  conditional <- contract_clause(
    "conditional", c("sale", "salvo", "que", "llueva"),
    c("salir", "salvo", "que", "llover"), c("VERB", "SCONJ", "SCONJ", "VERB"),
    c(0, 4, 4, 1), c("root", "mark", "mark", "advcl")
  )
  adjective <- contract_clause(
    "adjective", c("importante", "que", "vengas"), c("importante", "que", "venir"),
    c("ADJ", "SCONJ", "VERB"), c(0, 3, 1), c("root", "mark", "csubj")
  )
  tokens <- rbind(complement, purpose, conditional, adjective)
  raw <- biber(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_21_that_verb_comp, c(1, 0, 0, 0))
  expect_equal(raw$f_22_that_adj_comp, c(0, 0, 0, 1))
  expect_equal(raw$f_37_if, c(0, 0, 1, 0))
  expect_equal(raw$f_38_other_adv_sub, c(0, 1, 0, 0))
  for (feature in c("f_21_that_verb_comp", "f_22_that_adj_comp", "f_37_if",
                    "f_38_other_adv_sub")) {
    audit <- audit_features(tokens, feature = feature)
    expect_equal(tabulate(match(audit$doc_id, raw$doc_id), nbins = nrow(raw)),
                 raw[[feature]])
  }
})

test_that("infinitive morphology includes bare roots and auxiliary infinitives", {
  bare <- contract_clause("bare", "fumar", "fumar", "VERB", 0, "root", "VerbForm=Inf")
  auxiliary <- contract_clause(
    "auxiliary", c("debe", "ser", "revisado"), c("deber", "ser", "revisar"),
    c("AUX", "AUX", "VERB"), c(3, 3, 0), c("aux", "aux:pass", "root"),
    c("VerbForm=Fin|Person=3", "VerbForm=Inf", "VerbForm=Part")
  )
  noun <- bare
  noun$doc_id <- "nominal"
  noun$upos <- noun$xpos <- "NOUN"
  tokens <- rbind(bare, auxiliary, noun)
  raw <- biber(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_24_infinitives, c(1, 1, 0))
  expect_equal(raw$f_15_gerunds, rep(0, 3))
  expect_setequal(audit_features(tokens, feature = "f_24_infinitives")$keyword,
                  c("fumar", "ser"))
})

test_that("postnominal participles do not also count as adverbial participles", {
  modifier <- contract_clause(
    "modifier", c("articulo", "publicado"), c("articulo", "publicar"),
    c("NOUN", "VERB"), c(0, 1), c("root", "acl"), c(NA, "VerbForm=Part")
  )
  absolute <- contract_clause(
    "absolute", c("terminada", "salieron"), c("terminar", "salir"),
    c("VERB", "VERB"), c(2, 0), c("advcl", "root"),
    c("VerbForm=Part", "VerbForm=Fin")
  )
  tokens <- rbind(modifier, absolute)
  raw <- biber(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_26_past_participle, c(0, 1))
  expect_equal(raw$f_27_past_participle_whiz, c(1, 0))
  expect_equal(audit_features(tokens, feature = "f_26_past_participle")$doc_id, "absolute")
})

test_that("pied-piping needs a nominal relative and takes precedence over object relatives", {
  relative <- contract_clause(
    "relative", c("persona", "con", "quien", "hablo"),
    c("persona", "con", "quien", "hablar"), c("NOUN", "ADP", "PRON", "VERB"),
    c(0, 3, 4, 1), c("root", "case", "obl", "acl:relcl"),
    c(NA, NA, "PronType=Rel", "VerbForm=Fin")
  )
  complement <- contract_clause(
    "complement", c("insiste", "en", "que", "vengas"),
    c("insistir", "en", "que", "venir"), c("VERB", "ADP", "SCONJ", "VERB"),
    c(0, 4, 4, 1), c("root", "mark", "mark", "ccomp")
  )
  free_relative <- relative
  free_relative$doc_id <- "free"
  free_relative$upos[1] <- free_relative$xpos[1] <- "VERB"
  free_relative$token[1] <- free_relative$lemma[1] <- "hablar"
  free_relative$dep_rel[4] <- "ccomp"
  relative_que <- relative
  relative_que$doc_id <- "relative_que"
  relative_que$token[3] <- relative_que$lemma[3] <- "que"
  relative_que$upos[3] <- relative_que$xpos[3] <- "SCONJ"
  relative_que$dep_rel[3] <- "mark"
  relative_que$feats[3] <- NA_character_
  tokens <- rbind(relative, relative_que, complement, free_relative)
  raw <- biber(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_33_pied_piping, c(1, 1, 0, 0))
  expect_equal(raw$f_30_that_obj, rep(0, 4))
  expect_equal(raw$f_29_that_subj, rep(0, 4))
  expect_equal(raw$f_31_wh_subj, rep(0, 4))
  expect_equal(raw$f_32_wh_obj, rep(0, 4))
  expect_setequal(audit_features(tokens, feature = "f_33_pied_piping")$doc_id,
                  c("relative", "relative_que"))
  spacy <- dplyr::rename(tokens, pos = "upos", tag = "xpos", morph = "feats")
  class(spacy) <- c("spacyr_parsed", "data.frame")
  expect_equal(biber(spacy, measure = "none", normalize = FALSE), raw)
})

test_that("nonprepositional wh relatives retain subject and object classifications", {
  subject <- contract_clause(
    "subject", c("autora", "quien", "presento"), c("autora", "quien", "presentar"),
    c("NOUN", "PRON", "VERB"), c(0, 3, 1), c("root", "nsubj", "acl:relcl"),
    c(NA, "PronType=Rel", "VerbForm=Fin")
  )
  object <- subject
  object$doc_id <- "object"
  object$dep_rel[2] <- "obj"
  nonrelative <- object
  nonrelative$doc_id <- "nonrelative"
  nonrelative$dep_rel[3] <- "ccomp"
  raw <- biber(rbind(subject, object, nonrelative), measure = "none", normalize = FALSE)
  expect_equal(raw$f_29_that_subj, c(1, 0, 0))
  expect_equal(raw$f_30_that_obj, c(0, 1, 0))
})
test_that("conditional si is not suppressed merely by a communication main verb", {
  conditional <- contract_clause(
    "conditional", c("si", "llueve", "dire"), c("si", "llover", "decir"),
    c("SCONJ", "VERB", "VERB"), c(2, 3, 0), c("mark", "advcl", "root")
  )
  indirect <- conditional
  indirect$doc_id <- "indirect"
  indirect$dep_rel[2] <- "ccomp"
  tokens <- rbind(conditional, indirect)
  raw <- biber(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_37_if, c(1, 0))
  expect_equal(audit_features(tokens, feature = "f_37_if")$doc_id, "conditional")
})

test_that("conditional phrases are counted once with complete sentence-local evidence", {
  phrase <- contract_clause(
    "phrase", c("sale", "a", "menos", "que", "llueva"),
    c("salir", "a", "menos", "que", "llover"),
    c("VERB", "ADP", "ADV", "SCONJ", "VERB"),
    c(0, 5, 2, 5, 1), c("root", "mark", "fixed", "mark", "advcl")
  )
  comparison <- phrase
  comparison$doc_id <- "comparison"
  comparison$token[2] <- comparison$lemma[2] <- "mucho"
  comparison$upos[2] <- comparison$xpos[2] <- "ADV"
  separated <- phrase
  separated$doc_id <- "separated"
  separated$sentence_id[4:5] <- 2L
  tokens <- rbind(phrase, comparison, separated)
  raw <- biber(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_37_if, c(1, 0, 0))
  expect_equal(raw$f_38_other_adv_sub[1], 0)
  audit <- audit_features(tokens, feature = "f_37_if", window = 1)
  expect_equal(audit$doc_id, "phrase")
  expect_equal(audit$keyword, "a menos que")
  expect_equal(audit$left, "sale")
  expect_equal(audit$right, "llueva")
})

test_that("Spanish-GSD preserves the validated clause contrasts", {
  model <- .fc_ud()
  texts <- c(
    condition = "Si llueve, dir\u00e9 la verdad.",
    indirect = "Pregunt\u00f3 si vendr\u00eda a la fiesta.",
    complement = "Insisti\u00f3 en que termin\u00e1ramos.",
    modifier = "El art\u00edculo publicado ayer es interesante.",
    se_perfect = "Se han publicado los informes."
  )
  tokens <- udpipe::udpipe_annotate(model, x = texts, doc_id = names(texts))
  raw <- biber(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$doc_id, names(texts))
  expect_equal(raw$f_37_if, c(1, 0, 0, 0, 0))
  expect_equal(raw$f_33_pied_piping, rep(0, 5))
  expect_equal(raw$f_26_past_participle[4], 0)
  expect_equal(raw$f_27_past_participle_whiz[4], 1)
  expect_equal(raw$f_17_agentless_passives[5], 1)
  expect_equal(raw$f_02_perfect_aspect[5], 1)
})
