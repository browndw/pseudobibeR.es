test_that("se passives include compound tenses but exclude explicit reflexive labels", {
  simple <- contract_clause(
    "simple", c("se", "publican", "informes"), c("se", "publicar", "informe"),
    c("PRON", "VERB", "NOUN"), c(2, 0, 2), c("expl:pass", "root", "nsubj"),
    c("Reflex=Yes", "VerbForm=Fin|Person=3|Number=Plur", "Number=Plur")
  )
  compound <- contract_clause(
    "compound", c("se", "han", "publicado", "informes"),
    c("se", "haber", "publicar", "informe"), c("PRON", "AUX", "VERB", "NOUN"),
    c(3, 3, 0, 3), c("expl:pass", "aux", "root", "nsubj"),
    c("Reflex=Yes", "VerbForm=Fin|Person=3|Number=Plur", "VerbForm=Part", "Number=Plur")
  )
  reflexive <- simple
  reflexive$doc_id <- "reflexive"
  reflexive$dep_rel[1] <- "expl:pv"
  impersonal <- simple
  impersonal$doc_id <- "impersonal"
  impersonal$dep_rel[3] <- "obj"
  tokens <- rbind(simple, compound, reflexive, impersonal)
  raw <- biber_fixture(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_17_agentless_passives, c(1, 1, 0, 0))
  expect_equal(raw$f_18_by_passives, rep(0, 4))
  expect_equal(raw$f_02_perfect_aspect, c(0, 1, 0, 0))
  audit <- audit_fixture(tokens, feature = "f_17_agentless_passives")
  expect_setequal(audit$doc_id, c("simple", "compound"))
  expect_equal(audit$keyword, c("se", "se"))
  expect_equal(biber_fixture(tokens, measure = "none")$f_17_agentless_passives,
               1000 * c(1, 1, 0, 0) / word_tokens(tokens))
  spacy <- dplyr::rename(tokens, pos = "upos", tag = "xpos", morph = "feats")
  class(spacy) <- c("spacyr_parsed", "data.frame")
  expect_equal(biber_fixture(spacy, measure = "none", normalize = FALSE), raw)
})

test_that("passive agents require a por case phrase attached to the passive predicate", {
  agent <- contract_clause(
    "agent", c("fue", "revisado", "por", "Ana"), c("ser", "revisar", "por", "Ana"),
    c("AUX", "VERB", "ADP", "PROPN"), c(2, 0, 4, 2),
    c("aux:pass", "root", "case", "obl:agent"),
    c("VerbForm=Fin|Person=3", "VerbForm=Part", NA, NA)
  )
  nominal <- agent
  nominal$doc_id <- "nominal_modifier"
  nominal$dep_rel[4] <- "nmod"
  subordinate <- agent
  subordinate$doc_id <- "subordinate"
  subordinate$upos[4] <- subordinate$xpos[4] <- "VERB"
  subordinate$token[4] <- subordinate$lemma[4] <- "insistir"
  subordinate$feats[4] <- "VerbForm=Inf"
  subordinate$dep_rel[3:4] <- c("mark", "advcl")
  distant <- agent
  distant$doc_id <- "other_sentence"
  distant$sentence_id[3:4] <- 2L
  tokens <- rbind(agent, nominal, subordinate, distant)
  raw <- biber_fixture(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_18_by_passives, c(1, 0, 0, 0))
  expect_equal(raw$f_17_agentless_passives, c(0, 1, 1, 1))
  for (feature in c("f_17_agentless_passives", "f_18_by_passives")) {
    audit <- audit_fixture(tokens, feature = feature)
    expect_equal(tabulate(match(audit$doc_id, raw$doc_id), nbins = nrow(raw)),
                 raw[[feature]])
  }
})

test_that("existential haber excludes haber que with a dependent infinitive", {
  existential <- contract_clause(
    "existential", c("hay", "informes"), c("haber", "informe"),
    c("VERB", "NOUN"), c(0, 1), c("root", "obj"),
    c("VerbForm=Fin|Person=3|Tense=Pres|Mood=Ind", NA)
  )
  obligation <- contract_clause(
    "obligation", c("hay", "que", "revisar"), c("haber", "que", "revisar"),
    c("VERB", "SCONJ", "VERB"), c(0, 3, 1), c("root", "mark", "xcomp"),
    c("VerbForm=Fin|Person=3|Tense=Pres|Mood=Ind", NA, "VerbForm=Inf")
  )
  tokens <- rbind(existential, obligation)
  raw <- biber_fixture(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_20_existential_there, c(1, 0))
  expect_equal(raw$f_53_modal_necessity, c(0, 1))
  expect_equal(audit_fixture(tokens, feature = "f_20_existential_there")$doc_id,
               "existential")
})

test_that("existential haber covers perfect, infinitival and accented forms only", {
  perfect <- contract_clause(
    "perfect", c("ha", "habido", "problemas"), c("haber", "haber", "problema"),
    c("AUX", "VERB", "NOUN"), c(2, 0, 2), c("aux", "root", "obj"),
    c("VerbForm=Fin|Person=3|Tense=Pres|Mood=Ind", "VerbForm=Part|Tense=Past", NA)
  )
  infinitival <- contract_clause(
    "infinitival", c("puede", "haber", "errores"), c("poder", "haber", "error"),
    c("VERB", "VERB", "NOUN"), c(0, 1, 2), c("root", "csubj", "obj"),
    c("VerbForm=Fin|Person=3|Tense=Pres|Mood=Ind", "VerbForm=Inf", NA)
  )
  accented <- contract_clause(
    "accented", c("Había", "personas"), c("haber", "persona"),
    c("VERB", "NOUN"), c(0, 1), c("root", "obj"),
    c("VerbForm=Fin|Person=3|Tense=Imp|Mood=Ind", NA)
  )
  auxiliary <- contract_clause(
    "auxiliary", c("creo", "haber", "entendido"), c("creer", "haber", "entender"),
    c("VERB", "AUX", "VERB"), c(0, 3, 1), c("root", "aux", "xcomp"),
    c("VerbForm=Fin|Person=1|Tense=Pres|Mood=Ind", "VerbForm=Inf", "VerbForm=Part")
  )
  tokens <- rbind(perfect, infinitival, accented, auxiliary)
  raw <- biber_fixture(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_20_existential_there, c(1, 1, 1, 0))
  audit <- audit_fixture(tokens, feature = "f_20_existential_there")
  expect_equal(audit$keyword, c("habido", "haber", "Había"))
})
