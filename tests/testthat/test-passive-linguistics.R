# Fixtures of each test below. All doc_ids are distinct, so the cases run as
# one corpus (counts and audits are per document): each pipeline call has a
# large fixed cost.
passive_cases <- list(
  se_passive = local({
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
    rbind(simple, compound, reflexive, impersonal)
  }),
  agent = local({
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
    rbind(agent, nominal, subordinate, distant)
  }),
  existential = local({
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
    rbind(existential, obligation)
  }),
  haber_forms = local({
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
    rbind(perfect, infinitival, accented, auxiliary)
  })
)

passive_batch <- local({
  batch <- NULL
  function() {
    if (is.null(batch)) {
      tokens <- do.call(rbind, unname(passive_cases))
      batch <<- list(
        raw = biber_fixture(tokens, measure = "none", normalize = FALSE),
        audit = audit_by_feature(tokens, c("f_17_agentless_passives", "f_18_by_passives",
                                           "f_20_existential_there"))
      )
    }
    batch
  }
})

# Raw counts and audits restricted to one case, in its document order.
passive_raw <- function(case) {
  raw <- passive_batch()$raw
  raw <- raw[raw$doc_id %in% passive_cases[[case]]$doc_id, ]
  rownames(raw) <- NULL
  raw
}

passive_audit <- function(case, feature) {
  audit <- passive_batch()$audit[[feature]]
  audit[audit$doc_id %in% passive_cases[[case]]$doc_id, ]
}

test_that("se passives include compound tenses but exclude explicit reflexive labels", {
  tokens <- passive_cases$se_passive
  raw <- passive_raw("se_passive")
  expect_equal(raw$f_17_agentless_passives, c(1, 1, 0, 0))
  expect_equal(raw$f_18_by_passives, rep(0, 4))
  expect_equal(raw$f_02_perfect_aspect, c(0, 1, 0, 0))
  audit <- passive_audit("se_passive", "f_17_agentless_passives")
  expect_setequal(audit$doc_id, c("simple", "compound"))
  expect_equal(audit$keyword, c("se", "se"))
  expect_equal(biber_fixture(tokens, measure = "none")$f_17_agentless_passives,
               1000 * c(1, 1, 0, 0) / word_tokens(tokens))
  spacy <- dplyr::rename(tokens, pos = "upos", tag = "xpos", morph = "feats")
  class(spacy) <- c("spacyr_parsed", "data.frame")
  expect_equal(biber_fixture(spacy, measure = "none", normalize = FALSE), raw)
})

test_that("passive agents require a por case phrase attached to the passive predicate", {
  raw <- passive_raw("agent")
  expect_equal(raw$f_18_by_passives, c(1, 0, 0, 0))
  expect_equal(raw$f_17_agentless_passives, c(0, 1, 1, 1))
  for (feature in c("f_17_agentless_passives", "f_18_by_passives")) {
    audit <- passive_audit("agent", feature)
    expect_equal(tabulate(match(audit$doc_id, raw$doc_id), nbins = nrow(raw)),
                 raw[[feature]])
  }
})

test_that("existential haber excludes haber que with a dependent infinitive", {
  raw <- passive_raw("existential")
  expect_equal(raw$f_20_existential_there, c(1, 0))
  expect_equal(raw$f_53_modal_necessity, c(0, 1))
  expect_equal(passive_audit("existential", "f_20_existential_there")$doc_id,
               "existential")
})

test_that("existential haber covers perfect, infinitival and accented forms only", {
  raw <- passive_raw("haber_forms")
  expect_equal(raw$f_20_existential_there, c(1, 1, 1, 0))
  audit <- passive_audit("haber_forms", "f_20_existential_there")
  expect_equal(audit$keyword, c("habido", "haber", "Había"))
})
