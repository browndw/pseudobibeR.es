.expect_spacy_pipeline <- function(tokens) {
  raw <- biber_fixture(tokens, measure = "none", normalize = FALSE)
  ids <- c(
    "agentless", "agent", "perfect", "perfect_passive", "existential",
    "direct", "indirect", "modal", "necessity", "future", "progressive",
    "adjective", "negation", "reflexive", "conditional", "multi", "tokens",
    "pronominal", "split_misparse", "coordination_misparse", "wh_misparse"
  )
  expect_identical(raw$doc_id, ids)
  expect_named(raw, c("doc_id", setdiff(names(.feature_categories), "f_43_type_token")))
  positives <- list(
    f_02_perfect_aspect = c("perfect", "perfect_passive"),
    f_13_wh_question = "direct",
    f_17_agentless_passives = c("agentless", "perfect_passive", "modal", "reflexive"),
    f_18_by_passives = "agent",
    f_19_be_main_verb = c("adjective", "tokens"),
    f_20_existential_there = "existential",
    f_23_wh_clause = "indirect",
    f_37_if = "conditional",
    f_52_modal_possibility = "modal",
    f_53_modal_necessity = "necessity",
    f_54_modal_predictive = "future",
    f_56_verb_private = c("conditional", "pronominal"),
    f_66_neg_synthetic = "negation",
    f_67_neg_analytic = "negation"
  )
  for (feature in names(positives)) {
    expect_equal(raw[[feature]], as.numeric(ids %in% positives[[feature]]),
                 info = feature)
  }
  expect_equal(word_tokens(tokens), c(4, 7, 5, 5, 3, 3, 4, 5, 4, 3, 3, 4, 3, 3, 4, 4, 5, 6, 5, 5, 4))
  rate <- biber_fixture(tokens, measure = "none", normalize = TRUE)
  for (feature in .evidence_features) {
    expect_equal(rate[[feature]], 1000 * raw[[feature]] / word_tokens(tokens), info = feature)
  }
  audit <- audit_fixture(tokens, feature = .evidence_features)
  for (feature in .evidence_features) {
    occurrences <- audit[audit$feature == feature, ]
    expect_equal(
      tabulate(match(occurrences$doc_id, ids), nbins = length(ids)),
      raw[[feature]], info = feature
    )
  }
  passive <- audit[audit$feature == "f_18_by_passives", ]
  expect_equal(passive$keyword, "fue")
  expect_match(passive$right, "por el equipo", fixed = TRUE)
  pronouns <- audit[audit$feature == "f_06_first_person_pronouns" & audit$doc_id == "multi", ]
  expect_equal(pronouns$keyword, c("Yo", "Nosotros"))
  expect_equal(pronouns$sentence_id, c(1L, 2L))
  expect_equal(raw$f_03_present_tense[raw$doc_id == "multi"], 2)
  invisible(raw)
}

test_that("frozen native Spanish spaCy annotations support extraction and auditing offline", {
  fixture <- readRDS(test_path("fixtures", "spanish-spacy-regressions.rds"))
  expect_identical(fixture$model, "es_core_news_sm")
  expect_identical(fixture$model_version, "3.8.0")
  expect_s3_class(fixture$tokens, "spacyr_parsed")
  expect_type(fixture$tokens$morph, "character")
  raw <- .expect_spacy_pipeline(fixture$tokens)
  plain <- fixture$tokens
  class(plain) <- "data.frame"
  expect_equal(biber_fixture(plain, measure = "none", normalize = FALSE), raw)
  individual <- lapply(unique(plain$doc_id), function(id) {
    biber_fixture(plain[plain$doc_id == id, ], measure = "none", normalize = FALSE)
  })
  expect_equal(dplyr::bind_rows(individual), raw, ignore_attr = TRUE)
  parsed <- prepare_parsed_tokens(fixture$tokens)
  expect_false(any(parsed$dep_rel == "ROOT"))
  expect_true(any(parsed$dep_rel == "root"))
  expect_equal(parsed$morph_verbform[parsed$token == "he"], "Fin")
  expect_equal(parsed$morph_verbform[parsed$token == "terminado" & parsed$pos == "VERB"], "Part")
  no_morph <- fixture$tokens
  no_morph$morph <- NULL
  expect_error(biber_fixture(no_morph), 'additional_attributes = "morph"', fixed = TRUE)
  expect_error(audit_fixture(no_morph, feature = "f_02_perfect_aspect"), "Missing morphology")
})

test_that("Spanish spaCy model errors are not silently rewritten", {
  fixture <- readRDS(test_path("fixtures", "spanish-spacy-regressions.rds"))
  tokens <- fixture$tokens
  raw <- biber_fixture(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_63_split_auxiliary[raw$doc_id == "split_misparse"], 0)
  expect_equal(raw$f_52_modal_possibility[raw$doc_id == "split_misparse"], 0)
  expect_equal(raw$f_65_clausal_coordination[raw$doc_id == "coordination_misparse"], 0)
  expect_equal(raw$f_23_wh_clause[raw$doc_id == "wh_misparse"], 0)
  split <- tokens[tokens$doc_id == "split_misparse", ]
  expect_equal(split$head_token_id[split$token == "Puede"], 2)
  split$head_token_id[split$token == "Puede"] <- 3
  expect_equal(biber_fixture(split, measure = "none", normalize = FALSE)$f_63_split_auxiliary, 1)
  expect_equal(biber_fixture(split, measure = "none", normalize = FALSE)$f_52_modal_possibility, 1)
  clitic <- tokens[tokens$doc_id == "tokens", ]
  expect_equal(clitic$lemma[clitic$token == "verlo"], "ver \u00e9l")
  expect_equal(raw$f_56_verb_private[raw$doc_id == "tokens"], 0)
  indirect <- tokens[tokens$doc_id == "wh_misparse", ]
  expect_equal(indirect$pos[indirect$token == "d\u00f3nde"], "PRON")
  expect_equal(indirect$morph[indirect$token == "d\u00f3nde"], "PronType=Ind")
  indirect$pos[indirect$token == "d\u00f3nde"] <- "ADV"
  expect_equal(biber_fixture(indirect, measure = "none", normalize = FALSE)$f_23_wh_clause, 1)
})

test_that("spaCy passive conventions stay restricted to ser and nominal por-agents", {
  fixture <- readRDS(test_path("fixtures", "spanish-spacy-regressions.rds"))
  agent <- fixture$tokens[fixture$tokens$doc_id == "agent", ]
  missing_por <- agent
  missing_por$lemma[missing_por$token == "por"] <- "para"
  nominal_modifier <- agent
  nominal_modifier$dep_rel[nominal_modifier$token == "equipo"] <- "nmod"
  other_predicate <- agent
  other_predicate$head_token_id[other_predicate$token == "equipo"] <- 2
  verbal <- agent
  verbal$pos[verbal$token == "equipo"] <- "VERB"
  for (tokens in list(missing_por, nominal_modifier, other_predicate, verbal)) {
    raw <- biber_fixture(tokens, measure = "none", normalize = FALSE)
    expect_equal(raw$f_17_agentless_passives, 1)
    expect_equal(raw$f_18_by_passives, 0)
  }
  progressive <- fixture$tokens[fixture$tokens$doc_id == "progressive", ]
  stative <- agent
  stative$lemma[stative$token == "fue"] <- "estar"
  for (tokens in list(progressive, stative)) {
    raw <- biber_fixture(tokens, measure = "none", normalize = FALSE)
    expect_equal(raw$f_17_agentless_passives + raw$f_18_by_passives, 0)
  }
  ud <- dplyr::rename(agent, upos = "pos", xpos = "tag", feats = "morph")
  class(ud) <- "data.frame"
  expect_equal(biber_fixture(ud, measure = "none", normalize = FALSE)$f_18_by_passives, 0)
  ud$dep_rel[ud$token == "fue"] <- "aux:pass"
  raw_ud <- biber_fixture(ud, measure = "none", normalize = FALSE)
  expect_equal(raw_ud$f_17_agentless_passives, 1)
  expect_equal(raw_ud$f_18_by_passives, 0)
})

test_that("live Spanish spaCy morphology passes through spacyr into the common pipeline", {
  model <- Sys.getenv("PSEUDOBIBER_ES_SPACY_MODEL")
  skip_if(!nzchar(model), "Set PSEUDOBIBER_ES_SPACY_MODEL to enable live spaCy tests")
  skip_if_not_installed("spacyr")
  skip_if_not_installed("reticulate")
  python <- Sys.getenv("RETICULATE_PYTHON")
  if (!nzchar(python) || !file.exists(python)) {
    stop("Set RETICULATE_PYTHON to the Python executable in r-spacyr.")
  }
  fixture <- readRDS(test_path("fixtures", "spanish-spacy-regressions.rds"))
  spacyr::spacy_initialize(model = model)
  on.exit(spacyr::spacy_finalize(), add = TRUE)
  expect_equal(normalizePath(reticulate::py_config()$python), normalizePath(python))
  tokens <- withCallingHandlers(
    spacyr::spacy_parse(
      fixture$texts, dependency = TRUE, tag = TRUE, pos = TRUE, entity = FALSE,
      additional_attributes = "morph"
    ),
    warning = function(w) {
      if (startsWith(conditionMessage(w), "lemmatization may not work properly in model")) {
        invokeRestart("muffleWarning")
      }
    }
  )
  expect_s3_class(tokens, "spacyr_parsed")
  expect_true(any(vapply(tokens$morph, inherits, logical(1), "python.builtin.object")))
  raw <- .expect_spacy_pipeline(tokens)
  serialized <- tokens
  serialized$morph <- vapply(tokens$morph, as.character, character(1))
  expect_equal(biber_fixture(serialized, measure = "none", normalize = FALSE), raw)
  ttr <- biber_fixture(tokens, measure = "TTR", normalize = FALSE)
  expect_true("f_43_type_token" %in% names(ttr))
  expect_true(all(is.finite(ttr$f_43_type_token)))
})
