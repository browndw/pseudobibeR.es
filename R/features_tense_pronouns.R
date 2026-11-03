# features_tense_pronouns.R
# Tense, aspect, pronoun, and adverbial features for Spanish (f_01-f_13)
#
# LINGUISTIC NOTE -- pro-drop:
#   Spanish is a null-subject language. Explicit personal pronouns
#   are markedly informative (contrast, emphasis, disambiguation).
#   We count ONLY explicit pronouns; null subjects are not counted.
#
# NOTE -- perfect aspect (f_02):
#   HABER + participle = compound perfect (he llegado).
#   In a flat UD auxiliary chain, the first nonfinite auxiliary after HABER
#   determines the construction; the lexical head is the fallback complement.
#
# UD FIELDS REQUIRED IN tokens:
#   doc_id, sentence_id, token_id_int, head_token_id_int,
#   token, lemma, pos (UPOS), dep_rel, feats,
#   morph_tense, morph_mood, morph_verbform, morph_voice, morph_person,
#   morph_number

# -----------------------------------------------------------------------------
# 0.  Internal helpers
# -----------------------------------------------------------------------------

# Extracts a specific morphological feature from the `feats` column (UD format).
# E.g.: extract_feat("Tense=Past|VerbForm=Fin", "Tense") -> "Past"
# Safe with NAs: uses stringr::str_match(), which returns NA for non-matches,
# always with the same length as feats_vec.
extract_feat <- function(feats_vec, feat_name) {
  pattern <- paste0("(?:^|\\|)", feat_name, "=([^|]+)")
  m <- stringr::str_match(dplyr::coalesce(feats_vec, ""), pattern)
  m[, 2L]
}

nonfinite_complements <- function(tokens, governors) {
  nonfinite <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("AUX", "VERB"),
      .data$morph_verbform %in% c("Inf", "Ger", "Part")
    )
  complements <- dplyr::bind_rows(
    nonfinite %>% dplyr::transmute(
      .data$doc_id, .data$sentence_id,
      chain_head_id = .data$token_id_int,
      complement_id = .data$token_id_int,
      complement_form = .data$morph_verbform
    ),
    nonfinite %>%
      dplyr::filter(
        stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^aux") |
          dplyr::coalesce(.data$dep_rel, "") == "cop"
      ) %>%
      dplyr::transmute(
        .data$doc_id, .data$sentence_id,
        chain_head_id = .data$head_token_id_int,
        complement_id = .data$token_id_int,
        complement_form = .data$morph_verbform
      )
  )
  governors %>%
    dplyr::inner_join(
      complements,
      by = c("doc_id", "sentence_id", "complement_head_id" = "chain_head_id"),
      relationship = "many-to-many"
    ) %>%
    dplyr::filter(
      .data$complement_id > .data$token_id_int |
        .data$complement_id == .data$complement_head_id
    ) %>%
    # Flat UD chains share a lexical head; choose the next nonfinite auxiliary.
    dplyr::mutate(complement_order = dplyr::if_else(
      .data$complement_id == .data$complement_head_id,
      Inf, as.numeric(.data$complement_id)
    )) %>%
    dplyr::group_by(.data$doc_id, .data$sentence_id, .data$token_id_int) %>%
    dplyr::slice_min(.data$complement_order, n = 1L, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(-dplyr::all_of("complement_order"))
}

auxiliary_complements <- function(tokens) {
  governors <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("AUX", "VERB"),
      stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^aux"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::mutate(complement_head_id = .data$head_token_id_int)
  nonfinite_complements(tokens, governors)
}

# Phase 5: count_feature() (which returned a data.frame of counts) was removed
# after the Phase 2 migration. Every production call now goes through
# count_feature_traced() in R/evidence_helpers.R, which returns
# list(counts, evidence) with identical MWT-safe dedup logic.

# -----------------------------------------------------------------------------
# 1.  block_tense_es
#     f_01  simple past (preterite)      f_02  perfect aspect
#     f_03  present tense                f_04  place adverbials
#     f_05  time adverbials              f_11  indefinite pronouns
#     f_71  imperfect past (Spanish extension)
#     f_12  REMOVED (untranslatable): the pro-verb "do" does not exist in Spanish
# -----------------------------------------------------------------------------

#' Tense, aspect, adverbial, and indefinite-pronoun features (Spanish)
#'
#' @param tokens Annotated token data frame (UD format)
#' @param doc_ids One-column data frame with column `doc_id`
#' @param place_adverbials Character vector of place-adverbial lemmas (f_04)
#' @param time_adverbials  Character vector of time-adverbial lemmas (f_05)
#' @param indefinite_pronouns Character vector of indefinite pronoun lemmas (f_11)
#' @return Data frame: one row per doc, columns f_01-f_05, f_11, f_71
#' @keywords internal
block_tense_es <- function(
    tokens,
    doc_ids,
    place_adverbials,
    time_adverbials,
    indefinite_pronouns
) {

  # Phase 2b migration: each feature is computed via count_feature_traced(),
  # which returns list(counts, evidence). The block aggregates counts via
  # left_join (as before) and stacks evidence via bind_evidence; it returns
  # a block_result so that parse_biber_features() can collect
  # both dimensions when traced=TRUE.

  # -- f_01  Past tenses of the indicative and subjunctive -------------------
  # HERNAN REVIEW (Phase 1): the English "past tense" spans several Spanish
  # tenses and aspects in the indicative AND the subjunctive. f_01 counts every
  # finite verb with Tense in {Past, Imp, Pqp}, WITHOUT filtering by Mood:
  #   - simple perfect ind. (corri)       -> Tense=Past
  #   - imperfect ind. (corria)           -> Tense=Imp
  #   - imperfect subj. (corriera)        -> Tense=Imp
  #   - pluperfect ind. (habia ...)       -> aux 'habia' with Tense=Imp
  #   - pluperfect subj. (hubiera ...)    -> aux 'hubiera' with Tense=Imp
  # UD NOTE (verified, test-udpipe-tag-verification.R): spanish-gsd does NOT emit
  # Tense=Pqp; the pluperfect comes out with Tense=Imp on the auxiliary. "Pqp"
  # is kept for robustness with other models, but it is a dead branch with spanish-gsd.
  # VerbForm=Fin is kept on purpose: it excludes the participle (corrido, which
  # carries Tense=Past|VerbForm=Part) so the compound is not counted twice here.
  # DELIBERATE overlap with f_02: the auxiliary 'habia'/'hubiera' counts in
  # f_01 and also in f_02 (haber+participle), replicating English "had written".
  # See INSTRUCCIONES_CLAUDE_CODE_pseudobiber_es.md, Phase 1 / f_01.
  f01 <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "AUX"),
      dplyr::coalesce(extract_feat(.data$feats, "Tense"), "") %in% c("Past", "Imp", "Pqp"),
      dplyr::coalesce(extract_feat(.data$feats, "VerbForm"), "") == "Fin"
    ) %>%
    count_feature_traced("f_01_past_tense")

  # -- f_71  Imperfect past (Spanish extension) -----------------------------
  # Tense=Imp, Mood=Ind, VerbForm=Fin (caminaba, decia, era).
  # It does not exist in Biber's (1985) original catalog.
  # Kept as an extended Spanish feature; see biber_espanol_completo.md.
  # (NOTE: f_71 is removed from the final output in parse_biber_features; its
  #  evidence is filtered out there automatically since the name does not survive
  #  as a column of the final counts.)
  f71 <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "AUX"),
      dplyr::coalesce(extract_feat(.data$feats, "Tense"), "") == "Imp",
      dplyr::coalesce(extract_feat(.data$feats, "Mood"),  "") == "Ind",
      dplyr::coalesce(extract_feat(.data$feats, "VerbForm"), "") == "Fin"
    ) %>%
    count_feature_traced("f_71_preterit")

  # -- f_02  Perfect aspect: HABER + participle ------------------------------
  f02 <- auxiliary_complements(tokens) %>%
    dplyr::filter(.data$lemma == "haber", .data$complement_form == "Part") %>%
    count_feature_traced("f_02_perfect_aspect")

  # -- f_03  Present tense ---------------------------------------------------
  # Simple present indicative (Tense=Pres, Mood=Ind, VerbForm=Fin).
  # Does not include the present subjunctive (Mood=Sub) or the non-finite
  # forms (infinitive, gerund, participle).
  f03 <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "AUX"),
      dplyr::coalesce(extract_feat(.data$feats, "Tense"),    "") == "Pres",
      dplyr::coalesce(extract_feat(.data$feats, "Mood"),     "") == "Ind",
      dplyr::coalesce(extract_feat(.data$feats, "VerbForm"), "") == "Fin"
    ) %>%
    count_feature_traced("f_03_present_tense")

  # -- f_04  Place adverbials ------------------------------------------------
  # Lemma matching against a word list; POS = ADV or ADP.
  f04 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% place_adverbials,
      .data$pos   %in% c("ADV", "ADP", "NOUN")
    ) %>%
    count_feature_traced("f_04_place_adverbials")

  # -- f_05  Time adverbials -------------------------------------------------
  f05 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% time_adverbials,
      .data$pos   %in% c("ADV", "NOUN", "ADP")
    ) %>%
    count_feature_traced("f_05_time_adverbials")

  # -- f_11  Indefinite pronouns ---------------------------------------------
  # biber_espanol_completo.md §f_11 (critical EXCLUDE): un/una/unos/unas are
  # articles (DET, PronType=Art), not pronouns. UDPipe lemmatizes them as
  # "uno"; the PronType!=Art filter discards the article and keeps the
  # pronominal use (uno PRON PronType=Ind).
  # NOTE: count_feature_traced uses the name "f_11_indefinite_pronoun"
  # (without the final 's') because parse_biber_features() renames it to
  # f_11_indefinite_pronouns later. We do the same here to
  # keep parity. The evidence is labeled with the same name and
  # relabeled in parse_biber_features after the rename.
  # HERNAN REVIEW (Phase 4): syntactic control. Only the INDEPENDENT nominal
  # (pronominal) function counts. The DETERMINER use is excluded
  # (dep_rel=det: 'todo el dia', 'cualquier libro', 'algun caso'), which is not
  # pronominal. The adjectival use ('un libro cualquiera' -> ADJ/amod) is already left out
  # by the POS filter. Documented LIMIT: the model mis-tags degree adverb 'algo'
  # ('esta algo cansado') as PRON|nsubj, identical to the
  # pronominal use ('algo fallo') -> cannot be separated, not forced.
  f11 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% indefinite_pronouns,
      .data$pos   %in% c("PRON", "DET"),
      !stringr::str_detect(dplyr::coalesce(.data$feats, ""), "PronType=Art"),
      !stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^det($|:)")
    ) %>%
    count_feature_traced("f_11_indefinite_pronoun")

  # f_12 (pro-verb do): UNTRANSLATABLE in Spanish.
  # Spanish resolves verbal anaphora by ellipsis, with no pro-verb
  # equivalent to "do". See biber_espanol_completo.md sec. F_12.
  # Column removed from the output since the Phase 2 audit.

  # -- Assemble counts -------------------------------------------------------
  counts <- doc_ids %>%
    dplyr::left_join(f01$counts, by = "doc_id") %>%
    dplyr::left_join(f71$counts, by = "doc_id") %>%
    dplyr::left_join(f02$counts, by = "doc_id") %>%
    dplyr::left_join(f03$counts, by = "doc_id") %>%
    dplyr::left_join(f04$counts, by = "doc_id") %>%
    dplyr::left_join(f05$counts, by = "doc_id") %>%
    dplyr::left_join(f11$counts, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )

  # -- Assemble evidence -----------------------------------------------------
  evidence <- bind_evidence(
    f01$evidence, f71$evidence, f02$evidence, f03$evidence,
    f04$evidence, f05$evidence, f11$evidence
  )

  make_block_result(counts = counts, evidence = evidence)
}

# -----------------------------------------------------------------------------
# 2.  block_personal_pronouns_es
#     f_06  1st person   f_07  2nd person   f_08  3rd person
#     f_13  wh-question
#     f_09  REMOVED (untranslatable): Spanish is a null-subject language, there is no expletive "it"
# -----------------------------------------------------------------------------

#' Personal pronoun, expletive, and WH-question features (Spanish)
#'
#' @param tokens Annotated token data frame
#' @param doc_ids One-column data frame with column `doc_id`
#' @param wh_question_lemmas WH-word lemmas (default provided)
#' @return Data frame: one row per doc, columns f_06-f_08, f_13
#' @keywords internal
block_personal_pronouns_es <- function(
    tokens,
    doc_ids,
    wh_question_lemmas = c(
      "quien",  "qui\u00e9n",
      "que",    "qu\u00e9",
      "cual",   "cu\u00e1l",  "cuales", "cu\u00e1les",
      "donde",  "d\u00f3nde",
      "cuando", "cu\u00e1ndo",
      "como",   "c\u00f3mo",
      "cuanto", "cu\u00e1nto",  "cuanta", "cu\u00e1nta",
      "cuantos","cu\u00e1ntos", "cuantas","cu\u00e1ntas",
      "por_que","por_qu\u00e9"
    )) {

  # dep_rel values that signal reflexive/impersonal use -- excluded in f_06-f_08
  reflexive_deps <- c("expl:pv", "expl:impers", "expl")

  # Helper: filter the pronouns of one person by a list of lemmas.
  # In UDPipe Spanish-GSD, "se" is lemmatized as "el" with Reflex=Yes and
  # dep_rel = "iobj"/"obj" (not expl:pass). We exclude those cases so that
  # passive/impersonal/reflexive "se" does NOT count as a referential pronoun.
  # Phase 2c: it only filters (without collapsing columns); dedup+tally+evidence
  # are done by count_feature_traced below.
  filter_person_pronouns <- function(lemma_list) {
    tokens %>%
      dplyr::filter(
        .data$pos == "PRON",
        .data$lemma %in% lemma_list | .data$token %in% lemma_list,
        !dplyr::coalesce(.data$dep_rel, "") %in% reflexive_deps,
        !(tolower(.data$token) == "se" &
          stringr::str_detect(dplyr::coalesce(.data$feats, ""), "Reflex=Yes"))
      )
  }

  # -- Possessives: routed by morphological Person (HERNAN REVIEW, Phase 1) ---
  # The English feature includes possessives (our/your/their). spanish-gsd
  # tags them as DET with Poss=Yes and a reliable Person (verified in
  # test-udpipe-tag-verification.R): mi/nuestro -> Person=1, tu/vuestro ->
  # Person=2, su -> Person=3. The 'su' of 'usted' agrees in the 3rd person and is assigned to
  # f_08 by a morphological criterion (documented). Nominalized stressed forms
  # (mio, tuyo, suyo...) come out as NOUN WITHOUT Poss=Yes and are captured by lemma.
  # No double counting: count_feature_traced dedups by position and the lexical
  # pass excludes Poss=Yes.
  possessive_by_person <- function(person) {
    tokens %>%
      dplyr::filter(
        stringr::str_detect(dplyr::coalesce(.data$feats, ""), "Poss=Yes"),
        dplyr::coalesce(extract_feat(.data$feats, "Person"), "") == person
      )
  }
  possessive_lexical <- function(lemma_list) {
    tokens %>%
      dplyr::filter(
        .data$lemma %in% lemma_list,
        !stringr::str_detect(dplyr::coalesce(.data$feats, ""), "Poss=Yes"),
        .data$pos %in% c("NOUN", "PRON", "ADJ")
      )
  }
  # Voseo: spanish-gsd often tags "vos" as NOUN (lemma "tu"), losing it
  # in the pronominal pass (pos==PRON). Recovered by token; the dedup avoids
  # double counting when it does come as PRON.
  voseo_forms <- function() {
    tokens %>% dplyr::filter(tolower(.data$token) == "vos")
  }

  # -- f_06  1st person (pronouns + possessives) ----------------------------
  f06 <- dplyr::bind_rows(
    filter_person_pronouns(c("yo", "nosotros", "nosotras", "me", "nos",
                             "m\u00ed", "conmigo")),
    possessive_by_person("1"),
    possessive_lexical(c("m\u00edo", "nuestro"))
  ) %>% count_feature_traced("f_06_first_person_pronouns")

  # -- f_07  2nd person (pronouns + possessives + voseo) --------------------
  # "te" can be an unstressed 2nd person or part of an impersonal construction;
  # the reflexive_deps exclusion filters out the clearest expl cases.
  f07 <- dplyr::bind_rows(
    filter_person_pronouns(c("t\u00fa", "vos", "vosotros", "vosotras",
                             "usted", "ustedes", "te", "ti", "contigo", "os")),
    possessive_by_person("2"),
    possessive_lexical(c("tuyo", "vuestro")),
    voseo_forms()
  ) %>% count_feature_traced("f_07_second_person_pronouns")

  # -- f_08  3rd person (pronouns + possessives) ----------------------------
  f08 <- dplyr::bind_rows(
    filter_person_pronouns(c("\u00e9l", "ella", "ello", "ellos", "ellas",
                             "le", "lo", "la", "les", "los", "las",
                             "consigo", "s\u00ed")),
    possessive_by_person("3"),
    possessive_lexical(c("suyo"))
  ) %>% count_feature_traced("f_08_third_person_pronouns")

  # f_09 (pronoun it): UNTRANSLATABLE in Spanish.
  # Spanish is a null-subject language; there is no expletive equivalent to "it".
  # Impersonal haber (hay, habia...) is captured in f_20.
  # See biber_espanol_completo.md sec. F_09.
  # Column removed from the output since the Phase 2 audit.

  # -- f_13  Questions with an interrogative word ---------------------------
  question_sentences <- tokens %>%
    dplyr::filter(.data$token == "?") %>%
    dplyr::transmute(.data$doc_id, .data$sentence_id,
                     has_question = TRUE) %>%
    dplyr::distinct()

  f13 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% wh_question_lemmas,
      .data$pos   %in% c("ADV", "PRON", "DET", "ADJ")
    ) %>%
    dplyr::left_join(question_sentences,
                     by = c("doc_id", "sentence_id")) %>%
    dplyr::filter(!is.na(.data$has_question)) %>%
    count_feature_traced("f_13_wh_question")

  # -- Assemble counts -------------------------------------------------------
  counts <- doc_ids %>%
    dplyr::left_join(f06$counts, by = "doc_id") %>%
    dplyr::left_join(f07$counts, by = "doc_id") %>%
    dplyr::left_join(f08$counts, by = "doc_id") %>%
    dplyr::left_join(f13$counts, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"),
                    ~ dplyr::coalesce(., 0L))
    )

  # -- Assemble evidence -----------------------------------------------------
  evidence <- bind_evidence(f06$evidence, f07$evidence, f08$evidence, f13$evidence)

  make_block_result(counts = counts, evidence = evidence)
}
