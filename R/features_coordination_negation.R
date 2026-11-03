# features_coordination_negation.R
# Coordination, split structures, negation, and lexical features (Spanish)
#
# FEATURES PRODUCED: f_10, f_14, f_16, f_51 (nouns/demonstratives),
#                    f_63-f_65 (split aux, coordination),
#                    f_66-f_67 (negation)
#
# REMOVED FEATURES (untranslatable):
#   f_59 (contractions): morphology that does not exist in standard Spanish
#   f_61 (stranded prepositions): ungrammatical in Spanish
#   f_62 (split infinitives): requires a preverbal marker "to" that does not exist
#   f_15 (gerunds as nouns): the Spanish gerund is not nominal
# See biber_espanol_completo.md for the full justification.
#
# LINGUISTIC NOTE -- Spanish negation:
#
#   f_66 counts configured negative words, including negative concord with no.
#   f_67 counts no as ADV/PART attached to a verb, adjective or adverb.
#   Negative words and no intentionally remain separate occurrences.

# -----------------------------------------------------------------------------
# Features f_59 (contractions), f_61 (stranded preposition) and f_62 (split
# infinitive) REMOVED as untranslatable (biber_espanol_completo.md §F_59,
# §F_61, §F_62). There used to be stubs block_contractions_es /
# block_stranded_split_es that returned doc_ids without columns; removed in
# the Phase 1 audit (Section C2) as vestigial code.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# 1.  block_split_coordination_es   f_63-f_65
# -----------------------------------------------------------------------------

#' Split auxiliary and coordination features (Spanish)
#'
#' f_63  Auxiliar escindido: ADV interviene entre auxiliar y verbo principal
#' f_64  Coordinacion sintagmatica: CCONJ entre sintagmas no clausales
#' f_65  Coordinacion clausal:      CCONJ entre clausulas con sujeto
#'
#' @param tokens Annotated token data frame
#' @param doc_ids One-column data frame with column `doc_id`
#' @param token_lookup Token-level attribute lookup table
#' @param subject_heads Table of clause heads that have an explicit subject
#' @param negation_part_lemmas Negation particle lemmas (no)
#' @param modal_lemmas Modal head lemmas used for infinitival periphrases
#' @return Data frame with f_63, f_64, f_65
#' @keywords internal
block_split_coordination_es <- function(tokens, doc_ids, token_lookup,
                                         subject_heads, negation_part_lemmas,
                                         modal_lemmas) {

  adv_interveners <- tokens %>%
    dplyr::filter(
      .data$pos == "ADV" |
        (.data$pos == "PART" & .data$lemma %in% negation_part_lemmas)
    ) %>%
    dplyr::transmute(
      .data$doc_id, .data$sentence_id,
      adv_tok = .data$token_id_int
    )

  aux_deps <- auxiliary_complements(tokens) %>%
    dplyr::filter(
      .data$complement_form != "Inf" | !.data$lemma %in% modal_lemmas
    )
  aux_deps <- dplyr::bind_rows(
    aux_deps, modal_periphrasis_links(tokens, modal_lemmas)
  ) %>%
    dplyr::mutate(
      span_min = pmin(.data$token_id_int, .data$complement_id),
      span_max = pmax(.data$token_id_int, .data$complement_id)
    )

  # f_63  Phase 2k: keep columns for evidence.
  f63 <- aux_deps %>%
    dplyr::left_join(adv_interveners, by = c("doc_id", "sentence_id"),
                     relationship = "many-to-many") %>%
    dplyr::filter(
      !is.na(.data$adv_tok),
      .data$adv_tok > .data$span_min,
      .data$adv_tok < .data$span_max
    ) %>%
    count_feature_traced("f_63_split_auxiliary")

  finite_verbs <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "AUX"),
      is.na(.data$morph_verbform) | !.data$morph_verbform %in% c("Inf", "Ger", "Part")
    )
  finite_predicates <- dplyr::bind_rows(
    finite_verbs %>% dplyr::select("doc_id", "sentence_id", "token_id_int"),
    finite_verbs %>%
      dplyr::filter(.data$dep_rel == "cop", .data$morph_verbform == "Fin") %>%
      dplyr::transmute(.data$doc_id, .data$sentence_id, token_id_int = .data$head_token_id_int)
  ) %>%
    dplyr::distinct()
  token_lookup <- token_lookup %>%
    dplyr::left_join(
      finite_predicates %>% dplyr::mutate(is_finite_predicate = TRUE),
      by = c("doc_id", "sentence_id", "token_id_int")
    ) %>%
    dplyr::mutate(token_family = dplyr::case_when(
      dplyr::coalesce(.data$is_finite_predicate, FALSE) |
        .data$token_pos %in% c("VERB", "AUX") ~ "verbal",
      .data$token_pos %in% c("NOUN", "PROPN") ~ "nominal",
      TRUE ~ .data$token_pos
    ))

  cc_tokens <- tokens %>%
    dplyr::filter(
      .data$pos == "CCONJ",
      dplyr::coalesce(.data$dep_rel, "") == "cc"
    ) %>%
    # attributes of the head of the CCONJ (= second conjunct)
    dplyr::left_join(
      token_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::rename(
      conj_pos            = "token_pos",
      conj_dep_rel        = "token_dep_rel",
      conj_head_id        = "token_head_token_id_int",
      conj_verbform       = "token_morph_verbform",
      conj_family         = "token_family"
    ) %>%
    # attributes of the first conjunct (head of the head)
    dplyr::left_join(
      token_lookup %>%
        dplyr::select(
          "doc_id", "sentence_id", "token_id_int",
          first_conj_pos = "token_pos", first_conj_family = "token_family"
        ),
      by = c("doc_id", "sentence_id", "conj_head_id" = "token_id_int")
    ) %>%
    # explicit subject in the clause
    dplyr::left_join(
      subject_heads,
      by = c("doc_id", "sentence_id",
             "head_token_id_int" = "clause_head_token_id_int")
    ) %>%
    dplyr::mutate(
      has_subject = dplyr::coalesce(.data$has_subject, FALSE)
    )

  # f_64  Phrasal coordination
  # HERNAN REVIEW (Phase 5): cover the FOUR categories of the English feature:
  # nouns, adjectives, adverbs AND verbs (coordination of VPs with a shared
  # subject, e.g. 'lee y escribe'). The !has_subject guard separates
  # phrasal coordination (shared VP) from clausal coordination (f_65, own
  # subjects), avoiding double counting.
  f64_std <- cc_tokens %>%
    dplyr::filter(
      stringr::str_detect(dplyr::coalesce(.data$conj_dep_rel, ""), "^conj($|:)"),
      dplyr::coalesce(.data$conj_pos,     "") %in%
        c("NOUN", "PROPN", "ADJ", "ADV", "VERB", "AUX"),
      !is.na(.data$first_conj_pos),
      .data$first_conj_family == .data$conj_family,
      !.data$has_subject
    )

  # HERNAN REVIEW v2 (finding 8.5.D): when the first adjective is an amod of
  # a noun, UDPipe sometimes attaches the SECOND adjective directly to the
  # noun (not to the first adjective): "Un procedimiento rapido, pero
  # eficaz." -> eficaz(conj) hangs from procedimiento(NOUN), not from rapido(ADJ).
  # The check first_conj_pos==conj_pos fails (NOUN vs ADJ) and the case is
  # lost. Extra branch: if the "head of the head" is a NOUN with an amod child
  # whose POS matches that of the second conjunct, treat that amod as the
  # real first conjunct.
  amod_children <- tokens %>%
    dplyr::filter(dplyr::coalesce(.data$dep_rel, "") == "amod") %>%
    dplyr::transmute(.data$doc_id, .data$sentence_id,
                     noun_id  = .data$head_token_id_int,
                     amod_pos = .data$pos) %>%
    dplyr::distinct()

  f64_amod_anchor <- cc_tokens %>%
    dplyr::filter(
      stringr::str_detect(dplyr::coalesce(.data$conj_dep_rel, ""), "^conj($|:)"),
      dplyr::coalesce(.data$conj_pos,     "") == "ADJ",
      dplyr::coalesce(.data$first_conj_pos, "") == "NOUN",
      !.data$has_subject
    ) %>%
    dplyr::inner_join(
      amod_children,
      by = c("doc_id", "sentence_id", "conj_head_id" = "noun_id")
    ) %>%
    dplyr::filter(.data$amod_pos == "ADJ")

  f64 <- dplyr::bind_rows(f64_std, f64_amod_anchor) %>%
    count_feature_traced("f_64_phrasal_coordination")

  # f_65  Clausal coordination
  #  (a) finite verbal or copular predicates with an explicit subject.
  f65_dep <- cc_tokens %>%
    dplyr::filter(
      stringr::str_detect(dplyr::coalesce(.data$conj_dep_rel, ""), "^conj($|:)"),
      .data$has_subject
    ) %>%
    dplyr::semi_join(
      finite_predicates,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    )

  #  (b) positional branch (biber_espanol_completo.md §f_65): "y/e" in
  #      clause-initial position, preceded by sentence-final punctuation
  #      OR at the very start of the document. In Spanish-GSD a
  #      sentence that starts with "Y" is tagged CCONJ/cc whose head is the
  #      root of its own sentence (there is no conj relation across sentences).
  #      Excludes pero/mas and nominal fragments.
  f65_initial <- tokens %>%
    dplyr::filter(!.data$pos %in% c("PUNCT", "SYM", "SPACE")) %>%
    dplyr::group_by(.data$doc_id, .data$sentence_id) %>%
    dplyr::slice_min(.data$token_id_int, n = 1L, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      stringr::str_to_lower(.data$token) %in% c("y", "e"),
      .data$pos == "CCONJ"
    ) %>%
    dplyr::semi_join(
      finite_predicates,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    )

  coord_evidence_cols <- c("doc_id", "sentence_id", "token_id_int",
                           "token", "lemma", "pos", "feats", "head_token_id_int")
  f65 <- dplyr::bind_rows(
    f65_dep     %>% dplyr::select(dplyr::all_of(coord_evidence_cols)),
    f65_initial %>% dplyr::select(dplyr::all_of(coord_evidence_cols))
  ) %>% count_feature_traced("f_65_clausal_coordination")

  counts <- doc_ids %>%
    dplyr::left_join(f63$counts, by = "doc_id") %>%
    dplyr::left_join(f64$counts, by = "doc_id") %>%
    dplyr::left_join(f65$counts, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )
  evidence <- bind_evidence(f63$evidence, f64$evidence, f65$evidence)
  make_block_result(counts = counts, evidence = evidence)
}

# -----------------------------------------------------------------------------
# 4.  block_negation_es   f_66-f_67
# -----------------------------------------------------------------------------

#' Negation features (Spanish)
#'
#' f_66  Palabras negativas, incluso en concordancia con <<no>>
#' f_67  <<no>> como ADV/PART dependiente de VERB/AUX/ADJ/ADV
#'
#' @param tokens Annotated token data frame
#' @param doc_ids One-column data frame with column `doc_id`
#' @param neg_synthetic_terms Lemmas de pronombres/adverbios negativos
#'   (nadie, nada, ninguno, ninguna, nunca, jamas)
#' @param negation_part_lemmas Lemmas de particulas negativas (no)
#' @return Data frame with f_66_neg_synthetic, f_67_neg_analytic
#' @keywords internal
block_negation_es <- function(tokens, doc_ids,
                               neg_synthetic_terms,
                               negation_part_lemmas,
                               negation_adverbs = NULL) {

  # In Spanish, the adverbs "nunca", "jamas", "tampoco" are SYNTHETIC
  # negation according to Biber (1985) and biber_espanol_completo.md F_66, not
  # analytic. We add them to the list to detect them in f_66.
  neg_synthetic_terms <- unique(c(
    neg_synthetic_terms,
    "nunca", "jam\u00e1s", "jamas", "tampoco"
  ))

  # -- Helper table of analytically negated heads ----------------------------
  # Columns: doc_id, sentence_id, neg_head_id
  # HERNAN REVIEW (Phase 5): f_67 is extended beyond placement before the
  # verb. 'no' as a focus adverb on other constituents also counts:
  # 'una respuesta no definitiva' (ADJ head), 'no muy lejos' (ADV head). The
  # Source POS and dependency filters exclude nouns and independent answers.
  verb_pos <- c("VERB", "AUX", "ADJ", "ADV")

  analytic_heads <- tokens %>%
    dplyr::filter(
      .data$lemma %in% negation_part_lemmas,
      .data$pos %in% c("ADV", "PART"),
      .data$dep_rel %in% c("advmod", "neg"),
      !is.na(.data$head_token_id_int)
    ) %>%
    # Check that the head is a verbal, adjectival or adverbial constituent.
    dplyr::rename(neg_head_id = "head_token_id_int") %>%
    dplyr::left_join(
      tokens %>%
        dplyr::transmute(
          doc_id2      = .data$doc_id,
          sentence_id2 = .data$sentence_id,
          neg_head_id  = .data$token_id_int,
          head_pos     = .data$pos
        ),
      by = c(
        "doc_id"      = "doc_id2",
        "sentence_id" = "sentence_id2",
        "neg_head_id"
      )
    ) %>%
    dplyr::filter(dplyr::coalesce(.data$head_pos, "") %in% verb_pos) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$neg_head_id)

  # -- f_67  Analytic negation -----------------------------------------------
  # Phase 2l: we restore head_token_id_int (originally renamed to
  # neg_head_id for the join) so that count_feature_traced can build
  # evidence with the E1 schema.
  f67 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% negation_part_lemmas,
      .data$pos %in% c("ADV", "PART"),
      .data$dep_rel %in% c("advmod", "neg"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::inner_join(
      analytic_heads %>%
        dplyr::rename(head_token_id_int = "neg_head_id"),
      by = c("doc_id", "sentence_id", "head_token_id_int")
    ) %>%
    count_feature_traced("f_67_neg_analytic")

  # -- f_66  Synthetic negation ----------------------------------------------
  # Also accept CCONJ for "ni" (UDPipe tags it as a conjunction).
  # Do not exclude when it co-occurs with preverbal "no": in Spanish
  # negative concord ("no vino nadie") is obligatory and Biber counts
  # both elements. biber_espanol_completo.md F_66.
  f66 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% neg_synthetic_terms,
      .data$pos %in% c("PRON", "ADV", "DET", "CCONJ")
    ) %>%
    count_feature_traced("f_66_neg_synthetic")

  counts <- doc_ids %>%
    dplyr::left_join(f66$counts, by = "doc_id") %>%
    dplyr::left_join(f67$counts, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )
  evidence <- bind_evidence(f66$evidence, f67$evidence)
  make_block_result(counts = counts, evidence = evidence)
}

# -----------------------------------------------------------------------------
# 5.  block_lexical_membership_es   f_10 (dem.), f_14-f_16 (nouns)
# -----------------------------------------------------------------------------

#' Demonstrative pronoun, nominalization, and noun features (Spanish)
#'
#' f_10  Pronombres demostrativos (este, ese, aquel + formas)
#' f_14  Nominalizaciones (sustantivos con sufijos productivos)
#' f_15  ELIMINADO (intraducible): el gerundio espanol no funciona como
#'       sustantivo; las funciones nominales del -ing ingles se realizan
#'       mediante infinitivos (f_24) y nominalizaciones (f_14).
#'       biber_espanol_completo.md sec. F_15.
#' f_16  Otros sustantivos (= total NOUN/PROPN - f_14; sin resta de f_15)
#' f_51  Demostrativos determinantes (mismo este/ese/aquel en funcion DET)
#'
#' @param tokens Annotated token data frame
#' @param doc_ids One-column data frame with column `doc_id`
#' @param word_lists_lookup Word lists lookup
#' @return Data frame with f_10, f_14, f_16, f_51
#' @keywords internal
block_lexical_membership_es <- function(tokens, doc_ids, word_lists_lookup) {

  # 2026-04-20: pronoun_matchlist does not contain demonstratives (este/ese/aquel).
  # demonstrative_matchlist (added in word_lists.yaml) is loaded for f_10 and f_51.
  # pronoun_matchlist is kept for other generic uses within the block.
  # See: docs/DECISIONES_ES.md ?f_10-f_51.
  pronoun_terms         <- get_word_list(word_lists_lookup, "pronoun_matchlist")
  demonstr_terms        <- get_word_list(word_lists_lookup, "demonstrative_matchlist")
  nominalization_sfx    <- get_word_list(word_lists_lookup, "nominalization_suffixes")
  nominalization_stop   <- normalize_terms(
    get_word_list(word_lists_lookup, "nominalization_stoplist")
  )
  # gerund_stop removed: f_15 is untranslatable, the Spanish gerund is not nominal.

  # -- f_10  Demonstrative pronouns ------------------------------------------
  # 2026-04-20: changed from pronoun_terms to demonstr_terms.
  # pronoun_matchlist has no demonstrative forms (este/ese/aquel);
  # using it here made f_10 count personal/possessive pronouns.
  f10 <- tokens %>%
    dplyr::filter(
      stringr::str_to_lower(.data$token) %in%
        stringr::str_to_lower(demonstr_terms),
      .data$pos == "PRON"
    ) %>%
    count_feature_traced("f_10_demonstrative_pronoun")

  # -- f_51  Demonstrative determiners ---------------------------------------
  # 2026-04-20: changed from pronoun_terms to demonstr_terms.
  # pronoun_matchlist only had unstressed possessives (mi/tu/su); those are not
  # demonstratives. demonstrative_matchlist contains este/ese/aquel + forms.
  f51 <- tokens %>%
    dplyr::filter(
      stringr::str_to_lower(.data$token) %in%
        stringr::str_to_lower(demonstr_terms),
      .data$pos == "DET",
      dplyr::coalesce(.data$dep_rel, "") == "det"
    ) %>%
    count_feature_traced("f_51_demonstratives")

  # -- f_14  Nominalizations -------------------------------------------------
  if (length(nominalization_sfx) > 0) {
    sfx_pat <- paste0(
      "(", paste(nominalization_sfx, collapse = "|"), ")$"
    )
  } else {
    sfx_pat <- "^$"
  }

  # -- f_14  Nominalizations -------------------------------------------------
  # Phase 2m: we identify the nominalized tokens first (so that both
  # f_14 and f_16 can use the same set -- f_14 counts them, f_16
  # excludes them via anti_join to avoid subtracting from the count afterwards).
  f14_rows <- tokens %>%
    dplyr::filter(.data$pos == "NOUN") %>%
    dplyr::mutate(lem_lower = stringr::str_to_lower(.data$lemma)) %>%
    dplyr::filter(
      stringr::str_detect(.data$lem_lower, sfx_pat),
      !.data$lem_lower %in% nominalization_stop
    ) %>%
    dplyr::select(-"lem_lower")

  f14 <- count_feature_traced(f14_rows, "f_14_nominalizations")

  # f_15 (gerunds) REMOVED: untranslatable.

  # -- f_16  Other nouns -----------------------------------------------------
  # In the legacy code this was computed as (total NOUN/PROPN) - f_14. To
  # preserve the invariant count == nrow(evidence), here we anti-join the
  # f_14 tokens before counting; the numeric result is identical.
  f16_rows <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("NOUN", "PROPN")
    ) %>%
    dplyr::anti_join(
      f14_rows %>% dplyr::select("doc_id", "sentence_id", "token_id_int"),
      by = c("doc_id", "sentence_id", "token_id_int")
    )
  f16 <- count_feature_traced(f16_rows, "f_16_other_nouns")

  counts <- doc_ids %>%
    dplyr::left_join(f10$counts,  by = "doc_id") %>%
    dplyr::left_join(f51$counts,  by = "doc_id") %>%
    dplyr::left_join(f14$counts,  by = "doc_id") %>%
    dplyr::left_join(f16$counts,  by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )
  evidence <- bind_evidence(f10$evidence, f51$evidence, f14$evidence, f16$evidence)
  make_block_result(counts = counts, evidence = evidence)
}
