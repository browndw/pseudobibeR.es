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
#   f_67 counts scoped ADV/PART no, with guarded non-negation exclusions.
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

analytic_negation_rows <- function(tokens, negation_terms, nominal_no_modifiers) {
  context <- tokens %>%
    dplyr::group_by(.data$doc_id, .data$sentence_id) %>%
    dplyr::mutate(
      neg_prev_pos = dplyr::lag(.data$pos),
      neg_prev_lemma = dplyr::lag(.data$lemma),
      neg_prev2_pos = dplyr::lag(.data$pos, 2L),
      neg_prev_token = dplyr::lag(.data$token),
      neg_next_token = dplyr::lead(.data$token),
      neg_next_pos = dplyr::lead(.data$pos)
    ) %>%
    dplyr::ungroup()
  heads <- tokens %>%
    dplyr::transmute(
      doc_id = .data$doc_id, sentence_id = .data$sentence_id,
      head_token_id_int = .data$token_id_int,
      neg_head_pos = .data$pos, neg_head_lemma = .data$lemma,
      neg_head_dep_rel = .data$dep_rel
    )
  candidates <- context %>%
    dplyr::filter(
      .data$lemma %in% negation_terms, .data$pos %in% c("ADV", "PART"),
      .data$dep_rel %in% c("advmod", "neg"),
      .data$head_token_id_int > 0L,
      .data$head_token_id_int != .data$token_id_int
    ) %>%
    dplyr::inner_join(
      heads, by = c("doc_id", "sentence_id", "head_token_id_int"),
      relationship = "many-to-one"
    ) %>%
    dplyr::filter(
      !is.na(.data$neg_head_pos),
      !.data$neg_head_pos %in% c("", "PUNCT", "SYM", "SPACE"),
      !(.data$neg_head_lemma %in% negation_terms &
          .data$neg_head_pos %in% c("ADV", "PART"))
    )

  matches <- surface_matches(tokens, "no_obstante")
  candidates <- candidates %>%
    dplyr::anti_join(
      tokens[matches$start_row, ],
      by = c("doc_id", "sentence_id", "token_id_int")
    )

  modifiers <- normalize_terms(nominal_no_modifiers)
  article_before <- candidates$neg_prev_pos == "DET" |
    (candidates$neg_prev_lemma %in% modifiers & candidates$neg_prev2_pos == "DET")
  # Guard noun readings without suppressing la no violencia or un no fumador.
  nominal <- article_before & (
    (candidates$neg_next_pos == "ADP" & candidates$neg_head_pos %in% c("NOUN", "PROPN")) |
      (candidates$neg_head_pos == "ADJ" & candidates$neg_head_lemma %in% modifiers &
         candidates$neg_head_dep_rel != "amod")
  )
  article_heads <- tokens %>%
    dplyr::filter(.data$pos == "DET") %>%
    dplyr::transmute(
      doc_id = .data$doc_id, sentence_id = .data$sentence_id,
      article_id = .data$token_id_int, token_id_int = .data$head_token_id_int
    )
  nominal_heads <- candidates %>%
    dplyr::inner_join(
      article_heads, by = c("doc_id", "sentence_id", "token_id_int"),
      relationship = "many-to-many"
    ) %>%
    dplyr::filter(.data$token_id_int > .data$article_id)
  opening <- c("\"", "'", "\u00ab", "\u201c", "\u2018")
  closing <- c("\"", "'", "\u00bb", "\u201d", "\u2019")
  quoted <- candidates$neg_next_token ==
    closing[match(candidates$neg_prev_token, opening)]
  candidates <- candidates[
    !dplyr::coalesce(nominal | quoted, FALSE), ] %>%
    dplyr::anti_join(nominal_heads, by = c("doc_id", "sentence_id", "token_id_int"))

  commas <- tokens %>%
    dplyr::filter(.data$token == ",", .data$pos == "PUNCT") %>%
    dplyr::transmute(
      doc_id = .data$doc_id, sentence_id = .data$sentence_id,
      comma_id = .data$token_id_int
    )
  comma_counts <- candidates %>%
    dplyr::inner_join(
      commas,
      by = dplyr::join_by("doc_id", "sentence_id",
                          "token_id_int" < "comma_id", "head_token_id_int" > "comma_id"),
      relationship = "many-to-many"
    ) %>%
    dplyr::count(.data$doc_id, .data$sentence_id, .data$token_id_int,
                 name = "neg_comma_count")
  later <- candidates %>%
    dplyr::transmute(
      doc_id = .data$doc_id, sentence_id = .data$sentence_id,
      head_token_id_int = .data$head_token_id_int, later_id = .data$token_id_int
    )
  repeated_prefixes <- candidates %>%
    dplyr::filter(.data$neg_next_token == ",") %>%
    dplyr::semi_join(
      later,
      by = dplyr::join_by("doc_id", "sentence_id", "head_token_id_int",
                          "token_id_int" < "later_id")
    )
  # A reply prefix differs from a negator interrupted by a comma-delimited aside.
  candidates %>%
    dplyr::left_join(
      comma_counts, by = c("doc_id", "sentence_id", "token_id_int"),
      relationship = "many-to-one"
    ) %>%
    dplyr::filter(!dplyr::coalesce(
      .data$neg_next_token == "," & .data$neg_comma_count == 1L, FALSE
    )) %>%
    dplyr::anti_join(repeated_prefixes, by = c("doc_id", "sentence_id", "token_id_int"))
}

#' Negation features (Spanish)
#'
#' f_66  Palabras negativas, incluso en concordancia con <<no>>
#' f_67  Scoped ADV/PART <<no>>, including nominal, pronominal, numeral,
#' adjectival, adverbial, and verbal scope. Identifiable reply and noun uses,
#' quoted standalone <<no>>, and <<no obstante>> are excluded.
#'
#' @param tokens Annotated token data frame
#' @param doc_ids One-column data frame with column `doc_id`
#' @param neg_synthetic_terms Lemmas de pronombres/adverbios negativos
#'   (nadie, nada, ninguno, ninguna, nunca, jamas)
#' @param negation_part_lemmas Lemmas de particulas negativas (no)
#' @param negation_adverbs Unused compatibility argument.
#' @param nominal_no_modifiers Adjective lemmas used with an article to
#'   identify nominalized <<no>>, such as <<un no rotundo>>.
#' @return Block result with f_66/f_67 counts and token-level evidence.
#' @keywords internal
block_negation_es <- function(tokens, doc_ids,
                               neg_synthetic_terms,
                               negation_part_lemmas,
                               negation_adverbs = NULL,
                               nominal_no_modifiers = get_word_list(
                                 pseudobibeR.es::word_lists, "neg_nominal_no_modifiers")) {

  # Retain the existing f_66 inventory pending the shared mapping decision.
  neg_synthetic_terms <- unique(c(
    neg_synthetic_terms,
    "nunca", "jam\u00e1s", "jamas", "tampoco"
  ))

  f67 <- analytic_negation_rows(tokens, negation_part_lemmas, nominal_no_modifiers) %>%
    count_feature_traced("f_67_neg_analytic")

  # -- f_66  Synthetic negation ----------------------------------------------
  # Also accept CCONJ for "ni" (UDPipe tags it as a conjunction).
  # Preserve the existing independent marker counts in negative concord.
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
