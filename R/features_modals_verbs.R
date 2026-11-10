# features_modals_verbs.R
# Adjective, preposition, adverb, modal, and specialized-verb features (Spanish)
# f_39-f_42, f_52-f_58
#
# LINGUISTIC NOTE -- Spanish modals:
#   Modal verbs are recognized through their infinitival complements:
#     f_52  Possibility:  poder + INF, caber + INF
#     f_53  Necessity:    deber + INF, tener_que + INF,
#                         haber_de + INF, haber_que + INF
#     f_54  Predictive:   synthetic future (Tense=Fut,VerbForm=Fin)
#                         + ir_a + INF (progressive-future periphrasis)
#
#   To avoid false positives ("el poder", "el deber" as nouns)
#   we require a linked infinitival complement and, for tener, haber and ir,
#   the corresponding que/de/a marker.
#
# NOTE -- extract_feat() is defined in
#   features_tense_pronouns.R and is visible in the same package namespace.

# -----------------------------------------------------------------------------
# 0.  Helper: modal periphrasis (head verb + dependent infinitive)
# -----------------------------------------------------------------------------

modal_periphrasis_links <- function(tokens, lemmas) {
  modal_toks <- tokens %>%
    dplyr::filter(
      .data$lemma %in% lemmas,
      .data$pos %in% c("VERB", "AUX")
    )
  clause_deps <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "AUX", "ADJ", "NOUN", "PROPN", "ADV"),
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""),
        "^(xcomp|ccomp|obj)($|:)"
      )
    ) %>%
    dplyr::transmute(
      .data$doc_id, .data$sentence_id,
      modal_id = .data$head_token_id_int,
      complement_head_id = .data$token_id_int
    )
  links <- dplyr::bind_rows(
    nonfinite_complements(
      tokens,
      modal_toks %>% dplyr::inner_join(
        clause_deps,
        by = c("doc_id", "sentence_id", "token_id_int" = "modal_id"),
        relationship = "one-to-many"
      )
    ),
    auxiliary_complements(tokens) %>%
      dplyr::filter(.data$lemma %in% lemmas)
  ) %>%
    dplyr::filter(.data$complement_form == "Inf")

  markers <- tokens %>%
    dplyr::filter(
      .data$lemma %in% c("que", "de", "a"),
      .data$pos %in% c("SCONJ", "CCONJ", "ADP", "PART"),
      .data$dep_rel %in% c("mark", "fixed", "case")
    ) %>%
    dplyr::transmute(
      .data$doc_id, .data$sentence_id,
      marker_head_id = .data$head_token_id_int, marker = .data$lemma
    )
  marked <- links %>%
    dplyr::inner_join(markers, by = c("doc_id", "sentence_id"),
                      relationship = "many-to-many") %>%
    dplyr::filter(
      .data$marker_head_id == .data$token_id_int |
        .data$marker_head_id == .data$complement_id |
        .data$marker_head_id == .data$complement_head_id,
      (.data$lemma == "tener" & .data$marker == "que") |
        (.data$lemma == "haber" & .data$marker %in% c("que", "de")) |
        (.data$lemma == "ir" & .data$marker == "a")
    ) %>%
    dplyr::select(dplyr::all_of(names(links)))
  dplyr::bind_rows(
    links %>% dplyr::filter(!.data$lemma %in% c("tener", "haber", "ir")),
    marked
  ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int,
                    .data$complement_id, .keep_all = TRUE)
}

count_modal_periphrasis <- function(tokens, lemmas, feature_name = NULL) {
  combined <- modal_periphrasis_links(tokens, lemmas)
  if (is.null(feature_name)) {
    return(
      combined %>%
        dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int) %>%
        dplyr::group_by(.data$doc_id) %>%
        dplyr::tally()
    )
  }

  count_feature_traced(combined, feature_name)
}

expand_adjective_conjuncts <- function(tokens, rows) {
  conjuncts <- tokens %>%
    dplyr::filter(
      .data$pos == "ADJ",
      stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^conj($|:)")
    ) %>%
    dplyr::select(dplyr::all_of(names(rows)))
  repeat {
    added <- conjuncts %>%
      dplyr::semi_join(
        rows, by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
      ) %>%
      dplyr::anti_join(rows, by = c("doc_id", "sentence_id", "token_id_int"))
    if (!nrow(added)) break
    rows <- dplyr::bind_rows(rows, added)
  }
  rows
}

# -----------------------------------------------------------------------------
# 1.  block_adj_prep_adv_es   f_39-f_42
# -----------------------------------------------------------------------------

#' Adjective, preposition, and adverb features (Spanish)
#'
#' f_39  ADP tokens, with inventoried prepositional phrases counted once
#' f_40  Attributive adjectives and their conjuncts, with nonpredicative fallback
#' f_41  Predicative adjectives and their conjuncts (xcomp/acomp/cop target/attr)
#' f_42  ADV candidates; the final pipeline excludes occurrences counted as
#' place/time adverbials, downtoners, hedges, or amplifiers
#'
#' @param tokens Annotated token data frame
#' @param doc_ids One-column data frame with column `doc_id`
#' @param dict_lookup Dictionary lookup (yaml list loaded by parse_functions)
#' @param word_lists_lookup Word lists lookup
#' @return Data frame: one row per doc, columns f_39 - f_42
#' @keywords internal
block_adj_prep_adv_es <- function(tokens, doc_ids, dict_lookup, word_lists_lookup) {

  # f_39  Prepositions -- total count of ADP tokens (Biber 1985 counts them
  # all as a measure of nominal/informational density). UDPipe spanish-gsd
  # assigns dep_rel="mark" to some prepositions that introduce non-finite
  # clauses (Para obtener, antes de comenzar), so filtering by
  # dep_rel="case" caused undercounting. See biber_espanol_completo.md F_39.
  f39 <- tokens %>%
    dplyr::filter(.data$pos == "ADP") %>%
    count_feature_traced("f_39_prepositions")

  # HERNAN REVIEW (Phase 5, §f_39): PREPOSITIONAL PHRASES count as
  # ONE preposition, not as their component ADPs. spanish-gsd does not mark them
  # structurally (each ADP is 'case' of its own head), so phrase evidence
  # replaces the covered ADP occurrences. Expanded 'al'/'del' components are used.
  prep_locutions <- get_word_list(word_lists_lookup, "prepositions_compound")
  locutions <- surface_matches(tokens, prep_locutions)
  for (i in seq_len(nrow(locutions))) {
    rows <- tokens[seq.int(locutions$start_row[i], locutions$end_row[i]), ]
    covered <- f39$evidence$doc_id == rows$doc_id[1L] &
      f39$evidence$sentence_id == rows$sentence_id[1L] &
      f39$evidence$token_id %in% rows$token_id_int
    anchor <- rows[1L, ]
    anchor$token <- paste(rows$token, collapse = " ")
    f39$evidence <- bind_evidence(
      f39$evidence[!covered, ], evidence_from_rows(anchor, "f_39_prepositions")
    )
  }
  f39$counts <- f39$evidence %>%
    dplyr::count(.data$doc_id, name = "f_39_prepositions")

  # f_40  Attributive adjective
  amod_docs <- unique(tokens$doc_id[tokens$dep_rel %in% "amod" & tokens$pos == "ADJ"])
  f40_standard <- tokens %>%
      dplyr::filter(
        .data$pos == "ADJ",
        dplyr::coalesce(.data$dep_rel, "") == "amod"
      )
  f40_fallback <- tokens %>%
      dplyr::filter(!.data$doc_id %in% amod_docs) %>%
      dplyr::group_by(.data$doc_id, .data$sentence_id) %>%
      dplyr::arrange(.data$token_id_int, .by_group = TRUE) %>%
      dplyr::filter(
        .data$pos == "ADJ",
        dplyr::lead(.data$pos, default = "") %in% c("NOUN", "PROPN", "ADJ")
      ) %>%
      dplyr::ungroup()
  # f_41  Predicative adjective (HERNAN REVIEW v2: extended to predicative
  # complements, at the author's request -- the original spec explicitly
  # excluded these cases).
  #
  # List of "canonical" copular verbs according to biber_espanol_completo.md
  # (kept only as reference/documentation, NO LONGER used as a filter):
  # ser, estar, parecer, volverse, quedarse, ponerse, resultar, permanecer.
  linking_verbs <- c("ser", "estar", "parecer", "resultar",
                     "quedar",       # quedarse
                     "volver",       # volverse
                     "poner",        # ponerse
                     "permanecer")

  f41_dep <- tokens %>%
    dplyr::filter(
      .data$pos == "ADJ",
      dplyr::coalesce(.data$dep_rel, "") %in% c("xcomp", "acomp")
    )

  # ADJ that has as a dependent a verb tagged "cop" (root of the
  # sentence), WITHOUT a lemma restriction. Verified empirically: when the
  # subject is null or postposed, UDPipe reanalyzes constructions with a full
  # verb + predicative complement AS IF they were copular -- the ADJ becomes
  # the root and the real verb (llegar, encontrar, considerar...) becomes
  # its "cop" -- with the real verb's lemma, not one of linking_verbs. If UDPipe
  # already tagged dep_rel="cop", it is asserting structurally that the verb
  # works as a copula in that sentence; the lemma restriction was an
  # unnecessary safeguard once predicatives are included:
  #   "Llego cansada del trabajo."     -> Llego=cop of cansada (root, ADJ)
  #   "Encontraron abierta la puerta." -> Encontraron=cop of abierta (root)
  #   "Se considera relevante el tema."-> considera=cop of relevante (root)
  f41_cop_head <- tokens %>%
    dplyr::filter(.data$pos == "ADJ") %>%
    dplyr::inner_join(
      tokens %>%
        dplyr::filter(
          dplyr::coalesce(.data$dep_rel, "") == "cop"
        ) %>%
        dplyr::transmute(
          .data$doc_id, .data$sentence_id,
          token_id_int = .data$head_token_id_int   # the ADJ is the head of the cop
        ),
      by = c("doc_id", "sentence_id", "token_id_int")
    )

  # Legacy parser layout: the attr adjective depends on the linking verb.
  f41_cop_dep <- tokens %>%
    dplyr::filter(
      .data$pos == "ADJ",
      dplyr::coalesce(.data$dep_rel, "") == "attr",
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      tokens %>%
        dplyr::transmute(
          .data$doc_id, .data$sentence_id,
          head_token_id_int = .data$token_id_int,
          head_lemma = .data$lemma
        ),
      by = c("doc_id", "sentence_id", "head_token_id_int")
    ) %>%
    dplyr::filter(.data$head_lemma %in% linking_verbs)

  # Phase 2h: preserve the E1 columns for evidence. count_feature_traced
  # deduplicates with .keep_all = TRUE internally.
  adj_evidence_cols <- c("doc_id", "sentence_id", "token_id_int",
                         "token", "lemma", "pos", "feats", "head_token_id_int")
  f41_rows <- dplyr::bind_rows(
    f41_dep      %>% dplyr::select(dplyr::all_of(adj_evidence_cols)),
    f41_cop_head %>% dplyr::select(dplyr::all_of(adj_evidence_cols)),
    f41_cop_dep  %>% dplyr::select(dplyr::all_of(adj_evidence_cols))
  )
  f41 <- expand_adjective_conjuncts(tokens, f41_rows) %>%
    count_feature_traced("f_41_adj_pred")

  f40_fallback <- f40_fallback %>%
    dplyr::anti_join(
      f41$evidence,
      by = c("doc_id", "sentence_id", "token_id_int" = "token_id")
    )
  f40_rows <- dplyr::bind_rows(f40_standard, f40_fallback)
  f40 <- expand_adjective_conjuncts(tokens, f40_rows) %>%
    count_feature_traced("f_40_adj_attr")

  # Exclusions require the final reconciled evidence and dictionary phrase spans.
  f42 <- tokens %>%
    dplyr::filter(.data$pos == "ADV") %>%
    count_feature_traced("f_42_adverbs")

  counts <- doc_ids %>%
    dplyr::left_join(f39$counts, by = "doc_id") %>%
    dplyr::left_join(f40$counts, by = "doc_id") %>%
    dplyr::left_join(f41$counts, by = "doc_id") %>%
    dplyr::left_join(f42$counts, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )
  evidence <- bind_evidence(f39$evidence, f40$evidence, f41$evidence, f42$evidence)
  make_block_result(counts = counts, evidence = evidence)
}

exclude_specialized_adverbs <- function(evidence, spans) {
  features <- c("f_04_place_adverbials", "f_05_time_adverbials", "f_46_downtoners",
                "f_47_hedges", "f_48_amplifiers")
  adverbs <- evidence[evidence$feature == "f_42_adverbs", ]
  adverbs$.evidence_row <- which(evidence$feature == "f_42_adverbs")
  excluded <- dplyr::semi_join(
    adverbs, evidence[evidence$feature %in% features, ],
    by = c("doc_id", "sentence_id", "token_id")
  )$.evidence_row
  # A phrase can contain several adverbs or have a non-adverb evidence anchor.
  phrase_rows <- dplyr::inner_join(
    adverbs, spans[spans$feature %in% features, ],
    by = dplyr::join_by("doc_id", "sentence_id",
                        "token_id" >= "start", "token_id" <= "end"),
    relationship = "many-to-many"
  )$.evidence_row
  excluded <- c(excluded, phrase_rows)
  evidence[!seq_len(nrow(evidence)) %in% excluded, ]
}

# -----------------------------------------------------------------------------
# 2.  block_modals_es   f_52-f_54
# -----------------------------------------------------------------------------

#' Modal verb perifrastic features (Spanish)
#'
#' f_52  Posibilidad:  poder + INF, caber + INF
#' f_53  Necesidad:    deber + INF, tener_que + INF,
#'                     haber_de + INF, haber_que + INF
#' f_54  Predictivo:   futuro sintetico (Tense=Fut,VerbForm=Fin)
#'                     + condicional (Mood=Cnd,VerbForm=Fin) + ir a + INF
#'
#' Infinitival complements include auxiliary chains. Tener/haber/ir require
#' their linked que/de/a marker. Occurrences are anchored on the modal verb.
#'
#' @param tokens Annotated token data frame
#' @param doc_ids One-column data frame with column `doc_id`
#' @param dict_lookup Dictionary lookup
#' @return Data frame: one row per doc, columns f_52 - f_54
#' @keywords internal
block_modals_es <- function(tokens, doc_ids, dict_lookup) {

  # f_52  Possibility
  poss_lemmas <- dictionary_to_lemmas(dict_lookup, "f_52_modal_possibility",
                                      head_word = TRUE)
  possibility_rows <- modal_periphrasis_links(tokens, poss_lemmas)

  # HERNAN REVIEW (Phase 5): 'puede que' + subjunctive (counterpart of may/might).
  # spanish-gsd parses 'Puede que' as a FIXED expression: 'poder' (AUX/VERB) with
  # a 'que' child with dep_rel=fixed (verified; the subordinate verb is often
  # mis-tagged Mood=Ind, which is why this fixed layout does not filter mood).
  que_fixed_heads <- tokens %>%
    dplyr::filter(
      .data$lemma == "que",
      dplyr::coalesce(.data$dep_rel, "") == "fixed",
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id,
                    poder_id = .data$head_token_id_int)
  subjunctive_heads <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "AUX"),
      extract_feat(.data$feats, "Mood") == "Sub"
    ) %>%
    dplyr::transmute(
      .data$doc_id, .data$sentence_id,
      subjunctive_head = dplyr::if_else(
        stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^aux") |
          dplyr::coalesce(.data$dep_rel, "") == "cop",
        .data$head_token_id_int, .data$token_id_int
      )
    )
  que_clauses <- tokens %>%
    dplyr::filter(stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^ccomp($|:)")) %>%
    dplyr::semi_join(
      subjunctive_heads,
      by = c("doc_id", "sentence_id", "token_id_int" = "subjunctive_head")
    ) %>%
    dplyr::semi_join(
      tokens %>% dplyr::filter(.data$lemma == "que", .data$dep_rel == "mark"),
      by = c("doc_id", "sentence_id", "token_id_int" = "head_token_id_int")
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, poder_id = .data$head_token_id_int)
  f52_pq <- tokens %>%
    dplyr::filter(.data$lemma == "poder", .data$pos %in% c("VERB", "AUX")) %>%
    dplyr::semi_join(
      dplyr::bind_rows(que_fixed_heads, que_clauses),
      by = c("doc_id", "sentence_id", "token_id_int" = "poder_id")
    )
  f52 <- dplyr::bind_rows(possibility_rows, f52_pq) %>%
    count_feature_traced("f_52_modal_possibility")

  # f_53  Necessity
  #   deber + INF (without "de" = deontic necessity)
  #   deber_de + INF (epistemic probability; included by convention)
  #   tener que, haber de and haber que require a linked particle.
  nec_lemmas <- dictionary_to_lemmas(dict_lookup, "f_53_modal_necessity",
                                     head_word = TRUE)
  f53 <- count_modal_periphrasis(tokens, nec_lemmas, "f_53_modal_necessity")

  # f_54  Predictive
  # According to biber_espanol_completo.md:
  #   (a) Synthetic future: Tense=Fut + VerbForm=Fin
  #   (b) Simple and compound conditional: Mood=Cnd + VerbForm=Fin
  #   (c) Ir a + INF: future periphrasis (included as a functional equivalent of "will")
  # Note: in UDPipe spanish-gsd the conditional carries Mood=Cnd (hablaria, habria).

  f54_fut <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "AUX"),
      dplyr::coalesce(extract_feat(.data$feats, "Tense"),    "") == "Fut",
      dplyr::coalesce(extract_feat(.data$feats, "VerbForm"), "") == "Fin"
    )

  #   (a2) Conditional (Mood=Cnd + VerbForm=Fin): hablaria, habria hablado, etc.
  f54_cnd <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "AUX"),
      dplyr::coalesce(extract_feat(.data$feats, "Mood"),     "") == "Cnd",
      dplyr::coalesce(extract_feat(.data$feats, "VerbForm"), "") == "Fin"
    )

  #   (b) ir a + INF, with ir as auxiliary or head of the infinitive.
  # 2026-04-20: Tense filter added to exclude physical motion.
  # Without a filter, "fue a buscarlo" (Tense=Past) was counted as a periphrastic future.
  # Decision: Tense=Pres (va a hacer) and Tense=Imp (iba a hacer,
  # future in the past / conditional) are accepted. Tense=Past (fue a + INF =
  # directed physical motion) and Tense=Fut (ira a + INF = double future, rare) are excluded.
  # See: docs/DECISIONES_ES.md ?f_54.
  ir_a_inf <- modal_periphrasis_links(tokens, "ir") %>%
    dplyr::filter(
      .data$morph_verbform == "Fin",
      dplyr::coalesce(extract_feat(.data$feats, "Tense"), "") %in% c("Pres", "Imp")
    )

  modal_evidence_cols <- c("doc_id", "sentence_id", "token_id_int",
                           "token", "lemma", "pos", "feats", "head_token_id_int")
  f54 <- dplyr::bind_rows(
    f54_fut  %>% dplyr::select(dplyr::all_of(modal_evidence_cols)),
    f54_cnd  %>% dplyr::select(dplyr::all_of(modal_evidence_cols)),
    ir_a_inf %>% dplyr::select(dplyr::all_of(modal_evidence_cols))
  ) %>% count_feature_traced("f_54_modal_predictive")

  counts <- doc_ids %>%
    dplyr::left_join(f52$counts, by = "doc_id") %>%
    dplyr::left_join(f53$counts, by = "doc_id") %>%
    dplyr::left_join(f54$counts, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )
  evidence <- bind_evidence(f52$evidence, f53$evidence, f54$evidence)
  make_block_result(counts = counts, evidence = evidence)
}

# -----------------------------------------------------------------------------
# 3.  block_specialized_verbs_es   f_55-f_58
# -----------------------------------------------------------------------------

#' Specialized verb class features (Spanish)
#'
#' f_55  Verbos publicos  (decir, afirmar, senalar, indicar, declarar?)
#' f_56  Verbos privados  (creer, pensar, saber, sentir, suponer?)
#' f_57  Verbos suasivos  (pedir, exigir, recomendar, sugerir, ordenar?)
#' f_58  Verbos "seem"    (parecer, resultar copulativo, verse con atributo)
#'
#' Las listas de lemas se leen de dict.yaml (secciones f_55_verb_public,
#' f_56_verb_private, f_57_verb_suasive, f_58_verb_seem).
#'
#' @param tokens Annotated token data frame
#' @param doc_ids One-column data frame with column `doc_id`
#' @param dict_lookup Dictionary lookup
#' @return Data frame: one row per doc, columns f_55 - f_58
#' @keywords internal
block_specialized_verbs_es <- function(tokens, doc_ids, dict_lookup) {

  count_verb_class <- function(lemmas, col_name) {
    if (length(lemmas) == 0) {
      return(make_block_result(
        tibble::tibble(doc_id = character(), !!col_name := integer())
      ))
    }
    tokens %>%
      dplyr::filter(
        .data$lemma %in% lemmas,
        .data$pos   %in% c("VERB", "AUX")
      ) %>%
      count_feature_traced(col_name)
  }

  f55 <- count_verb_class(
    dictionary_to_lemmas(dict_lookup, "f_55_verb_public"),  "f_55_verb_public")
  # Exclude the f_58 verbs (parecer/resultar) to avoid double counting:
  # parecer is an epistemic evidential (f_58), not a mental-process verb.
  # biber_espanol_completo.md F_58.
  private_lemmas <- setdiff(
    dictionary_to_lemmas(dict_lookup, "f_56_verb_private"),
    dictionary_to_lemmas(dict_lookup, "f_58_verb_seem")
  )
  reflexive_heads <- tokens %>%
    dplyr::filter(.data$pos == "PRON", extract_feat(.data$feats, "Reflex") == "Yes") %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, reflexive_head = .data$head_token_id_int)
  realization <- tokens %>%
    dplyr::filter(
      "darse_cuenta" %in% as.character(dict_lookup[["f_56_verb_private"]]),
      .data$lemma == "dar", .data$pos %in% c("VERB", "AUX")
    ) %>%
    dplyr::semi_join(reflexive_heads,
                     by = c("doc_id", "sentence_id", "token_id_int" = "reflexive_head")) %>%
    dplyr::semi_join(
      tokens %>% dplyr::filter(
        .data$lemma == "cuenta", .data$pos == "NOUN",
        stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^(obj|compound)($|:)")
      ),
      by = c("doc_id", "sentence_id", "token_id_int" = "head_token_id_int")
    )
  # Model LIMIT (finding 8.5.C, investigated and NOT patched): UDPipe
  # spanish-gsd mis-lemmatizes several conjugated forms of "recomendar"
  # (a verb with e->ie diphthongization): "recomienda"->"recomienda" (no reduction),
  # "recomiendan"->"recomiendar", "recomiendo"->"recomir" -- each form
  # yields a different, unpredictable lemma, with no pattern that can be patched
  # cleanly via a list of surface forms. 13 diphthongizing verbs of the
  # f_52/f_53/f_57 inventory were tested: only "recomendar" (and
  # "perder", irrelevant here) fail; the rest lemmatize correctly.
  # Documented as a limitation of the model, not of the rule; "recomendar"
  # only counts in f_57 in the forms that DO lemmatize well (recomendo,
  # recomendando, recomendado...).
  f57 <- count_verb_class(
    dictionary_to_lemmas(dict_lookup, "f_57_verb_suasive"), "f_57_verb_suasive")
  # HERNAN REVIEW (Phase 5): 'resultar' only in uses of appearance/evaluation
  # (copular: 'resulta adecuada', 'resulta evidente'), where dep_rel=cop.
  # Excludes change of state ('resulto ganador') and consequence
  # ('resulto de una falla'), where 'resultar' is a full verb (root/not cop).
  # 'parecer' (and the other lemmas of the class) always count.
  seem_lemmas <- dictionary_to_lemmas(dict_lookup, "f_58_verb_seem")
  appearance_verbs <- tokens %>%
    dplyr::filter(
      "verse" %in% seem_lemmas, .data$lemma == "ver", .data$pos %in% c("VERB", "AUX")
    )
  appearance <- dplyr::bind_rows(
    appearance_verbs %>% dplyr::semi_join(
      reflexive_heads,
      by = c("doc_id", "sentence_id", "token_id_int" = "reflexive_head")
    ),
    appearance_verbs %>%
      dplyr::filter(.data$dep_rel == "cop") %>%
      dplyr::semi_join(
        reflexive_heads,
        by = c("doc_id", "sentence_id", "head_token_id_int" = "reflexive_head")
      )
  ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int, .keep_all = TRUE)
  adjective_heads <- tokens %>%
    dplyr::filter(.data$pos == "ADJ") %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, adjective_id = .data$token_id_int)
  appearance_cop <- appearance %>%
    dplyr::filter(.data$dep_rel == "cop") %>%
    dplyr::semi_join(
      adjective_heads,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "adjective_id")
    )
  appearance_head <- appearance %>%
    dplyr::semi_join(
      tokens %>% dplyr::filter(
        .data$pos == "ADJ",
        stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^(xcomp|acomp|attr)($|:)")
      ),
      by = c("doc_id", "sentence_id", "token_id_int" = "head_token_id_int")
    )
  seem_rows <- tokens %>%
    dplyr::filter(
      .data$lemma %in% seem_lemmas,
      .data$pos   %in% c("VERB", "AUX"),
      .data$lemma != "resultar" |
        dplyr::coalesce(.data$dep_rel, "") == "cop"
    )
  f58 <- dplyr::bind_rows(seem_rows, appearance_cop, appearance_head) %>%
    count_feature_traced("f_58_verb_seem")
  f56 <- dplyr::bind_rows(
    tokens %>% dplyr::filter(.data$lemma %in% private_lemmas, .data$pos %in% c("VERB", "AUX")),
    realization
  ) %>%
    dplyr::anti_join(
      f58$evidence,
      by = c("doc_id", "sentence_id", "token_id_int" = "token_id")
    ) %>%
    count_feature_traced("f_56_verb_private")

  counts <- doc_ids %>%
    dplyr::left_join(f55$counts, by = "doc_id") %>%
    dplyr::left_join(f56$counts, by = "doc_id") %>%
    dplyr::left_join(f57$counts, by = "doc_id") %>%
    dplyr::left_join(f58$counts, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )
  evidence <- bind_evidence(f55$evidence, f56$evidence, f57$evidence, f58$evidence)
  make_block_result(counts = counts, evidence = evidence)
}
