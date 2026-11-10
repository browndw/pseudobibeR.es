#' @importFrom magrittr %>%
#' @importFrom rlang .data :=
#' @importFrom utils tail
#' @keywords internal
#' @noRd
parse_biber_features <- function(tokens, measure, normalize, engine = c("spacy", "udpipe"),
                                 traced = FALSE) {
  engine <- match.arg(engine)
  dict <- pseudobibeR.es::dict
  word_lists <- pseudobibeR.es::word_lists
  doc_ids <- dplyr::distinct(tokens, .data$doc_id)
  tokens <- flag_mwe_tokens(tokens, get_word_list(word_lists, "multiword_patterns"))
  nominal_infinitives <- nominal_infinitive_rows(tokens, word_lists, dict)
  head_lookup <- tokens %>%
    dplyr::select(
      "doc_id", "sentence_id", "token_id_int",
      head_pos = "pos", head_lemma = "lemma", head_token = "token",
      head_feats = "feats", head_morph_verbform = "morph_verbform",
      head_morph_voice = "morph_voice", head_morph_tense = "morph_tense"
    )
  token_lookup <- tokens %>%
    dplyr::select(
      "doc_id", "sentence_id", "token_id_int", token_pos = "pos",
      token_dep_rel = "dep_rel", token_head_token_id_int = "head_token_id_int",
      token_morph_verbform = "morph_verbform"
    )
  subject_heads <- tokens %>%
    dplyr::filter(stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^(nsubj|csubj)($|:)")) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id,
                    clause_head_token_id_int = .data$head_token_id_int) %>%
    dplyr::mutate(has_subject = TRUE)
  neg_terms <- normalize_terms(get_word_list(word_lists, "negation_particles"))
  neg_adverbs <- normalize_terms(get_word_list(word_lists, "neg_analytic_adverbs"))
  blocks <- list(
    block_tense_es(
      tokens, doc_ids,
      dictionary_to_lemmas(dict, "f_04_place_adverbials"),
      dictionary_to_lemmas(dict, "f_05_time_adverbials"),
      dictionary_to_lemmas(dict, "f_11_indefinite_pronouns")
    ),
    block_personal_pronouns_es(tokens, doc_ids),
    block_proverbs_es(tokens, doc_ids, get_word_list(word_lists, "proverb_object_pronouns")),
    block_impersonal_predicates_es(
      tokens, doc_ids,
      get_word_list(word_lists, "impersonal_weather_verbs"),
      get_word_list(word_lists, "impersonal_weather_root_forms"),
      get_word_list(word_lists, "impersonal_hacer_terms"),
      get_word_list(word_lists, "impersonal_clause_verbs"),
      nominal_infinitives
    ),
    block_lexical_membership_es(tokens, doc_ids, word_lists, nominal_infinitives),
    block_passive_voice_es(tokens, doc_ids, head_lookup, engine),
    block_clause_embedding_es(tokens, doc_ids, head_lookup),
    block_reduced_complements_es(
      tokens, doc_ids, head_lookup,
      get_word_list(word_lists, "reduced_complement_predicates"), nominal_infinitives
    ),
    block_participial_clauses_es(tokens, doc_ids, head_lookup, nominal_infinitives),
    block_relatives_es(
      tokens, doc_ids, head_lookup, get_word_list(word_lists, "relative_subject_intransitives"),
      get_word_list(word_lists, "resumptive_prepositional_frames"),
      get_word_list(word_lists, "resumptive_first_person_forms")
    ),
    block_adj_prep_adv_es(tokens, doc_ids, dict, word_lists),
    block_specialized_verbs_es(tokens, doc_ids, dict),
    block_modals_es(tokens, doc_ids, dict),
    block_contractions_es(tokens, doc_ids, get_word_list(word_lists, "colloquial_contractions")),
    block_interrupted_infinitives_es(
      tokens, doc_ids,
      normalize_terms(c(neg_terms, neg_adverbs,
                        get_word_list(word_lists, "neg_synthetic_determiners"))),
      get_word_list(word_lists, "split_infinitive_adverb_fallbacks"), nominal_infinitives
    ),
    block_split_coordination_es(
      tokens, doc_ids, token_lookup, subject_heads, neg_terms,
      c(dictionary_to_lemmas(dict, "f_52_modal_possibility", head_word = TRUE),
        dictionary_to_lemmas(dict, "f_53_modal_necessity", head_word = TRUE), "ir")
    ),
    block_negation_es(
      tokens, doc_ids,
      normalize_terms(get_word_list(word_lists, "neg_synthetic_determiners")),
      neg_terms,
      nominal_no_modifiers = get_word_list(word_lists, "neg_nominal_no_modifiers")
    )
  )
  evidence <- do.call(bind_evidence, lapply(blocks, extract_evidence))
  evidence <- evidence[evidence$feature %in% .evidence_features, ]

  lexical <- dictionary_evidence(tokens, c(.dual_path_features, .dict_only_features))
  lexical$evidence <- clause_initial_particles(lexical$evidence, tokens)
  for (feature in .dual_path_features) {
    spans <- lexical$spans[lexical$spans$feature == feature, ]
    for (i in seq_len(nrow(spans))) {
      covered <- evidence$feature == feature &
        evidence$doc_id == spans$doc_id[i] &
        evidence$sentence_id == spans$sentence_id[i] &
        evidence$token_id >= spans$start[i] & evidence$token_id <= spans$end[i]
      evidence <- evidence[!covered, ]
    }
  }
  evidence <- bind_evidence(evidence, lexical$evidence)
  evidence <- dplyr::distinct(evidence, .data$doc_id, .data$feature,
                              .data$sentence_id, .data$token_id, .keep_all = TRUE)
  evidence <- exclude_specialized_adverbs(evidence, lexical$spans)

  counts <- doc_ids
  for (feature in .evidence_features) {
    feature_rows <- evidence[evidence$feature == feature, ]
    counts[[feature]] <- tabulate(match(feature_rows$doc_id, counts$doc_id),
                                  nbins = nrow(counts))
  }
  for (feature in .zero_output_features) counts[[feature]] <- 0L
  metrics <- block_lexical_complexity_es(tokens, doc_ids)
  counts <- dplyr::left_join(counts, metrics, by = "doc_id")
  if (normalize) {
    if (any(counts$n_tokens == 0L)) {
      warning("Documents with no word tokens have undefined normalized frequencies.",
              call. = FALSE)
    }
    counts$tot_counts <- ifelse(counts$n_tokens > 0L, counts$n_tokens, NA_real_)
    counts <- normalize_counts(counts)
  }
  if (measure != "none") {
    document_tokens <- lexical_diversity_tokens(tokens, doc_ids)
    lengths <- quanteda::ntoken(document_tokens)
    nonempty <- lengths > 0L
    if (any(!nonempty)) {
      warning(
        "Documents with no lexical-diversity tokens have undefined lexical diversity: ",
        paste(doc_ids$doc_id[!nonempty], collapse = ", "), ".",
        call. = FALSE
      )
    }
    actual <- measure
    if (measure == "MATTR" && any(lengths < 200L)) {
      message("Setting type-to-token ratio to TTR")
      actual <- "TTR"
    }
    counts$f_43_type_token <- rep(NA_real_, nrow(doc_ids))
    if (any(nonempty)) {
      lexdiv <- quanteda.textstats::textstat_lexdiv(
        document_tokens[nonempty], measure = actual
      )
      counts$f_43_type_token[nonempty] <- lexdiv[[actual]][
        match(as.character(which(nonempty)), lexdiv$document)
      ]
    }
  }
  counts <- counts[, c("doc_id", intersect(names(.feature_categories), names(counts)))]
  counts <- as.data.frame(counts, stringsAsFactors = FALSE)
  if (traced) list(counts = counts, evidence = evidence) else counts
}
