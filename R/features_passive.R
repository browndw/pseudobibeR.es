# Passive voice, copular and existential features for Spanish
#
# LINGUISTIC NOTE -- three types of passive in Spanish:
#
#   (A) PERIPHRASTIC passive (ser/estar + participle)
#       <<El libro fue escrito por Galdos.>>
#       UDPipe uses aux:pass. Spanish spaCy also uses plain aux for ser
#       attached to a verbal participle.
#
#   (B) REFLEXIVE / SE-PASSIVE
#       <<Se publico el informe.>>
#       The UD parser marks the se with dep_rel = expl:pass or expl.
#       The verb carries Voice=Pass in feats, or VerbForm=Fin without aux:pass.
#       The French heuristic does NOT detect it -- it needs its own logic.
#
#   (C) STATIVE passive (estar + participle)
#       <<El puente esta cerrado.>>
#       The participle carries Voice=Pass or Aspect=Perf. In UD the participle
#       is usually tagged as ADJ with dep_rel = amod/nmod.
#       It is included in (A) when estar carries aux:pass.
#
# f_17  Agentive passive without an explicit agent  (A+B without a <<por>> phrase)
# f_18  Passive with a <<por>> agent                (A+B with a <<por>> phrase)
# f_19  Ser/estar as main verb (nominal/adjectival copula, not aux)
# f_20  Impersonal existential haber            (hay, habia, hubo?)

#' Extract passive voice, copular and existential features (Spanish)
#'
#' Detecta la pasiva perifistica (ser/estar + participo, aux:pass) y la
#' pasiva refleja (se + VerbForm=Part/Fin, expl:pass). Excluye haber
#' perfect de f_17/f_18. Distingue ser/estar copulativo (f_19) del
#' auxiliar de pasiva. Detecta haber impersonal-existencial (f_20).
#' Spanish spaCy also permits ser + aux attached to a verbal participle
#' and nominal por-agents attached as obj.
#'
#' @param tokens Annotated token data frame
#' @param doc_ids Document IDs
#' @param head_lookup Head token lookup table
#' @param engine Parser convention: "udpipe" or "spacy"
#' @return Data frame con f_17 a f_20
#' @keywords internal
block_passive_voice_es <- function(tokens, doc_ids, head_lookup,
                                   engine) {
  passive_rel_values <- if (engine == "spacy") c("auxpass", "aux:pass") else "aux:pass"

  # -----------------------------------------------------------------------
  # (A) Periphrastic passive -- aux marked as aux:pass
  # -----------------------------------------------------------------------
  perifrastic_passive <- tokens %>%
    dplyr::filter(.data$pos %in% c("VERB", "AUX"),
                  .data$lemma %in% c("ser", "estar"),
                  !is.na(.data$head_token_id_int)) %>%
    dplyr::left_join(head_lookup,
                     by = c("doc_id", "sentence_id",
                            "head_token_id_int" = "token_id_int")) %>%
    dplyr::filter(
      .data$head_pos == "VERB",
      .data$dep_rel %in% passive_rel_values |
        (engine == "spacy" & .data$lemma == "ser" & .data$dep_rel == "aux" &
           .data$head_morph_verbform == "Part")
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id,
                    .data$head_token_id_int, .keep_all = TRUE)

  # -----------------------------------------------------------------------
  # (B) Reflexive / impersonal passive with "se"
  #
  # UDPipe Spanish-GSD does NOT use expl:pass for passive "se": it tags it as
  # PRON with dep_rel "iobj"/"obj"/"obl"/"expl" depending on the case, and the lemma
  # is usually "el" (not "se"). It does not mark Voice=Pass on the verb either. That is
  # why we detect it by surface form, with a third-person finite predicate
  # or auxiliary. Explicit expl:pv/expl:impers annotations are excluded.
  #
  # HERNAN REVIEW (Phase 5, §f_17): ONLY the REFLEXIVE PASSIVE counts in f_17;
  # the IMPERSONAL se is EXCLUDED. Criterion (spec): the reflexive passive has
  # a patient subject that agrees with the verb ('se publicaron LOS INFORMES'
  # -> plural nsubj), while the impersonal does not ('se entrevisto A
  # LOS CANDIDATOS' -> object with 'a', no nsubj; 'se recomienda leer' ->
  # xcomp). We therefore require the verb to have an nsubj/nsubj:pass child.
  # -----------------------------------------------------------------------
  verbs_with_patient_subj <- tokens %>%
    dplyr::filter(
      stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^nsubj"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id,
                    head_token_id_int = .data$head_token_id_int)

  finite_third_person <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "AUX"),
      .data$morph_verbform == "Fin", .data$morph_person == "3"
    )
  third_person_predicates <- dplyr::bind_rows(
    finite_third_person %>%
      dplyr::transmute(.data$doc_id, .data$sentence_id,
                       head_token_id_int = .data$token_id_int),
    finite_third_person %>%
      dplyr::filter(stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^aux")) %>%
      dplyr::select("doc_id", "sentence_id", "head_token_id_int")
  ) %>% dplyr::distinct()

  se_passive <- tokens %>%
    dplyr::filter(
      tolower(.data$token) == "se",
      .data$pos == "PRON",
      !dplyr::coalesce(.data$dep_rel, "") %in% c("expl:pv", "expl:impers"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(head_lookup,
                     by = c("doc_id", "sentence_id",
                            "head_token_id_int" = "token_id_int")) %>%
    dplyr::filter(.data$head_pos %in% c("VERB", "AUX")) %>%
    dplyr::semi_join(
      third_person_predicates,
      by = c("doc_id", "sentence_id", "head_token_id_int")
    ) %>%
    # Reflexive passive: the verb has a patient subject (nsubj). Excludes the
    # impersonal (no nsubj).
    dplyr::semi_join(
      verbs_with_patient_subj,
      by = c("doc_id", "sentence_id", "head_token_id_int")
    ) %>%
    # Exclude when there is already an aux:pass (already counted as periphrastic)
    dplyr::anti_join(
      perifrastic_passive %>%
        dplyr::select("doc_id", "sentence_id", "head_token_id_int"),
      by = c("doc_id", "sentence_id", "head_token_id_int")
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id,
                    .data$head_token_id_int, .keep_all = TRUE)

  # -----------------------------------------------------------------------
  # Union A + B
  # Phase 2d: we widen the select() to keep the columns needed
  # for evidence (token, lemma, pos, feats, token_id_int). The final
  # count is still "one row per passive construction" (distinct on
  # head_token_id_int).
  # -----------------------------------------------------------------------
  passive_aux_cols <- c("doc_id", "sentence_id", "token_id_int",
                        "token", "lemma", "pos", "feats", "head_token_id_int")
  all_passive <- dplyr::bind_rows(
    perifrastic_passive %>% dplyr::select(dplyr::all_of(passive_aux_cols)),
    se_passive          %>% dplyr::select(dplyr::all_of(passive_aux_cols))
  ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$head_token_id_int,
                    .keep_all = TRUE)

  por_objects <- tokens %>%
    dplyr::filter(.data$pos == "ADP", .data$lemma == "por",
                  dplyr::coalesce(.data$dep_rel, "") == "case") %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id,
                    agent_id = .data$head_token_id_int)
  agent_heads <- tokens %>%
    dplyr::inner_join(
      por_objects,
      by = c("doc_id", "sentence_id", "token_id_int" = "agent_id")
    ) %>%
    dplyr::filter(
      stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^(obl(:.*)?|agent)$") |
        (engine == "spacy" & .data$dep_rel == "obj" &
           .data$pos %in% c("NOUN", "PROPN", "PRON"))
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$head_token_id_int) %>%
    dplyr::mutate(has_agent = TRUE)
  all_passive <- all_passive %>%
    dplyr::left_join(agent_heads, by = c("doc_id", "sentence_id", "head_token_id_int")) %>%
    dplyr::mutate(has_agent = dplyr::coalesce(.data$has_agent, FALSE))

  # Spanish-GSD does not reliably distinguish agentive and causal por-phrases.
  f17_rows <- all_passive %>%
    dplyr::filter(!.data$has_agent)
  f17_counts <- f17_rows %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_17_agentless_passives = "n")
  f17_evidence <- evidence_from_rows(f17_rows, "f_17_agentless_passives")

  # f_18  By-passives (with a <<por>> phrase)
  f18_rows <- all_passive %>%
    dplyr::filter(.data$has_agent)
  f18_counts <- f18_rows %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_18_by_passives = "n")
  f18_evidence <- evidence_from_rows(f18_rows, "f_18_by_passives")

  # -----------------------------------------------------------------------
  # f_19  Ser/estar as main verb (nominal/adjectival copula)
  #       Excludes: (1) use as the aux of a periphrastic passive,
  #                 (2) use as the aux of progressive aspect (estar + gerund)
  # -----------------------------------------------------------------------
  # Token IDs that we already know are aux:pass
  aux_pass_ids <- tokens %>%
    dplyr::filter(.data$dep_rel %in% passive_rel_values,
                  .data$lemma %in% c("ser", "estar")) %>%
    dplyr::select("doc_id", "sentence_id", "token_id_int")

  # Token IDs that are progressive aux (estar + gerund)
  progressive_aux_ids <- tokens %>%
    dplyr::filter(.data$lemma == "estar",
                  .data$pos %in% c("AUX", "VERB"),
                  stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""),
                                      "^aux"),
                  !is.na(.data$head_token_id_int)) %>%
    dplyr::left_join(
      tokens %>%
        dplyr::select("doc_id", "sentence_id",
                      "token_id_int", "morph_verbform") %>%
        dplyr::rename(head_vf = "morph_verbform"),
      by = c("doc_id", "sentence_id",
             "head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::filter(.data$head_vf == "Ger") %>%
    dplyr::select("doc_id", "sentence_id", "token_id_int")

  f19 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% c("ser", "estar"),
      # dep_rel must NOT be any kind of aux
      !stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^aux"),
      # Exclude tokens absorbed by a multiword phrase
      # (e.g. "sea" in "o sea", "es" in "es decir").
      !dplyr::coalesce(.data$in_mwe, FALSE)
    ) %>%
    dplyr::anti_join(aux_pass_ids,
                     by = c("doc_id", "sentence_id", "token_id_int")) %>%
    dplyr::anti_join(progressive_aux_ids,
                     by = c("doc_id", "sentence_id", "token_id_int")) %>%
    count_feature_traced("f_19_be_main_verb")

  # -----------------------------------------------------------------------
  # f_20  Impersonal existential haber (hay, habia, hubo, ha habido, puede haber)
  #       Condition: lemma = haber, VERB/AUX, clausal dep_rel (root/ccomp/xcomp/csubj/advcl/acl/parataxis)
  #       and NO dependent nsubj (that tells it apart from perfect haber).
  #       Auxiliary haber (ha comido, haber entendido) has dep_rel aux and is
  #       excluded by the dependency filter. Forms are compared without accents.
  # -----------------------------------------------------------------------
  existential_forms <- c(
    "hay",
    "habia",
    "hubo",
    "habra",
    "habria",
    "haya",                    # present subjunctive
    "hubiera", "hubiese",      # past subjunctive
    "habiendo",                # existential gerund
    "habido",                  # perfect existential (ha habido)
    "haber"                    # infinitival existential (puede haber)
  )

  modal_haber <- count_modal_periphrasis(tokens, "haber", "f_53_modal_necessity")
  haber_nodes <- tokens %>%
    dplyr::filter(
      .data$lemma == "haber",
      .data$pos %in% c("VERB", "AUX"),
      stringi::stri_trans_general(stringr::str_to_lower(.data$token), "Latin-ASCII") %in%
        existential_forms,
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""),
        "^(root|ccomp|xcomp|csubj|advcl|acl|parataxis)"
      )
    ) %>%
    dplyr::anti_join(
      modal_haber$evidence,
      by = c("doc_id", "sentence_id", "token_id_int" = "token_id")
    ) %>%
    # Exclude when it has a dependent nsubj (= perfect haber with a subject)
    dplyr::anti_join(
      tokens %>%
        dplyr::filter(
          stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^nsubj")
        ) %>%
        dplyr::transmute(.data$doc_id, .data$sentence_id,
                         head_token_id_int = .data$head_token_id_int),
      by = c("doc_id", "sentence_id", "token_id_int" = "head_token_id_int")
    )

  f20 <- haber_nodes %>% count_feature_traced("f_20_existential_there")

  # -----------------------------------------------------------------------
  # Assemble
  # -----------------------------------------------------------------------
  counts <- doc_ids %>%
    dplyr::left_join(f17_counts,  by = "doc_id") %>%
    dplyr::left_join(f18_counts,  by = "doc_id") %>%
    dplyr::left_join(f19$counts,  by = "doc_id") %>%
    dplyr::left_join(f20$counts,  by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )

  evidence <- bind_evidence(f17_evidence, f18_evidence, f19$evidence, f20$evidence)

  make_block_result(counts = counts, evidence = evidence)
}
