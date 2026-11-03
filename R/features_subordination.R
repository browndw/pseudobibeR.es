# features_subordination.R
# Subordination and clause-embedding features for Spanish (f_21-f_38)
#
# GENERAL STRATEGY:
#   Complete rewrite that does not rely on pre-computed flags
#   (is_infinitive, is_relative_subject, etc.) that do not always exist.
#   All the logic works directly on standard UD columns:
#     pos (UPOS), dep_rel, feats, lemma, token, head_token_id_int.
#   extract_feat() from features_tense_pronouns.R is used to read
#   morphological features from the `feats` field.
#
# FEATURE MAP:
#   f_21  that-complementizer after a verb      ("dijo que...")
#   f_22  that-complementizer after an adjective ("seguro de que...")
#   f_23  Wh-clause (relative/indirect question)
#   f_24  Infinitives (VerbForm=Inf as clausal head)
#   f_25  Adverbial gerund / complement          (VerbForm=Ger, dep_rel=advcl|ccomp)
#   f_26  Adverbial / absolute participle        (VerbForm=Part, dep_rel=advcl|ccomp|acl)
#   f_27  Postnominal participle (whiz-deletion) (VerbForm=Part, dep_rel=acl, head=NOUN)
#   f_28  REMOVED (untranslatable): the gerund cannot be a noun modifier
#   f_29  Subject relative with <<que>>/quien/cual  [merge f_29+f_31]
#   f_30  Object relative with <<que>>/quien/cual   [merge f_30+f_32]
#   f_31  ABSORBED into f_29
#   f_32  ABSORBED into f_30
#   f_33  Pied-piping (prep + relative pronoun)
#   f_34  Sentential relative (eso que, lo que)
#   f_35  Causal clause (only: porque)
#   f_36  Concessive clause (only: aunque)
#   f_37  Conditional clause (si, a_menos_que, salvo_que)
#   f_38  Other adverbial subordinators
#   f_60  REMOVED (untranslatable): <<que>> is obligatory in Spanish

# -----------------------------------------------------------------------------
# 1.  block_participial_clauses_es   f_24-f_27 (f_28 removed)
# -----------------------------------------------------------------------------

#' Participial and infinitive clause features (Spanish)
#'
#' @param tokens Annotated token data frame (UD format)
#' @param doc_ids One-column data frame with column `doc_id`
#' @param head_lookup Pre-built head-token attribute table
#' @return Data frame: one row per doc, columns f_24 - f_27
#' @keywords internal
block_participial_clauses_es <- function(tokens, doc_ids, head_lookup) {

  # Pre-compute verbform per token (avoids calling extract_feat() repeatedly)
  tokens <- tokens %>%
    dplyr::mutate(
      .vf = dplyr::coalesce(
        extract_feat(.data$feats, "VerbForm"), ""
      )
    )

  # -- f_24  Verbal infinitives, including roots and auxiliaries -------------

  # -- f_15  Nominal gerund (REVERTED to the original design) ----------------
  # f_15_gerunds ALWAYS stays at 0: the Spanish gerund (-ando/-iendo)
  # cannot have a nominal function (that function is covered by the infinitive,
  # captured by f_24, or by a derived nominalization, captured by f_14). The
  # author decided to revert the attempt of Hernan's review (Phase 3) to activate
  # this column via the subject infinitive (csubj); it goes back to the
  # constant-zero behavior of the original design. See biber_espanol_completo.md §f_15.
  # The column is filled with 0 in parse_functions.R (surface_only_zero).

  f24 <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "AUX"),
      .data$.vf == "Inf"
    ) %>%
    count_feature_traced("f_24_infinitives")

  # -- f_25  Adverbial gerund / complement -----------------------------------
  # VerbForm=Ger with dep_rel=advcl|ccomp, NOT postnominal
  f25 <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "AUX"),
      .data$.vf  == "Ger",
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""),
        "^(advcl|ccomp)"
      )
    ) %>%
    count_feature_traced("f_25_present_participle")

  # -- f_26  Adverbial / absolute participle ---------------------------------
  # VerbForm=Part + Voice?Pass with dep_rel=advcl|ccomp|acl
  # (absolute participle: <<llegado el momento?>>, <<terminada la reunion?>>)
  f26_rows <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "ADJ"),
      .data$.vf  == "Part",
      dplyr::coalesce(extract_feat(.data$feats, "Voice"), "") != "Pass",
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""),
        "^(advcl|ccomp|acl)"
      )
    )

  # -- f_27  Postnominal participle (whiz-deletion) --------------------------
  # VerbForm=Part, dep_rel=acl, head is NOUN/PROPN
  f27 <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "ADJ"),
      .data$.vf  == "Part",
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""), "^acl"
      ),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      head_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::filter(
      dplyr::coalesce(.data$head_pos, "") %in% c("NOUN", "PROPN")
    ) %>%
    count_feature_traced("f_27_past_participle_whiz")

  f26 <- f26_rows %>%
    dplyr::anti_join(
      f27$evidence,
      by = c("doc_id", "sentence_id", "token_id_int" = "token_id")
    ) %>%
    count_feature_traced("f_26_past_participle")

  # f_28 (postnominal gerund / whiz-deletion) REMOVED:
  # The Spanish gerund cannot work as a postnominal noun modifier;
  # its use in that position is ungrammatical in standard Spanish. The
  # equivalent function is carried out by full relative clauses (f_29/f_30).
  # Mapping f_28 to those relatives would double the count. Untranslatable.
  # See biber_espanol_completo.md sec. F_28.

  counts <- doc_ids %>%
    dplyr::left_join(f24$counts, by = "doc_id") %>%
    dplyr::left_join(f25$counts, by = "doc_id") %>%
    dplyr::left_join(f26$counts, by = "doc_id") %>%
    dplyr::left_join(f27$counts, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )
  evidence <- bind_evidence(f24$evidence, f25$evidence,
                            f26$evidence, f27$evidence)
  make_block_result(counts = counts, evidence = evidence)
}

# -----------------------------------------------------------------------------
# 2.  block_relatives_es   f_29-f_34
# -----------------------------------------------------------------------------

#' Relative clause features (Spanish)
#'
#' Estrategia UD pura:
#'   - Clausulas relativas = tokens con dep_rel que comienza por "acl"
#'     (acl, acl:relcl) cuyo marcador (dep_rel=mark o ref) es un pronombre
#'     relativo o wh.
#'   - Tipo de relativa (sujeto/objeto) se determina por la funcion UD
#'     del pronombre relativo dentro de la clausula subordinada.
#'
#' @param tokens Annotated token data frame
#' @param doc_ids One-column data frame with column `doc_id`
#' @param head_lookup Pre-built head-token attribute table
#' @return Data frame: one row per doc, columns f_29 - f_34
#' @keywords internal
block_relatives_es <- function(tokens, doc_ids, head_lookup) {

  # Table of relative pronouns/adverbs with their function in the relative clause
  # -- relative pronouns in Spanish UD
  rel_pronouns_all <- c(
    "que", "quien", "quienes",
    "cual", "cuales",
    "cuyo", "cuya", "cuyos", "cuyas",
    "donde", "cuando", "como"
  )
  rel_subj_roles <- c("nsubj", "nsubj:pass")
  # "nmod" is added to capture "quien/cual" after a preposition (dep_rel=nmod
  # in Spanish-GSD when the relative pronoun is the object of a PP).
  rel_obj_roles  <- c("obj", "iobj", "obl", "obl:agent", "nmod")

  # Tokens that are relative pronouns (PronType=Rel or dep_rel=ref|mark
  # and lemma in the list)
  rel_tokens <- tokens %>%
    dplyr::filter(
      .data$lemma %in% rel_pronouns_all,
      .data$pos   %in% c("PRON", "ADV", "DET", "ADJ", "SCONJ"),
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""),
        "^(ref|mark|nsubj|obj|iobj|obl|acl|nmod)"
      ) |
        stringr::str_detect(
          dplyr::coalesce(extract_feat(.data$feats, "PronType"), ""),
          "Rel"
        )
    )

  # Head of the relative clause: the token whose dep_rel = acl*
  # and whose sentence_id matches
  acl_heads <- tokens %>%
    dplyr::filter(
      stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^acl")
    ) %>%
    dplyr::transmute(
      .data$doc_id, .data$sentence_id,
      acl_head_id   = .data$token_id_int,
      acl_head_pos  = .data$pos,
      antecedent_id = .data$head_token_id_int
    )

  # Antecedents of the relatives (NOUN/PROPN preceding the acl_head)
  antecedents <- tokens %>%
    dplyr::filter(.data$pos %in% c("NOUN", "PROPN")) %>%
    dplyr::transmute(
      .data$doc_id, .data$sentence_id,
      antecedent_id = .data$token_id_int,
      ant_pos = .data$pos
    )

  nominal_relatives <- acl_heads %>%
    dplyr::semi_join(antecedents, by = c("doc_id", "sentence_id", "antecedent_id"))
  pied_piping <- tokens %>%
    dplyr::group_by(.data$doc_id, .data$sentence_id) %>%
    dplyr::arrange(.data$token_id_int, .by_group = TRUE) %>%
    dplyr::filter(
      .data$lemma %in% rel_pronouns_all,
      .data$pos %in% c("PRON", "SCONJ"),
      dplyr::lag(.data$pos, default = "") == "ADP" |
        (dplyr::lag(.data$pos, n = 2L, default = "") == "ADP" &
         dplyr::lag(.data$pos, default = "") == "DET")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::semi_join(
      nominal_relatives,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "acl_head_id")
    )

  # Subject/object discriminator for "que" relatives (Spanish-GSD annotates
  # the relative as SCONJ/mark, with no case distinction). Operational rule
  # of biber_espanol_completo.md §f_29: "the relative is the subject when,
  # within the acl:relcl clause, no other nsubj is present" -> its inverse
  # defines f_30: if the verb of the relative clause HAS an explicit external nsubj
  # (someone else is the subject), the relative "que" fills the OBJECT gap.
  # Note: refining by morphological person was dropped because
  # Spanish-GSD mis-tags the 1st sg preterite ("compre") as
  # Person=3|Tense=Fut; with pure pro-drop the gap cannot be told apart and
  # stays in f_29 (documented limitation §2.5/§6/§f_29).
  relcl_obj_ids <- tokens %>%
    dplyr::filter(
      stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^nsubj"),
      .data$lemma != "que",
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id,
                    acl_head_id = .data$head_token_id_int) %>%
    dplyr::mutate(is_obj_relcl = TRUE)

  # -- f_29  Subject relative (que + quien/cual merged) ---------------------
  # MERGE f_29 + f_31 according to biber_espanol_completo.md:
  # In Spanish "que" covers both "that" (f_29) and "who/which" (f_31) in
  # the subject position of a restrictive relative. "Quien/quienes" and
  # "cual/cuales" are formal variants that are absorbed into f_29.
  # The formal collapse makes it impossible to tell f_29 and f_31 apart on the surface.

  # Relative pronouns that can be the subject of a relative (que + quien/cual)
  rel_subj_lemmas <- c("que", "quien", "quienes", "cual", "cuales")

  f29_que <- rel_tokens %>%
    dplyr::filter(
      .data$lemma == "que",
      dplyr::coalesce(.data$dep_rel, "") %in% rel_subj_roles
    ) %>%
    dplyr::left_join(
      acl_heads,
      by = c("doc_id", "sentence_id",
             "head_token_id_int" = "acl_head_id")
    ) %>%
    dplyr::inner_join(antecedents,
                      by = c("doc_id", "sentence_id", "antecedent_id"))

  # Extra branch for Spanish-GSD: relative "que" tagged as SCONJ/mark
  # whose immediate head is a verb with dep_rel=acl* (restrictive relative).
  # In Spanish-GSD the relative pronoun "que" is annotated as SCONJ/mark,
  # not as PRON/nsubj. This branch captures both positions (subject and object)
  # since with Spanish pro-drop they cannot be told apart reliably.
  f29_que_sconj <- tokens %>%
    dplyr::filter(
      .data$lemma == "que",
      .data$pos   == "SCONJ",
      dplyr::coalesce(.data$dep_rel, "") == "mark",
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      acl_heads,
      by = c("doc_id", "sentence_id",
             "head_token_id_int" = "acl_head_id")
    ) %>%
    dplyr::filter(!is.na(.data$antecedent_id)) %>%
    dplyr::inner_join(antecedents,
                      by = c("doc_id", "sentence_id", "antecedent_id")) %>%
    # SUBJECT only: discard relatives whose verb is of the object type
    dplyr::left_join(
      relcl_obj_ids,
      by = c("doc_id", "sentence_id",
             "head_token_id_int" = "acl_head_id")
    ) %>%
    dplyr::filter(!dplyr::coalesce(.data$is_obj_relcl, FALSE))

  # HERNAN REVIEW v2: REVERT of the Phase 2 un-merge, at the author's
  # explicit request. quien/quienes/cual/cuales are absorbed again into
  # f_29/f_30 (original merge per biber_espanol_completo.md); f_31/f_32
  # go back to always-zero columns (see parse_functions.R). The formal
  # collapse on the surface ("que" covers both "that" and "who/which" in
  # relative subject/object position) made it impossible to tell them apart
  # reliably, and that is the original reason for the merge.
  wh_pronouns   <- c("quien", "quienes", "cual", "cuales")

  f29_wh <- rel_tokens %>%
    dplyr::filter(
      .data$lemma %in% wh_pronouns,
      dplyr::coalesce(.data$dep_rel, "") %in% rel_subj_roles
    ) %>%
    dplyr::semi_join(
      nominal_relatives,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "acl_head_id")
    )

  # Phase 2f: select() keeps the E1 columns before bind_rows; the
  # dedup and the evidence are done by count_feature_traced (same dedup as
  # the previous distinct, now with .keep_all = TRUE to keep
  # token/lemma/pos/feats).
  rel_evidence_cols <- c("doc_id", "sentence_id", "token_id_int",
                         "token", "lemma", "pos", "feats", "head_token_id_int")
  f29 <- dplyr::bind_rows(
    f29_que       %>% dplyr::select(dplyr::all_of(rel_evidence_cols)),
    f29_que_sconj %>% dplyr::select(dplyr::all_of(rel_evidence_cols)),
    f29_wh        %>% dplyr::select(dplyr::all_of(rel_evidence_cols))
  ) %>%
    dplyr::anti_join(
      pied_piping, by = c("doc_id", "sentence_id", "token_id_int")
    ) %>%
    count_feature_traced("f_29_that_subj")

  # -- f_30  Object relative (que + quien/cual merged) ----------------------
  # MERGE f_30 + f_32 according to biber_espanol_completo.md:
  # Same reasoning as f_29+f_31, for the object position.

  f30_que <- rel_tokens %>%
    dplyr::filter(
      .data$lemma == "que",
      dplyr::coalesce(.data$dep_rel, "") %in% rel_obj_roles
    ) %>%
    dplyr::left_join(
      acl_heads,
      by = c("doc_id", "sentence_id",
             "head_token_id_int" = "acl_head_id")
    ) %>%
    dplyr::inner_join(antecedents,
                      by = c("doc_id", "sentence_id", "antecedent_id"))

  # SCONJ/mark branch for object "que": when the acl:relcl verb has an
  # explicit subject (not pro-dropped), the gap is the object.
  # In practice, f_30 captures quien/cual in object position (detectable)
  # and "que"/SCONJ/mark whose relative clause has an explicit nsubj.
  f30_que_obj <- tokens %>%
    dplyr::filter(
      .data$lemma == "que",
      .data$pos   == "SCONJ",
      dplyr::coalesce(.data$dep_rel, "") == "mark",
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      acl_heads,
      by = c("doc_id", "sentence_id",
             "head_token_id_int" = "acl_head_id")
    ) %>%
    dplyr::filter(!is.na(.data$antecedent_id)) %>%
    dplyr::inner_join(antecedents,
                      by = c("doc_id", "sentence_id", "antecedent_id")) %>%
    # OBJECT only: relatives whose verb has an external nsubj or is 1st/2nd person
    # (pro-dropped subject -> the relative "que" fills the object gap).
    # biber_espanol_completo.md §f_30.
    dplyr::inner_join(
      relcl_obj_ids,
      by = c("doc_id", "sentence_id",
             "head_token_id_int" = "acl_head_id")
    )

  f30_wh <- rel_tokens %>%
    dplyr::filter(
      .data$lemma %in% wh_pronouns,
      dplyr::coalesce(.data$dep_rel, "") %in% rel_obj_roles
    ) %>%
    dplyr::semi_join(
      nominal_relatives,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "acl_head_id")
    )

  f30 <- dplyr::bind_rows(
    f30_que     %>% dplyr::select(dplyr::all_of(rel_evidence_cols)),
    f30_que_obj %>% dplyr::select(dplyr::all_of(rel_evidence_cols)),
    f30_wh      %>% dplyr::select(dplyr::all_of(rel_evidence_cols))
  ) %>%
    dplyr::anti_join(
      pied_piping, by = c("doc_id", "sentence_id", "token_id_int")
    ) %>%
    count_feature_traced("f_30_that_obj")

  # f_31 and f_32 are absorbed into f_29 and f_30 respectively (REVERT at the
  # author's request). No separate columns are generated in the output.

  # -- f_33  Pied-piping (preposition + relative pronoun) --------------------
  # In Spanish the typical pied-piping pattern in UDPipe Spanish-GSD is:
  #   ADP + DET + PRON_rel  (con los que, en el que, por los cuales)
  #   ADP + PRON_rel        (con quien, en cual)
  # Also accept the relative "que" appearing as SCONJ/mark (UDPipe
  # spanish-gsd often tags it that way inside relatives with a preposition).
  # Heuristic: the relative pronoun/conjunction is preceded by an ADP
  # 1 or 2 positions earlier in the same sentence, and the head is a VERB in
  # a relative clause (acl:relcl).
  f33 <- pied_piping %>%
    count_feature_traced("f_33_pied_piping")

  # -- f_34  Sentential relative (lo que, eso que, lo cual) ------------------
  # The antecedent is a neuter pronoun (eso, esto, ello, lo)
  sentence_rel_antecedents <- c("eso", "esto", "ello", "lo")

  f34 <- rel_tokens %>%
    dplyr::filter(
      .data$lemma %in% c("que", "cual", "cuales"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      acl_heads,
      by = c("doc_id", "sentence_id",
             "head_token_id_int" = "acl_head_id")
    ) %>%
    dplyr::left_join(
      tokens %>%
        dplyr::transmute(
          .data$doc_id, .data$sentence_id,
          antecedent_id = .data$token_id_int,
          ant_lemma = .data$lemma
        ),
      by = c("doc_id", "sentence_id", "antecedent_id")
    ) %>%
    dplyr::filter(
      .data$ant_lemma %in% sentence_rel_antecedents
    )

  # Positional heuristic branch (biber_espanol_completo.md §f_34): "comma +
  # lo que / lo cual in clause-initial position" is a strong signal of a
  # sentential relative. Spanish-GSD attaches the verb of the relative with
  # dep_rel=parataxis/advcl/conj (not acl) and "cual"/"que" hangs from that
  # verb, whose head is in turn a VERB (clausal antecedent, not nominal).
  # The preceding-comma requirement excludes free relatives ("Lo que
  # quieras esta bien", no comma, initial).
  verb_heads_lkp <- tokens %>%
    dplyr::transmute(
      .data$doc_id, .data$sentence_id,
      vh_id  = .data$token_id_int,
      vh_pos = .data$pos
    )
  f34_locual <- tokens %>%
    dplyr::arrange(.data$doc_id, .data$sentence_id, .data$token_id_int) %>%
    dplyr::group_by(.data$doc_id, .data$sentence_id) %>%
    dplyr::mutate(
      prev_pos   = dplyr::lag(.data$pos),
      prev_tok   = dplyr::lag(.data$token),
      next_tok   = dplyr::lead(stringr::str_to_lower(.data$token))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      # clitic "lo" is lemmatized as "el" in Spanish-GSD -> use the surface form
      stringr::str_to_lower(.data$token) == "lo",
      dplyr::coalesce(.data$prev_pos, "") == "PUNCT" |
        dplyr::coalesce(.data$prev_tok, "") == ",",
      dplyr::coalesce(.data$next_tok, "") %in% c("cual", "que"),
      !is.na(.data$head_token_id_int)
    ) %>%
    # "lo" hangs from the relative (cual/que); that relative hangs from the verb
    # of the relative clause; that verb hangs from another VERB (antecedent clause)
    dplyr::left_join(
      tokens %>% dplyr::transmute(
        .data$doc_id, .data$sentence_id,
        rel_id = .data$token_id_int,
        rel_head = .data$head_token_id_int
      ),
      by = c("doc_id", "sentence_id", "head_token_id_int" = "rel_id")
    ) %>%
    dplyr::left_join(
      tokens %>% dplyr::transmute(
        .data$doc_id, .data$sentence_id,
        rv_id = .data$token_id_int,
        rv_head = .data$head_token_id_int
      ),
      by = c("doc_id", "sentence_id", "rel_head" = "rv_id")
    ) %>%
    dplyr::left_join(
      verb_heads_lkp,
      by = c("doc_id", "sentence_id", "rv_head" = "vh_id")
    ) %>%
    dplyr::filter(dplyr::coalesce(.data$vh_pos, "") == "VERB")

  f34 <- dplyr::bind_rows(
    f34        %>% dplyr::select(dplyr::all_of(rel_evidence_cols)),
    f34_locual %>% dplyr::select(dplyr::all_of(rel_evidence_cols))
  ) %>%
    count_feature_traced("f_34_sentence_relatives")

  # f_31 and f_32 have been absorbed into f_29 and f_30 respectively
  # (merge per biber_espanol_completo.md); no separate columns are generated.
  counts <- doc_ids %>%
    dplyr::left_join(f29$counts, by = "doc_id") %>%
    dplyr::left_join(f30$counts, by = "doc_id") %>%
    dplyr::left_join(f33$counts, by = "doc_id") %>%
    dplyr::left_join(f34$counts, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )
  evidence <- bind_evidence(f29$evidence, f30$evidence, f33$evidence, f34$evidence)
  make_block_result(counts = counts, evidence = evidence)
}

# -----------------------------------------------------------------------------
# 3.  block_clause_embedding_es   f_21-f_23, f_35-f_38 (f_60 removed)
# -----------------------------------------------------------------------------

#' Complementizer, subordinator, and clause-embedding features (Spanish)
#'
#' f_21  That-complementizador tras VERB ("dijo que", "sabe que")
#' f_22  That-complementizador tras ADJ  ("seguro de que", "feliz de que")
#' f_23  Clausula-wh (relativa/interrogativa indirecta)
#' f_35  Subordinada causal    (solo: porque)
#' f_36  Subordinada concesiva (solo: aunque)
#' f_37  Subordinada condicional (si, a_menos_que, salvo_que)
#' f_38  Otros subordinadores adverbiales (cuando, mientras, antes_de_que?)
#'
#' f_60 (that-deletion) ELIMINADO: intraducible. En espanol el complementante
#' <<que>> es practicamente obligatorio; su omision es agramatical.
#' biber_espanol_completo.md sec. F_60.
#'
#' @param tokens Annotated token data frame
#' @param doc_ids One-column data frame with column `doc_id`
#' @param head_lookup Pre-built head-token attribute table
#' @param dict_lookup Dictionary lookup (para listas de subordinadores)
#' @return Data frame: one row per doc, columns f_21-f_23, f_35-f_38
#' @keywords internal
block_clause_embedding_es <- function(tokens, doc_ids, head_lookup,
                                       dict_lookup = NULL) {

  conditional_hits <- surface_matches(tokens, c("a_menos_que", "salvo_que"))
  conditional_spans <- lapply(seq_len(nrow(conditional_hits)), function(i) {
    seq.int(conditional_hits$start_row[i], conditional_hits$end_row[i])
  })
  marked <- vapply(conditional_spans, function(rows) {
    any(stringr::str_detect(dplyr::coalesce(tokens$dep_rel[rows], ""), "^mark"))
  }, logical(1))
  conditional_spans <- conditional_spans[marked]
  loc_cond <- tokens[conditional_hits$start_row[marked], ]
  loc_cond$token <- vapply(conditional_spans, function(rows) {
    paste(tokens$token[rows], collapse = " ")
  }, character(1))
  loc_exclude <- tokens[unique(as.integer(unlist(conditional_spans))), ] %>%
    dplyr::select("doc_id", "sentence_id", "token_id_int")

  # -- f_21  <<que>> complementizer after a VERB -----------------------------
  # UD conditions:
  #   - token lemma = "que", pos = SCONJ
  #   - dep_rel = mark
  #   - the head of the mark has pos = VERB or AUX
  # dep_rel table of the head: tells complements (ccomp/xcomp/...) from
  # relatives (acl:relcl). biber_espanol_completo.md §f_21 (EXCLUDE): "que"
  # as a relative pronoun in acl:relcl goes to f_29/f_30, NOT to f_21.
  head_deprel <- tokens %>%
    dplyr::transmute(
      .data$doc_id, .data$sentence_id,
      head_token_id_int = .data$token_id_int,
      head_dep_rel = .data$dep_rel
    )

  # REVISION (verified empirically): "que" (SCONJ, mark) ALWAYS has as its
  # head the VERB of its own subordinate clause (V) -- never
  # the matrix predicate directly. V is in turn csubj/advcl/ccomp/xcomp
  # of that real predicate (VERB or ADJECTIVE), which is the "grandparent" of "que".
  # The distinction f_21 (VERB complement) vs f_22 (ADJ complement)
  # depends on the POS of the GRANDPARENT, not of the immediate head of "que" (which
  # is always a VERB and so distinguishes nothing). Without this second level, f_21
  # absorbed ALL the adjectival cases (verified: "Es importante que
  # vengas" -> it used to count in f_21, never in f_22) and f_22 always stayed
  # at 0. See TABLA_RASGOS_ES.md §f_21/f_22.
  grandparent_lookup <- tokens %>%
    dplyr::transmute(
      .data$doc_id, .data$sentence_id,
      v_id  = .data$token_id_int,
      gp_id = .data$head_token_id_int
    )
  gp_pos_lookup <- tokens %>%
    dplyr::transmute(
      .data$doc_id, .data$sentence_id,
      gp_id  = .data$token_id_int,
      gp_pos = .data$pos
    )

  que_mark_base <- tokens %>%
    dplyr::filter(
      .data$lemma == "que",
      .data$pos   == "SCONJ",
      dplyr::coalesce(.data$dep_rel, "") == "mark",
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::anti_join(loc_exclude, by = c("doc_id", "sentence_id", "token_id_int")) %>%
    dplyr::left_join(
      head_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::left_join(
      head_deprel,
      by = c("doc_id", "sentence_id", "head_token_id_int")
    ) %>%
    dplyr::left_join(
      grandparent_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "v_id")
    ) %>%
    dplyr::left_join(
      gp_pos_lookup,
      by = c("doc_id", "sentence_id", "gp_id")
    )

  f21 <- que_mark_base %>%
    dplyr::filter(
      dplyr::coalesce(.data$head_pos, "") %in% c("VERB", "AUX"),
      stringr::str_detect(dplyr::coalesce(.data$head_dep_rel, ""),
                          "^(ccomp|xcomp|csubj)($|:)"),
      # NEW: the grandparent (real predicate) must be a VERB/AUX, not an ADJECTIVE.
      dplyr::coalesce(.data$gp_pos, "") %in% c("VERB", "AUX")
    ) %>%
    count_feature_traced("f_21_that_verb_comp")

  # -- f_22  <<que>> complementizer after an ADJ -----------------------------
  # The real adjective lives in the GRANDPARENT of "que" (see the note above), not in
  # its immediate head.
  f22 <- que_mark_base %>%
    dplyr::filter(
      !stringr::str_detect(dplyr::coalesce(.data$head_dep_rel, ""), "^acl"),
      dplyr::coalesce(.data$gp_pos, "") == "ADJ"
    ) %>%
    count_feature_traced("f_22_that_adj_comp")

  # -- f_23  Wh-clause (indirect question) -----------------------------------
  # Spec section f_23 Include: only ACCENTED wh-words in subordinate/argument position.
  # Spec section f_23 Exclude: "Relative uses of the same words without accents".
  #
  # UDPipe spanish-gsd marks relative "que" as PronType=Int,Rel
  # (ambivalent) and sometimes normalizes the accent in the lemma. Therefore:
  # (a) Require an accent in the SURFACE FORM of the token (not only in the lemma).
  # (b) Exclude heads whose dep_rel is acl/acl:relcl (clearly relatives).
  wh_adv_tilde <- c("d\u00f3nde", "cu\u00e1ndo", "c\u00f3mo",
                    "cu\u00e1nto", "cu\u00e1nta",
                    "cu\u00e1ntos", "cu\u00e1ntas")
  wh_pron_tilde <- c("qu\u00e9", "qui\u00e9n", "qui\u00e9nes",
                     "cu\u00e1l", "cu\u00e1les")
  # Unaccented lemmas we accept ONLY if the surface token carries an accent
  # (makes up for the inconsistent lemmatization of UDPipe spanish-gsd).
  wh_pron_lemmas <- c(wh_pron_tilde,
                      "que", "quien", "quienes", "cual", "cuales")

  is_int_pron <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("PRON", "DET", "ADJ"),
      .data$lemma %in% wh_pron_lemmas,
      stringr::str_detect(
        dplyr::coalesce(.data$feats, ""), "PronType=[^|]*Int"
      ),
      # Accent in the surface form -- discards relative "que/quien/cual"
      tolower(.data$token) %in% wh_pron_tilde |
        .data$lemma %in% wh_pron_tilde
    )

  is_int_adv <- tokens %>%
    dplyr::filter(
      .data$pos == "ADV",
      .data$lemma %in% wh_adv_tilde
    )

  # dep_rel of the verb heading the wh-clause: if it is "root" the clause
  # is the main clause -> DIRECT question (goes to f_13, NOT to f_23). The indirect
  # question has the verb subordinated (ccomp/xcomp/advcl/acl...).
  # biber_espanol_completo.md §f_23 (EXCLUDE): "Direct questions -> f_13".
  wh_head_deprel <- tokens %>%
    dplyr::transmute(
      .data$doc_id, .data$sentence_id,
      head_token_id_int = .data$token_id_int,
      wh_head_dep = .data$dep_rel
    )

  f23 <- dplyr::bind_rows(is_int_pron, is_int_adv) %>%
    dplyr::filter(
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""),
        "^(obj|obl|nsubj|iobj|advmod|nmod|ccomp)"
      ),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      head_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::left_join(
      wh_head_deprel,
      by = c("doc_id", "sentence_id", "head_token_id_int")
    ) %>%
    dplyr::filter(
      dplyr::coalesce(.data$head_pos, "") %in% c("VERB", "AUX", "ADJ"),
      dplyr::coalesce(.data$wh_head_dep, "") != "root",
      # Spec §f_23 Exclude: relatives. UDPipe marks the head as acl/acl:relcl
      # when the wh-word introduces a relative (case "El equipo que fue asignado").
      !dplyr::coalesce(.data$wh_head_dep, "") %in% c("acl", "acl:relcl")
    ) %>%
    count_feature_traced("f_23_wh_clause")

  # -- f_35  Causal ----------------------------------------------------------
  # Only "porque" -- a narrow lexical restriction parallel to "because" in Biber
  # (1985), which does not include causal since/as either. Excluded: ya_que,
  # puesto_que, dado_que, pues, causal como. biber_espanol_completo.md F_35.
  causal_lemmas <- c("porque")

  f35 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% causal_lemmas,
      .data$pos   %in% c("SCONJ", "CCONJ", "ADV"),
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""), "^(mark|cc|advmod)"
      )
    ) %>%
    count_feature_traced("f_35_because")

  # -- f_36  Concessive ------------------------------------------------------
  # Only "aunque" -- covers both "although" and "though" of Biber (1985).
  # Excluded: si_bien, aun_cuando, a_pesar_de_que, pese_a_que.
  # biber_espanol_completo.md F_36.
  concessive_lemmas <- c("aunque")

  f36 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% concessive_lemmas,
      .data$pos   %in% c("SCONJ", "CCONJ", "ADV"),
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""), "^(mark|cc|advmod)"
      )
    ) %>%
    count_feature_traced("f_36_though")

  # -- f_37  Conditional -----------------------------------------------------
  # Conditional "si" + "a_menos_que" and "salvo_que" as equivalents of
  # "unless". Excluded: en_caso_de_que, siempre_que, siempre_y_cuando,
  # con_tal_de_que (structurally parallel to excluding provided/as_long_as
  # in Biber). biber_espanol_completo.md F_37.
  conditional_lemmas <- c("si", "a_menos_que", "salvo_que")

  # biber_espanol_completo.md §f_37 (critical EXCLUDE): "si" as an
  # indirect-question complementizer does NOT count. The discriminator is the
  # governing verb: if the grandparent of "si" (head of the subordinate verb that
  # "si" marks) is a verb of communication/cognition -> indirect question
  # ("pregunto si vendria", "no se si vino"), it is excluded.
  interrog_governors <- c(
    "preguntar", "saber", "ignorar", "dudar", "desconocer", "averiguar",
    "comprobar", "ver", "decidir", "recordar", "contar", "decir",
    "explicar", "entender", "comprender", "olvidar", "consultar",
    "investigar", "examinar", "determinar", "aclarar", "cuestionar"
  )
  # Table subordinate verb -> its head (grandparent). Keys without collision:
  # mid_id  = id of the verb that "si" marks (head of "si")
  # gp_id   = head of that verb (grandparent of "si")
  mid_to_gp <- tokens %>%
    dplyr::transmute(
      .data$doc_id, .data$sentence_id,
      mid_id = .data$token_id_int,
      gp_id  = .data$head_token_id_int,
      mid_dep_rel = .data$dep_rel
    )
  gp_lemma_tbl <- tokens %>%
    dplyr::transmute(
      .data$doc_id, .data$sentence_id,
      gp_id    = .data$token_id_int,
      gp_lemma = .data$lemma
    )

  f37_evidence_cols <- c("doc_id", "sentence_id", "token_id_int",
                         "token", "lemma", "pos", "feats", "head_token_id_int")
  f37_si <- tokens %>%
    dplyr::filter(
      .data$lemma %in% conditional_lemmas,
      .data$pos   %in% c("SCONJ", "ADV"),
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""), "^mark"
      )
    ) %>%
    dplyr::mutate(mid_id = .data$head_token_id_int) %>%
    dplyr::left_join(mid_to_gp,
                     by = c("doc_id", "sentence_id", "mid_id")) %>%
    dplyr::left_join(gp_lemma_tbl,
                     by = c("doc_id", "sentence_id", "gp_id")) %>%
    dplyr::filter(
      !(.data$lemma == "si" &
          dplyr::coalesce(.data$gp_lemma, "") %in% interrog_governors &
          stringr::str_detect(dplyr::coalesce(.data$mid_dep_rel, ""),
                              "^(ccomp|xcomp)($|:)"))
    )

  f37 <- dplyr::bind_rows(
    f37_si   %>% dplyr::select(dplyr::all_of(f37_evidence_cols)),
    loc_cond %>% dplyr::select(dplyr::all_of(f37_evidence_cols))
  ) %>%
    count_feature_traced("f_37_if")

  # -- f_38  Other adverbial subordinators -----------------------------------
  # Every SCONJ with dep_rel=mark NOT already counted in f_21/f_22/f_35/f_36/f_37
  counted_sub <- unique(c(
    "que",
    causal_lemmas, concessive_lemmas, conditional_lemmas
  ))

  # biber_espanol_completo.md §f_38 includes "mientras" (and mientras_que) as an
  # adverbial subordinator. Spanish-GSD tags it inconsistently
  # as SCONJ or CCONJ (in "mientras que" -> mientras=CCONJ/mark, que=SCONJ/
  # fixed). A CCONJ branch restricted to an allowlist of subordinating lemmas
  # is added so that coordinators (y/o/pero/ni) are not captured.
  adv_sub_cconj <- c("mientras", "conforme", "seg\u00fan", "segun")

  f38 <- tokens %>%
    dplyr::filter(
      (
        .data$pos %in% c("SCONJ", "ADP", "ADV") &
          stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^mark")
      ) | (
        .data$pos == "CCONJ" &
          stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^mark") &
          stringr::str_to_lower(.data$lemma) %in% adv_sub_cconj
      ),
      !.data$lemma %in% counted_sub
    ) %>%
    # HERNAN REVIEW (Phase 5): exclude the components of 'a menos que' /
    # 'salvo que' (already counted in f_37), which would otherwise slip in here as mark.
    dplyr::anti_join(
      loc_exclude,
      by = c("doc_id", "sentence_id", "token_id_int")
    ) %>%
    count_feature_traced("f_38_other_adv_sub")

  # f_60 (that-deletion) REMOVED: untranslatable.
  # See the docstring and biber_espanol_completo.md sec. F_60.

  counts <- doc_ids %>%
    dplyr::left_join(f21$counts, by = "doc_id") %>%
    dplyr::left_join(f22$counts, by = "doc_id") %>%
    dplyr::left_join(f23$counts, by = "doc_id") %>%
    dplyr::left_join(f35$counts, by = "doc_id") %>%
    dplyr::left_join(f36$counts, by = "doc_id") %>%
    dplyr::left_join(f37$counts, by = "doc_id") %>%
    dplyr::left_join(f38$counts, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )

  evidence <- bind_evidence(
    f21$evidence, f22$evidence, f23$evidence,
    f35$evidence, f36$evidence, f37$evidence, f38$evidence
  )

  make_block_result(counts = counts, evidence = evidence)
}
