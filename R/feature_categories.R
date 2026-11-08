#' Feature -> Biber category mapping (internal)
#'
#' Named vector that maps each of the 67 column names of the
#' \code{biber()} output to its Biber (1988) category in Spanish.
#' Used to order the output and select audit categories.
#'
#' @keywords internal
#' @noRd
.feature_categories <- c(
  # A. Tense and aspect
  f_01_past_tense              = "A. Tiempo y aspecto",
  f_02_perfect_aspect          = "A. Tiempo y aspecto",
  f_03_present_tense           = "A. Tiempo y aspecto",
  # B. Place and time adverbials
  f_04_place_adverbials        = "B. Adverbiales de lugar y tiempo",
  f_05_time_adverbials         = "B. Adverbiales de lugar y tiempo",
  # C. Pronouns (includes the pro-verb do in Biber 1988)
  f_06_first_person_pronouns   = "C. Pronombres",
  f_07_second_person_pronouns  = "C. Pronombres",
  f_08_third_person_pronouns   = "C. Pronombres",
  f_09_pronoun_it              = "C. Pronombres",
  f_10_demonstrative_pronoun   = "C. Pronombres",
  f_11_indefinite_pronouns     = "C. Pronombres",
  f_12_proverb_do              = "C. Pronombres",
  # D. Questions
  f_13_wh_question             = "D. Interrogativas",
  # E. Nominal forms
  f_14_nominalizations         = "E. Formas nominales",
  f_15_gerunds                 = "E. Formas nominales",
  f_16_other_nouns             = "E. Formas nominales",
  # F. Passives
  f_17_agentless_passives      = "F. Pasivas",
  f_18_by_passives             = "F. Pasivas",
  # G. Stative forms
  f_19_be_main_verb            = "G. Formas estativas",
  f_20_existential_there       = "G. Formas estativas",
  # H. Subordination
  f_21_that_verb_comp          = "H. Subordinacion",
  f_22_that_adj_comp           = "H. Subordinacion",
  f_23_wh_clause               = "H. Subordinacion",
  f_24_infinitives             = "H. Subordinacion",
  f_25_present_participle      = "H. Subordinacion",
  f_26_past_participle         = "H. Subordinacion",
  f_27_past_participle_whiz    = "H. Subordinacion",
  f_28_present_participle_whiz = "H. Subordinacion",
  f_29_that_subj               = "H. Subordinacion",
  f_30_that_obj                = "H. Subordinacion",
  f_31_wh_subj                 = "H. Subordinacion",
  f_32_wh_obj                  = "H. Subordinacion",
  f_33_pied_piping             = "H. Subordinacion",
  f_34_sentence_relatives      = "H. Subordinacion",
  f_35_because                 = "H. Subordinacion",
  f_36_though                  = "H. Subordinacion",
  f_37_if                      = "H. Subordinacion",
  f_38_other_adv_sub           = "H. Subordinacion",
  # I. Prepositional phrases, adjectives and adverbs
  f_39_prepositions            = "I. Sintagmas prep., adj. y adv.",
  f_40_adj_attr                = "I. Sintagmas prep., adj. y adv.",
  f_41_adj_pred                = "I. Sintagmas prep., adj. y adv.",
  f_42_adverbs                 = "I. Sintagmas prep., adj. y adv.",
  # J. Lexical specificity (metrics, not counts)
  f_43_type_token              = "J. Especificidad lexica",
  f_44_mean_word_length        = "J. Especificidad lexica",
  # K. Lexical classes
  f_45_conjuncts               = "K. Clases lexicas",
  f_46_downtoners              = "K. Clases lexicas",
  f_47_hedges                  = "K. Clases lexicas",
  f_48_amplifiers              = "K. Clases lexicas",
  f_49_emphatics               = "K. Clases lexicas",
  f_50_discourse_particles     = "K. Clases lexicas",
  f_51_demonstratives          = "K. Clases lexicas",
  # L. Modals
  f_52_modal_possibility       = "L. Modales",
  f_53_modal_necessity         = "L. Modales",
  f_54_modal_predictive        = "L. Modales",
  # M. Specialized verbs
  f_55_verb_public             = "M. Verbos especializados",
  f_56_verb_private            = "M. Verbos especializados",
  f_57_verb_suasive            = "M. Verbos especializados",
  f_58_verb_seem               = "M. Verbos especializados",
  # N. Reduced forms
  f_59_contractions            = "N. Formas reducidas",
  f_60_that_deletion           = "N. Formas reducidas",
  f_61_stranded_preposition    = "N. Formas reducidas",
  f_62_split_infinitive        = "N. Formas reducidas",
  f_63_split_auxiliary         = "N. Formas reducidas",
  # O. Coordination
  f_64_phrasal_coordination    = "O. Coordinacion",
  f_65_clausal_coordination    = "O. Coordinacion",
  # P. Negation
  f_66_neg_synthetic           = "P. Negacion",
  f_67_neg_analytic            = "P. Negacion"
)

#' The compatibility-zero feature (internal)
#'
#' Features that appear as columns in the \code{biber()} output for
#' surface parity with \code{pseudobibeR.fr} but whose value is always
#' 0 because detection is unavailable or its Spanish scope remains pending.
#' Documented in the Spanish feature table and README.
#'
#' Used by \code{biber_es_traced()} and the tests to tell apart
#' "detectable feature that gave count=0 in this text" (genuine absence) and
#' "zero-output feature by contract" (there is no detector to run).
#'
#' @keywords internal
#' @noRd
.zero_output_features <- c(
  "f_28_present_participle_whiz"
)

#' List of the 2 metric features (internal)
#'
#' Features whose value in \code{biber()} is not a token count but
#' a metric computed over the whole document:
#' \itemize{
#'   \item \code{f_43_type_token}: type/token ratio (MATTR, TTR, CTTR or MSTTR
#'         depending on the \code{measure} argument).
#'   \item \code{f_44_mean_word_length}: mean length in characters of
#'         all tokens except punctuation.
#' }
#'
#' Metrics are document summaries rather than occurrence counts and are
#' not available through \code{audit_features()}.
#'
#' @keywords internal
#' @noRd
.metric_features <- c(
  "f_43_type_token",
  "f_44_mean_word_length"
)

#' List of the 66 features with real detection (internal)
#'
#' Derived by difference: 67 total - 1 zero-output = 66 detectable.
#' Includes the 2 metric features (f_43, f_44). For the pure set of
#' "features that generate evidence rows", also subtract \code{.metric_features}.
#'
#' Used by the cross-cutting coverage test to iterate over all
#' the features that must generate non-empty evidence when count > 0.
#'
#' @keywords internal
#' @noRd
.detectable_features <- setdiff(names(.feature_categories), .zero_output_features)

#' Features that generate evidence (internal)
#'
#' 64 features: detectable - metrics. Each one must satisfy the
#' invariant \code{count == nrow(filter(evidence, feature == X))} when
#' count > 0, including dictionary and mixed-source detections.
#'
#' @keywords internal
#' @noRd
.evidence_features <- setdiff(.detectable_features, .metric_features)

#' Mixed-source features (internal)
#'
#' Dictionary phrase spans replace their covered grammatical detections.
#' Counts come from the deduplicated combined evidence, never from adding
#' or taking maxima of document-level totals.
#'
#' @keywords internal
#' @noRd
.dual_path_features <- c(
  "f_04_place_adverbials",
  "f_05_time_adverbials"
)

#' Dict-only features: only the dict path produces the count (internal)
#'
#' Category K (f_45-f_50) uses sentence-local surface dictionary spans.
#' Each matched word or phrase produces one evidence occurrence.
#'
#' @keywords internal
#' @noRd
.dict_only_features <- c(
  "f_45_conjuncts",
  "f_46_downtoners",
  "f_47_hedges",
  "f_48_amplifiers",
  "f_49_emphatics",
  "f_50_discourse_particles"
)

#' Features with a known UDPipe limitation (internal)
#'
#' Features whose detector is correct but which UDPipe spanish-gsd under-reports
#' because of tagging/parsing errors in specific constructions. The
#' count/evidence invariant still holds logically (when UDPipe gets it
#' right), but the cross-cutting test cannot assume count >= 1 on a
#' synthetic example because the parser already returned 0 before the
#' detector could weigh in.
#'
#' The cross-cutting test skips positive parser assertions with an explicit reason.
#'
#' @keywords internal
#' @noRd
.udpipe_limited_features <- c(
  # f_22 left this list (Hernan's review): it was not a parser limitation
  # but a bug in the rule (it checked the immediate head of "que",
  # which is never an ADJ). Fixed by examining the head of the subordinate verb.
  "f_26_past_participle",
  # f_27 has the same problem as f_26: UDPipe spanish-gsd tags the
  # postnominal participle inconsistently (ADJ without VerbForm in
  # some cases, VERB with dep_rel "amod" in others). The detector is
  # correct when UDPipe gets it right, but we cannot force count >= 1
  # on a generic synthetic example.
  "f_27_past_participle_whiz"
)

#' Structural-only evidence features (internal)
#'
#' Complement: \code{.evidence_features - .dual_path_features - .dict_only_features}.
#' These are the features generated exclusively by a structural
#' \code{block_*_es()}, with no dict contribution, so their block evidence
#' captures 100% of the count.
#'
#' The count/evidence equality also holds for dictionary and mixed-source
#' features; this set records detection provenance only.
#'
#' @keywords internal
#' @noRd
.strict_evidence_features <- setdiff(
  .evidence_features,
  c(.dual_path_features, .dict_only_features)
)
