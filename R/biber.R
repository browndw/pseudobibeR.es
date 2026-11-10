#' Extract Biber features from annotated Spanish text
#'
#' @description
#' Counts Spanish adaptations of Biber's lexicogrammatical features from
#' Universal Dependencies annotations. Parsing is performed separately.
#'
#' @param tokens An annotation returned by `udpipe::udpipe_annotate()` or
#'   `spacyr::spacy_parse()`. A data frame with either parser's columns is
#'   also accepted. Required columns are `doc_id`, `sentence_id`, `token_id`,
#'   `token`, `lemma`, `head_token_id`, `dep_rel`, and `upos`/`xpos` or
#'   `pos`/`tag`. Morphology is supplied in `feats` or `morph`. With
#'   `spacyr::spacy_parse()`, set `dependency = TRUE`, `tag = TRUE`,
#'   `pos = TRUE`, and `additional_attributes = "morph"`.
#' @param measure Lexical diversity measure: `"MATTR"`, `"TTR"`, `"CTTR"`,
#'   `"MSTTR"`, or `"none"`. The latter omits `f_43_type_token`.
#'   MATTR is the default. If any document has fewer than 200 retained tokens,
#'   MATTR falls back to TTR for the entire corpus, with one message.
#'   Explicitly selected TTR, CTTR, and MSTTR are retained for all documents.
#' @param normalize Logical. Normalize counts per 1,000 UD word tokens,
#'   excluding punctuation, symbols, spaces, digit-final numeric tokens
#'   (`42`), and multiword-token container rows. Function words and
#'   spelled-out numerals are included. Metrics are never normalized.
#'
#' @param feature_names `"language"` (default) to name columns with the
#'   Spanish display names in [feature_names], or `"shared"` for the identifiers
#'   shared by the English, Spanish and French packages. Set
#'   `options(pseudobibeR.feature_names = "shared")` to change the default.
#' @return A data frame with one row per document: `doc_id` and the 67
#'   feature columns (66 when `measure = "none"`). The compatibility feature
#'   f_28_present_participle_whiz is always zero: standard Spanish has no
#'   productive postnominal gerund functioning as a reduced relative.
#' @details
#' Feature identifiers match the English and French companion packages,
#' but Spanish heuristics are language-specific. Tense features count finite
#' verbs and auxiliaries in any mood: f_03 includes the present subjunctive,
#' as f_01 includes the imperfect subjunctive. `f_09_pronoun_it` counts identified
#' impersonal predicates, anchored on existing words, not inferred null subjects
#' or UD empty nodes. Existential haber stays in f_20; personal pro-drop,
#' generic complements, ambiguous se, and neuter lo are outside the f_09 baseline.
#' `f_12_proverb_do` counts lexical hacer with action-proform lo (including
#' enclitics) or lo mismo, not auxiliary/copular, causative, reflexive or overt
#' nominal-object uses. Bare lo is structurally eligible without semantic
#' antecedent resolution. Each occurrence is anchored on the supplied verb word;
#' pronoun/noun counts and token populations remain independent.
#' `f_15_gerunds` counts infinitives functioning as nouns: clear noun-like
#' nominal infinitives (`el saber`, `su andar`) and infinitives heading a subject
#' placed before their verb (`Fumar perjudica`). Postposed subjects (`Es
#' importante descansar`), objects and prepositional infinitives remain f_24.
#' Infinitive/verb identity is required; endings or an article alone are
#' insufficient. Qualifying occurrences are
#' excluded from f_14, f_16 and f_24 by word identity, without retagging input.
#' Verbal infinitival clauses remain in f_24 when their source annotations
#' qualify. Recognized nominal subjects do not license impersonal f_09.
#' Nominal relatives are partitioned by marker family and role: que in
#' f_29/f_30, quien/cual forms in f_31/f_32. Pied-piping f_33 takes exclusive
#' precedence over other relative categories. Explicit roles receive bounded
#' conflict/agreement checks without retagging; finite appos attachments can be
#' recovered with relative morphology. Roleless que needs positive predicate
#' evidence or an external subject, not merely absence of another subject.
#' Ambiguous role gaps remain unclassified, so a zero can reflect parser limits.
#' `f_59_contractions` counts recognized colloquial surface reductions, including
#' native apostrophe splits, once per longest expression with full-span evidence.
#' Standard al/del, nominal determiner contexts and short nominal acronyms are
#' excluded. Source POS/lemmas and word populations are not rewritten, even when
#' models mis-tag reduction fragments or apostrophes.
#' `f_60_that_deletion` measures bounded complement reduction under creer,
#' pensar, esperar, decir, afirmar and asegurar (D-46): infinitival or unmarked finite ccomp/xcomp clauses,
#' including clear coordination and auxiliary/copular clause-form projection.
#' Overt introducers, interrogative complements and nominal infinitives are
#' excluded. Each identified clause head contributes one occurrence, without
#' reconstructing deleted material or retagging input. Infinitival overlap with
#' the independent form measure f_24 is intentional. This is not every
#' infinitive or a claim of register/semantic equivalence across languages.
#' Missing governor/complement analyses can produce false negatives.
#' `f_62_split_infinitive` counts adverb-interrupted preposition-infinitive units
#' once per matched span, requiring a linked verbal infinitive and at least one
#' non-negative adverb. Negation-only interruptions are excluded; negative ADV/PART
#' may accompany a qualifying adverb. Consecutive adverbs have no arbitrary
#' token cap. Subjects, punctuation breaks and nominal infinitives are excluded.
#' Evidence/audits preserve the full span anchored at its introductory preposition.
#' Bounded mejor/siquiera lookup supports known mistaggings without retagging;
#' lost marker attachments can cause misses. f_24/f_63 remain independent.
#' `f_61_stranded_preposition` measures bounded resumptive prepositional
#' relatives in Spanish, not literal stranding. Initial hablar-de, depender-de
#' and confiar-en frames require a nominal que relative, a finite predicate,
#' separate-subject/first-or-second-person evidence, and a postverbal linked
#' third-person tonic pronoun PP with no known agreement conflict or bare object.
#' One occurrence per relative clause retains the first PP span in audits.
#' Detected resumptive markers leave f_29/f_30; f_33 and other relative bins
#' are unchanged. Lookup-only first-person preterite recovery for hablar/confiar
#' uses exact accented forms in word_lists, without rewriting input or metrics.
#' This is bounded structural evidence, not unrestricted coreference resolution.
#' Counts depend on parser accuracy. Integration tests cover UDPipe Spanish-GSD and
#' spaCy `es_core_news_sm`, not general cross-parser equivalence. Native spaCy
#' root/passive conventions are supported. Tokenization and model errors can
#' produce different counts and normalization denominators across parsers.
#' Lexical diversity uses lower-case surface-plus-tag types, not lemmas.
#' Fine-grained parser tags are retained; missing or `"_"` tags use UPOS.
#' Punctuation, symbols, and spaces are excluded using UD POS labels.
#' The English numeric pattern `\\d_` removes numeric matches from the
#' tagged stream, including digit-final surface forms; spelled-out numerals
#' remain words. Dictionary phrases are not compounded for this metric.
#' Retained tokens determine the 200-token fallback threshold, rather than
#' the normalization denominator, which still includes spelled-out numerals.
#' One lexical-diversity measure is used throughout each supplied corpus.
#' For MSTTR, `quanteda.textstats` may shorten the common segment length
#' for short documents and issues a warning. Mean word length (f_44) uses purely alphabetic UD word tokens, unrounded;
#' documents without any receive `NA`.
#' Documents with no retained diversity tokens receive `NA` for
#' `f_43_type_token`, with a warning identifying those documents.
#' Documents retain their order of first appearance.
#'
#' @seealso [audit_features()], [dict], [word_lists]
#' @references Biber, D. (1988). *Variation across speech and writing*.
#'   Cambridge University Press. \doi{10.1017/CBO9780511621024}
#' @examples
#' biber(udpipe_samples, measure = "TTR", normalize = FALSE)
#' @export
biber <- function(tokens, measure = c("MATTR", "TTR", "CTTR", "MSTTR", "none"),
                  normalize = TRUE, feature_names = getOption("pseudobibeR.feature_names", "language")) {
  UseMethod("biber")
}

#' @rdname biber
#' @export
biber.udpipe_connlu <- function(tokens,
                               measure = c("MATTR", "TTR", "CTTR", "MSTTR", "none"),
                               normalize = TRUE,
                               feature_names = getOption("pseudobibeR.feature_names", "language")) {
  feature_names <- match_feature_names(feature_names)
  out <- extract_spanish_features(tokens, match.arg(measure), normalize, "udpipe")
  names(out) <- display_feature_names(names(out), feature_names)
  out
}

#' @rdname biber
#' @export
biber.spacyr_parsed <- function(tokens,
                               measure = c("MATTR", "TTR", "CTTR", "MSTTR", "none"),
                               normalize = TRUE,
                               feature_names = getOption("pseudobibeR.feature_names", "language")) {
  feature_names <- match_feature_names(feature_names)
  out <- extract_spanish_features(tokens, match.arg(measure), normalize, "spacy")
  names(out) <- display_feature_names(names(out), feature_names)
  out
}


extract_spanish_features <- function(tokens, measure, normalize, engine, traced = FALSE) {
  if (!is.logical(normalize) || length(normalize) != 1L || is.na(normalize)) {
    stop("'normalize' must be TRUE or FALSE.", call. = FALSE)
  }
  old_ctype <- Sys.getlocale("LC_CTYPE")
  if (!isTRUE(l10n_info()[["UTF-8"]])) {
    on.exit(Sys.setlocale("LC_CTYPE", old_ctype), add = TRUE)
    for (locale in c("en_US.UTF-8", "C.UTF-8", "es_ES.UTF-8")) {
      if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", locale)))) break
    }
    if (!isTRUE(l10n_info()[["UTF-8"]])) {
      stop("A UTF-8 locale is required for Spanish feature extraction.", call. = FALSE)
    }
  }
  tokens <- prepare_parsed_tokens(tokens)
  parse_biber_features(tokens, measure, normalize, engine, traced = traced)
}
