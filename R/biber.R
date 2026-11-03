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
#'   MATTR and MSTTR fall back to TTR separately for documents shorter
#'   than 200 tokens, with a message.
#' @param normalize Logical. Normalize counts per 1,000 UD word tokens,
#'   excluding punctuation, symbols, spaces, and multiword-token container
#'   rows. Function words are included. Metrics are never normalized.
#'
#' @return A data frame with one row per document, 67 feature columns
#'   (66 when `measure = "none"`), and `n_tokens` and `n_lex_tokens`.
#'   `n_tokens` is the normalization denominator. Lexical tokens are nouns,
#'   proper nouns, verbs, adjectives, and adverbs. Ten compatibility
#'   features that are unavailable or merged in Spanish are always zero.
#' @details
#' Feature identifiers match the English and French companion packages,
#' but Spanish heuristics are language-specific. Counts depend on the
#' accuracy of the parser. Integration tests cover UDPipe Spanish-GSD and
#' spaCy `es_core_news_sm`, not general cross-parser equivalence. Native spaCy
#' root/passive conventions are supported. Tokenization and model errors can
#' produce different counts and normalization denominators across parsers.
#' Lexical diversity uses lower-case surface tokens, including punctuation,
#' before phrase compounding. Mean word length uses UD word tokens.
#' Documents retain their order of first appearance.
#'
#' @seealso [audit_features()], [dict], [word_lists]
#' @references Biber, D. (1988). *Variation across speech and writing*.
#'   Cambridge University Press. \doi{10.1017/CBO9780511621024}
#' @examples
#' biber(udpipe_samples, measure = "TTR", normalize = FALSE)
#' @export
biber <- function(tokens, measure = c("MATTR", "TTR", "CTTR", "MSTTR", "none"),
                  normalize = TRUE) {
  UseMethod("biber")
}

#' @rdname biber
#' @export
biber.udpipe_connlu <- function(tokens,
                               measure = c("MATTR", "TTR", "CTTR", "MSTTR", "none"),
                               normalize = TRUE) {
  extract_spanish_features(tokens, match.arg(measure), normalize, "udpipe")
}

#' @rdname biber
#' @export
biber.spacyr_parsed <- function(tokens,
                               measure = c("MATTR", "TTR", "CTTR", "MSTTR", "none"),
                               normalize = TRUE) {
  extract_spanish_features(tokens, match.arg(measure), normalize, "spacy")
}

#' @rdname biber
#' @export
biber.data.frame <- function(tokens,
                            measure = c("MATTR", "TTR", "CTTR", "MSTTR", "none"),
                            normalize = TRUE) {
  engine <- if ("upos" %in% names(tokens)) "udpipe" else "spacy"
  extract_spanish_features(tokens, match.arg(measure), normalize, engine)
}

#' @export
biber.default <- function(tokens,
                         measure = c("MATTR", "TTR", "CTTR", "MSTTR", "none"),
                         normalize = TRUE) {
  stop("'tokens' must be UDPipe annotations, spacyr annotations, or a data frame.",
       call. = FALSE)
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
