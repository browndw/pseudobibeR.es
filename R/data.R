#' Spanish lexical feature dictionary
#'
#' A `quanteda::dictionary` whose named entries contain lower-case Spanish
#' words and underscore-separated phrases. Structural detectors use lemma
#' lists; dictionary-only features and adverbial phrases use surface matches.
#' @format A dictionary with one entry per lexical feature.
#' @seealso [biber()]
"dict"

#' Spanish support word lists
#'
#' Lists used by Spanish feature detectors, including nominalization suffixes,
#' demonstratives, negation particles, and multiword patterns.
#' @format A named list of character vectors.
#' @seealso [biber()]
"word_lists"

#' Spanish examples illustrating linguistic features
#'
#' Hand-specified examples and expected counts. These are linguistic
#' expectations rather than guarantees of parser accuracy.
#' @format A tibble with `feature`, `example`, and numeric `count` columns.
"spanish_examples"

#' Annotated Spanish sample documents
#'
#' Original sample sentences annotated with UDPipe Spanish-GSD
#' `spanish-gsd-ud-2.5-191206`. The native annotation object can be passed
#' directly to [biber()] and [audit_features()].
#' @format A `udpipe_connlu` annotation object for three Spanish documents.
"udpipe_samples"
