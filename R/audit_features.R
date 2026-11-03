#' Audit feature occurrences in annotated Spanish text
#'
#' @param tokens Annotated tokens, as accepted by [biber()].
#' @param feature One or more canonical feature identifiers.
#' @param category A category letter (`"A"` through `"P"`) or its label in
#'   `.feature_categories`. Supply exactly one of `feature` and `category`.
#' @param sample_n Optional positive integer limiting the total number of rows.
#' @param seed Optional non-negative integer random seed. The caller's random
#'   state is restored after sampling.
#' @param max_per_doc Optional positive integer limiting rows per document.
#' @param window Non-negative integer number of context tokens on each side.
#'
#' @return An `audit_features_result` tibble with the French companion's
#'   columns: `feature`, `category`, `doc_id`, `sentence_id`, `count`,
#'   `token_count`, `match_type`, `left`, `keyword`, `right`, and `chunk`.
#'   Each row is an exact detected token or multiword construction, with
#'   `count = 1`. Metrics cannot be audited as occurrences. Compatibility
#'   features that are always zero return no rows.
#' @examples
#' audit_features(udpipe_samples, category = "A")
#' @seealso [biber()]
#' @export
audit_features <- function(tokens, feature = NULL, category = NULL,
                           sample_n = NULL, seed = NULL, max_per_doc = NULL,
                           window = 5) {
  UseMethod("audit_features")
}

#' @rdname audit_features
#' @export
audit_features.udpipe_connlu <- function(tokens, feature = NULL, category = NULL,
                                         sample_n = NULL, seed = NULL,
                                         max_per_doc = NULL, window = 5) {
  audit_spanish_features(tokens, feature, category, sample_n, seed, max_per_doc,
                         window, "udpipe")
}

#' @rdname audit_features
#' @export
audit_features.spacyr_parsed <- function(tokens, feature = NULL, category = NULL,
                                         sample_n = NULL, seed = NULL,
                                         max_per_doc = NULL, window = 5) {
  audit_spanish_features(tokens, feature, category, sample_n, seed, max_per_doc,
                         window, "spacy")
}

#' @rdname audit_features
#' @export
audit_features.data.frame <- function(tokens, feature = NULL, category = NULL,
                                      sample_n = NULL, seed = NULL,
                                      max_per_doc = NULL, window = 5) {
  engine <- if ("upos" %in% names(tokens)) "udpipe" else "spacy"
  audit_spanish_features(tokens, feature, category, sample_n, seed, max_per_doc,
                         window, engine)
}

#' @export
audit_features.default <- function(tokens, feature = NULL, category = NULL,
                                   sample_n = NULL, seed = NULL,
                                   max_per_doc = NULL, window = 5) {
  stop("'tokens' must be UDPipe annotations, spacyr annotations, or a data frame.",
       call. = FALSE)
}

validate_audit_integer <- function(value, name, minimum = 0, optional = FALSE) {
  if (optional && is.null(value)) return(invisible(NULL))
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
      !is.finite(value) || value < minimum || value > .Machine$integer.max ||
      value != floor(value)) {
    stop("'", name, "' must be a single integer >= ", minimum, ".", call. = FALSE)
  }
}

audit_spanish_features <- function(tokens, feature, category, sample_n, seed,
                                   max_per_doc, window, engine) {
  if (is.null(feature) == is.null(category)) {
    stop("Supply exactly one of 'feature' or 'category'.", call. = FALSE)
  }
  if (!is.null(category)) {
    if (!is.character(category) || length(category) != 1L || is.na(category)) {
      stop("'category' must be a single category letter or label.", call. = FALSE)
    }
    labels <- unname(.feature_categories)
    selected <- toupper(category) == substr(labels, 1L, 1L) |
      tolower(category) == tolower(labels)
    feature <- names(.feature_categories)[selected]
    if (!length(feature)) stop("Unknown category: ", category, call. = FALSE)
  }
  if (!is.character(feature) || !length(feature) || anyNA(feature) ||
      any(!feature %in% names(.feature_categories))) {
    stop("'feature' must contain known canonical feature identifiers.", call. = FALSE)
  }
  if (any(feature %in% .metric_features)) {
    stop("Lexical summary metrics cannot be audited as occurrences.", call. = FALSE)
  }
  validate_audit_integer(sample_n, "sample_n", 1, TRUE)
  validate_audit_integer(max_per_doc, "max_per_doc", 1, TRUE)
  validate_audit_integer(seed, "seed", 0, TRUE)
  validate_audit_integer(window, "window")
  result <- extract_spanish_features(tokens, "none", FALSE, engine, traced = TRUE)
  prepared <- prepare_parsed_tokens(tokens)
  evidence <- result$evidence[result$evidence$feature %in% feature, ]
  rows <- lapply(seq_len(nrow(evidence)), function(i) {
    occurrence <- evidence[i, ]
    sentence <- prepared[prepared$doc_id == occurrence$doc_id &
                           prepared$sentence_id == occurrence$sentence_id, ]
    start <- match(occurrence$token_id, sentence$token_id_int)
    width <- length(strsplit(occurrence$token, " ", fixed = TRUE)[[1L]])
    end <- min(nrow(sentence), start + width - 1L)
    left <- if (start > 1L && window > 0) seq.int(max(1L, start - window), start - 1L) else integer()
    right <- if (end < nrow(sentence) && window > 0) seq.int(end + 1L, min(nrow(sentence), end + window)) else integer()
    tibble::tibble(
      feature = occurrence$feature,
      category = substr(.feature_categories[[occurrence$feature]], 1L, 1L),
      doc_id = occurrence$doc_id, sentence_id = occurrence$sentence_id,
      count = 1L, token_count = nrow(sentence), match_type = "exact",
      left = paste(sentence$surface[left], collapse = " "),
      keyword = paste(sentence$surface[seq.int(start, end)], collapse = " "),
      right = paste(sentence$surface[right], collapse = " "),
      chunk = paste(sentence$surface, collapse = " ")
    )
  })
  out <- if (length(rows)) dplyr::bind_rows(rows) else tibble::tibble(
    feature = character(), category = character(), doc_id = character(),
    sentence_id = integer(), count = integer(), token_count = integer(),
    match_type = character(), left = character(), keyword = character(),
    right = character(), chunk = character()
  )
  if (!is.null(sample_n) || !is.null(max_per_doc)) {
    if (!is.null(seed)) {
      had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
      on.exit({
        if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
        else rm(".Random.seed", envir = .GlobalEnv)
      }, add = TRUE)
      set.seed(seed)
    }
    if (!is.null(max_per_doc)) {
      out <- out %>%
        dplyr::group_by(.data$doc_id) %>%
        dplyr::group_modify(~ dplyr::slice_sample(.x, n = min(max_per_doc, nrow(.x)))) %>%
        dplyr::ungroup()
    }
    if (!is.null(sample_n) && nrow(out) > sample_n) {
      out <- dplyr::slice_sample(out, n = sample_n)
    }
  }
  structure(out, class = c("audit_features_result", class(out)))
}

#' Print an audit result
#' @param x An `audit_features_result`.
#' @param n Maximum number of rows to display.
#' @param ... Arguments passed to the tibble print method.
#' @export
print.audit_features_result <- function(x, n = 10, ...) {
  cat("audit_features_result:", nrow(x), "exact occurrences\n")
  print(tibble::as_tibble(x), n = n, ...)
  invisible(x)
}
