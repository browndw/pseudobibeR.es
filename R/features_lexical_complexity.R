PUNCT_UPOS <- c("PUNCT", "SYM", "SPACE")

#' Normalization denominator per document
#'
#' UD word rows excluding punctuation, symbols, spaces and digit-final numeric
#' tokens (`42`, `3,5`), the same numeric rule as the English package and the
#' lexical-diversity stream. Spelled-out numerals and function words count.
#'
#' @param tokens Prepared tokens.
#' @param doc_ids Document IDs in output order.
#' @return Integer vector aligned with `doc_ids`.
#' @keywords internal
#' @noRd
document_word_counts <- function(tokens, doc_ids) {
  words <- tokens[!tokens$pos %in% PUNCT_UPOS & !stringi::stri_detect_regex(tokens$token, "\\d$"), ]
  tabulate(match(words$doc_id, doc_ids$doc_id), nbins = nrow(doc_ids))
}

lexical_diversity_tokens <- function(tokens, doc_ids) {
  words <- tokens[!tokens$pos %in% PUNCT_UPOS, ]
  tags <- dplyr::coalesce(dplyr::na_if(words$tag, "_"), words$pos)
  tagged <- paste(words$token, stringi::stri_trans_tolower(tags), sep = "_")
  documents <- factor(
    match(words$doc_id, doc_ids$doc_id), levels = seq_len(nrow(doc_ids))
  )
  quanteda::as.tokens(split(tagged, documents)) %>%
    quanteda::tokens_remove("\\d_", valuetype = "regex")
}

#' Moving-average type-token ratio, equivalent to quanteda.textstats
#'
#' Reproduces `quanteda.textstats::textstat_lexdiv(x, "MATTR")` (0.97) without
#' materializing every window as a string. quanteda joins each window with
#' spaces and re-tokenizes it, so some tagged tokens (`etc._x`, `-según_adp`)
#' split into several pieces, then lower-cases and drops punctuation, symbol,
#' number and URL pieces before computing TTR. That per-type expansion is
#' computed once with quanteda itself; windows then slide in linear time.
#'
#' @param x A quanteda `tokens` object.
#' @param window Window length in tokens.
#' @return Numeric vector of MATTR values, one per document.
#' @keywords internal
#' @noRd
mattr_lexdiv <- function(x, window = 100L) {
  x <- quanteda::tokens(x, split_hyphens = FALSE, remove_numbers = TRUE,
                        remove_symbols = TRUE, remove_punct = TRUE, remove_url = TRUE)
  lengths <- quanteda::ntoken(x)
  if (any(lengths < window)) {
    window <- min(lengths)
    warning("MATTR_window exceeds some documents' token lengths, resetting to ",
            window, call. = FALSE)
  }
  vocab <- quanteda::types(x)
  pieces <- quanteda::tokens_tolower(quanteda::tokens(vocab))
  pieces <- as.list(quanteda::tokens(pieces, remove_punct = TRUE, remove_symbols = TRUE,
                                     remove_numbers = TRUE, remove_url = TRUE))
  piece_vocab <- unique(unlist(pieces, use.names = FALSE))
  piece_ids <- lapply(pieces, match, table = piece_vocab)
  n_pieces <- lengths(piece_ids)
  flat_ids <- unlist(piece_ids, use.names = FALSE)
  type_offset <- c(0L, cumsum(n_pieces))
  counts <- integer(length(piece_vocab))
  vapply(unclass(x), function(doc) {
    doc_pieces <- n_pieces[doc]
    offset <- c(0L, cumsum(doc_pieces))
    seq_ids <- flat_ids[sequence(doc_pieces, type_offset[doc] + 1L)]
    n_windows <- length(doc) - window + 1L
    ratios <- numeric(n_windows)
    counts[] <- 0L
    distinct <- 0L
    for (k in seq_len(offset[window + 1L])) {
      id <- seq_ids[k]
      if (counts[id] == 0L) distinct <- distinct + 1L
      counts[id] <- counts[id] + 1L
    }
    ratios[1L] <- distinct / offset[window + 1L]
    for (w in seq_len(n_windows - 1L)) {
      for (k in seq.int(offset[w] + 1L, length.out = doc_pieces[w])) {
        id <- seq_ids[k]
        counts[id] <- counts[id] - 1L
        if (counts[id] == 0L) distinct <- distinct - 1L
      }
      last <- w + window
      for (k in seq.int(offset[last] + 1L, length.out = doc_pieces[last])) {
        id <- seq_ids[k]
        if (counts[id] == 0L) distinct <- distinct + 1L
        counts[id] <- counts[id] + 1L
      }
      ratios[w + 1L] <- distinct / (offset[last + 1L] - offset[w + 1L])
    }
    mean(ratios)
  }, numeric(1), USE.NAMES = FALSE)
}

# f_44 (D-51): mean length in letters of purely alphabetic word tokens (any
# Unicode letter), unrounded; numerals, hyphenated and apostrophe forms are
# excluded, as in English and French. Documents without such words get NA.
block_lexical_complexity_es <- function(tokens, doc_ids) {
  words <- tokens[!tokens$pos %in% PUNCT_UPOS &
                    stringi::stri_detect_regex(tokens$token, "^\\p{L}+$"), ]
  metrics <- words %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::summarise(
      f_44_mean_word_length = mean(nchar(.data$token, type = "chars")),
      .groups = "drop"
    )
  doc_ids %>%
    dplyr::left_join(metrics, by = "doc_id") %>%
    dplyr::mutate(n_tokens = document_word_counts(tokens, doc_ids))
}
