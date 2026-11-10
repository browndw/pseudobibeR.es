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
