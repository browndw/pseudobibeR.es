LEXICAL_UPOS <- c("NOUN", "VERB", "ADJ", "ADV", "PROPN")
PUNCT_UPOS <- c("PUNCT", "SYM", "SPACE")

block_lexical_complexity_es <- function(tokens, doc_ids) {
  words <- tokens[!tokens$pos %in% PUNCT_UPOS, ]
  metrics <- words %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::summarise(
      n_tokens = dplyr::n(),
      n_lex_tokens = sum(.data$pos %in% LEXICAL_UPOS),
      f_44_mean_word_length = round(mean(nchar(.data$token, type = "chars")), 3),
      .groups = "drop"
    )
  doc_ids %>%
    dplyr::left_join(metrics, by = "doc_id") %>%
    dplyr::mutate(
      n_tokens = dplyr::coalesce(.data$n_tokens, 0L),
      n_lex_tokens = dplyr::coalesce(.data$n_lex_tokens, 0L)
    )
}
