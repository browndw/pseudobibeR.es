LEXICAL_UPOS <- c("NOUN", "VERB", "ADJ", "ADV", "PROPN")
PUNCT_UPOS <- c("PUNCT", "SYM", "SPACE")

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
