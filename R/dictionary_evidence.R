surface_matches <- function(tokens, patterns) {
  empty <- data.frame(start_row = integer(), end_row = integer())
  if (!length(patterns) || !nrow(tokens)) return(empty)
  groups <- dplyr::group_indices(dplyr::group_by(tokens, .data$doc_id, .data$sentence_id))
  rows <- split(seq_len(nrow(tokens)), groups)
  words <- lapply(rows, function(i) tokens$token[i])
  matches <- quanteda::kwic(
    quanteda::as.tokens(words),
    pattern = quanteda::phrase(gsub("_", " ", patterns, fixed = TRUE)),
    valuetype = "glob", case_insensitive = TRUE, window = 0
  )
  if (!nrow(matches)) return(empty)
  found <- unique(data.frame(
    start_row = vapply(seq_len(nrow(matches)), function(i) {
      rows[[as.character(matches$docname[i])]][matches$from[i]]
    }, integer(1)),
    end_row = vapply(seq_len(nrow(matches)), function(i) {
      rows[[as.character(matches$docname[i])]][matches$to[i]]
    }, integer(1))
  ))
  keep <- vapply(seq_len(nrow(found)), function(i) {
    !any(found$start_row <= found$start_row[i] & found$end_row >= found$end_row[i] &
           (found$start_row < found$start_row[i] | found$end_row > found$end_row[i]))
  }, logical(1))
  found[keep, , drop = FALSE]
}

dictionary_evidence <- function(tokens, features) {
  evidence <- list()
  spans <- list()
  for (feature in features) {
    hits <- surface_matches(tokens, dict[[feature]])
    if (!nrow(hits)) next
    rows <- tokens[hits$start_row, ]
    rows$token <- vapply(seq_len(nrow(hits)), function(i) {
      paste(tokens$token[seq.int(hits$start_row[i], hits$end_row[i])], collapse = " ")
    }, character(1))
    evidence[[feature]] <- evidence_from_rows(rows, feature)
    spans[[feature]] <- data.frame(
      feature = feature, doc_id = rows$doc_id, sentence_id = rows$sentence_id,
      start = tokens$token_id_int[hits$start_row], end = tokens$token_id_int[hits$end_row]
    )
  }
  list(
    evidence = do.call(bind_evidence, evidence),
    spans = if (length(spans)) dplyr::bind_rows(spans) else
      data.frame(feature = character(), doc_id = character(), sentence_id = integer(),
                 start = integer(), end = integer())
  )
}
