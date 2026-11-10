#' Add expanded variants of contracted articles to phrase patterns
#'
#' UDPipe splits `al` into `a el` and `del` into `de el`; spaCy keeps them as
#' one word. Each phrase is kept and, when it contains a contraction, also
#' compiled in its expanded form (`al lado de` and `a el lado de`).
#'
#' @param phrases Space-separated phrases.
#' @return Phrases with expanded variants added.
#' @keywords internal
#' @noRd
expand_contracted_articles <- function(phrases) {
  expanded <- gsub("(^| )al( |$)", "\\1a el\\2", phrases)
  expanded <- gsub("(^| )del( |$)", "\\1de el\\2", expanded)
  unique(c(phrases, expanded))
}

surface_matches <- function(tokens, patterns) {
  empty <- data.frame(start_row = integer(), end_row = integer())
  if (!length(patterns) || !nrow(tokens)) return(empty)
  groups <- dplyr::group_indices(dplyr::group_by(tokens, .data$doc_id, .data$sentence_id))
  rows <- split(seq_len(nrow(tokens)), groups)
  words <- lapply(rows, function(i) tokens$token[i])
  matches <- quanteda::kwic(
    quanteda::as.tokens(words),
    pattern = quanteda::phrase(expand_contracted_articles(gsub("_", " ", patterns, fixed = TRUE))),
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
  dict <- pseudobibeR.es::dict
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

#' Keep clause-initial discourse particles (f_50)
#'
#' Biber counts discourse particles clause-initially (sentence-initial or after
#' punctuation: *well, now, anyway*), as the English package does (D-56).
#' A dictionary match for f_50 is kept only when its first word starts the
#' sentence or follows punctuation, so adverbial or verbal homographs
#' elsewhere in the clause are not counted.
#'
#' @param evidence Dictionary evidence.
#' @param tokens Prepared tokens.
#' @return `evidence` without non-initial f_50 rows.
#' @keywords internal
clause_initial_particles <- function(evidence, tokens) {
  f50 <- which(evidence$feature == "f_50_discourse_particles")
  if (!length(f50)) return(evidence)
  previous <- match(
    paste(evidence$doc_id[f50], evidence$sentence_id[f50], evidence$token_id[f50] - 1L),
    paste(tokens$doc_id, tokens$sentence_id, tokens$token_id_int)
  )
  initial <- is.na(previous) | tokens$pos[previous] %in% c("PUNCT", "SYM")
  drop <- f50[!initial]
  if (length(drop)) evidence[-drop, ] else evidence
}
