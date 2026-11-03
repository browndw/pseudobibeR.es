prepare_parsed_tokens <- function(tokens) {
  if (inherits(tokens, "udpipe_connlu")) {
    if (!requireNamespace("udpipe", quietly = TRUE)) {
      stop("Install 'udpipe' to convert UDPipe annotations.", call. = FALSE)
    }
    tokens <- as.data.frame(tokens, stringsAsFactors = FALSE)
  }
  if (!is.data.frame(tokens) || nrow(tokens) == 0L) {
    stop("'tokens' must be a non-empty data frame.", call. = FALSE)
  }
  tokens <- tibble::as_tibble(tokens)
  if ("upos" %in% names(tokens)) {
    tokens$pos <- tokens$upos
    tokens$tag <- tokens$xpos
  }
  required <- c("doc_id", "sentence_id", "token_id", "token", "lemma",
                "pos", "tag", "head_token_id", "dep_rel")
  missing <- setdiff(required, names(tokens))
  if (length(missing)) {
    stop("Missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  for (column in c("doc_id", "token_id", "token", "lemma", "pos", "tag",
                   "head_token_id", "dep_rel")) {
    tokens[[column]] <- as.character(tokens[[column]])
  }
  if (anyNA(tokens$doc_id) || any(!nzchar(trimws(tokens$doc_id)))) {
    stop("'doc_id' must contain non-missing, non-empty identifiers.", call. = FALSE)
  }
  doc_order <- unique(tokens$doc_id)
  sentence <- suppressWarnings(as.integer(as.character(tokens$sentence_id)))
  if (anyNA(sentence) || any(as.character(sentence) != as.character(tokens$sentence_id))) {
    stop("'sentence_id' must contain integer identifiers.", call. = FALSE)
  }
  tokens$sentence_id <- sentence
  container <- grepl("^[0-9]+[-.][0-9]+$", tokens$token_id)
  tokens <- tokens[!container, ]
  tokens$token_id_int <- suppressWarnings(as.integer(tokens$token_id))
  tokens$head_token_id_int <- suppressWarnings(as.integer(tokens$head_token_id))
  if (anyNA(tokens$token_id_int) ||
      any(as.character(tokens$token_id_int) != tokens$token_id) ||
      any(tokens$token_id_int < 1L)) {
    stop("'token_id' must contain positive integer word IDs.", call. = FALSE)
  }
  if (anyDuplicated(tokens[c("doc_id", "sentence_id", "token_id_int")])) {
    stop("Duplicate token keys (doc_id, sentence_id, token_id).", call. = FALSE)
  }
  if (anyNA(tokens$token) || any(!nzchar(tokens$token))) {
    stop("'token' must contain non-missing, non-empty strings.", call. = FALSE)
  }
  if (anyNA(tokens$pos)) {
    stop("'pos'/'upos' must be present for every word token.", call. = FALSE)
  }
  if ("morph" %in% names(tokens)) {
    morph <- vapply(tokens$morph, function(x) {
      if (inherits(x, "python.builtin.object")) {
        if (!requireNamespace("reticulate", quietly = TRUE)) {
          stop("Install 'reticulate' to convert spaCy morphology.", call. = FALSE)
        }
        x <- reticulate::py_to_r(x)
      }
      if (is.null(x) || !length(x)) NA_character_ else paste(as.character(x), collapse = "|")
    }, character(1))
    if ("feats" %in% names(tokens)) {
      tokens$feats <- dplyr::coalesce(dplyr::na_if(as.character(tokens$feats), ""), morph)
    } else {
      tokens$feats <- morph
    }
  }
  if (!"feats" %in% names(tokens)) {
    stop("Missing morphology: supply 'feats' or 'morph'.", call. = FALSE)
  }
  tokens$feats <- as.character(tokens$feats)
  tokens$surface <- tokens$token
  tokens$token <- stringi::stri_trans_tolower(tokens$token)
  tokens$lemma <- stringi::stri_trans_tolower(tokens$lemma)
  tokens$pos[tokens$token == "\n"] <- "PUNCT"
  tokens <- tokens[tokens$pos != "SPACE", ]
  if (!all(doc_order %in% tokens$doc_id)) {
    stop("Every document must contain at least one non-space word token.", call. = FALSE)
  }
  tokens$tag <- dplyr::coalesce(dplyr::na_if(tokens$tag, ""), tokens$pos)
  for (key in c("Tense", "VerbForm", "Mood", "PronType", "Voice", "Number", "Person")) {
    tokens[[paste0("morph_", tolower(key))]] <- extract_morph_value(tokens$feats, key)
  }
  tokens[order(match(tokens$doc_id, doc_order), tokens$sentence_id, tokens$token_id_int), ]
}
