# Tests address features by their shared identifiers; display names are
# covered in test-feature-names.R.
options(pseudobibeR.feature_names = "shared")

contract_tokens <- function(doc_id = "d1") {
  data.frame(
    doc_id = doc_id, sentence_id = 1L, token_id = as.character(1:4),
    token = c("Yo", "pienso", "hoy", "."),
    lemma = c("yo", "pensar", "hoy", "."), upos = c("PRON", "VERB", "ADV", "PUNCT"),
    xpos = c("PRON", "VERB", "ADV", "PUNCT"),
    feats = c("Person=1|PronType=Prs", "Tense=Pres|Mood=Ind|VerbForm=Fin", NA, NA),
    head_token_id = c("2", "0", "2", "2"),
    dep_rel = c("nsubj", "root", "advmod", "punct")
  )
}

contract_clause <- function(doc_id, token, lemma, upos, heads, relations,
                            feats = rep(NA_character_, length(token))) {
  data.frame(
    doc_id = doc_id, sentence_id = 1L, token_id = as.character(seq_along(token)),
    token = token, lemma = lemma, upos = upos, xpos = upos, feats = feats,
    head_token_id = as.character(heads), dep_rel = relations
  )
}

# Plain annotation tables are test fixtures, not a public input type: route
# them to the internal extractors used by the UDPipe and spaCy S3 methods.
biber_fixture <- function(tokens, measure = c("MATTR", "TTR", "CTTR", "MSTTR", "none"),
                          normalize = TRUE) {
  if (inherits(tokens, c("udpipe_connlu", "spacyr_parsed"))) {
    return(pseudobibeR.es::biber(tokens, measure = measure, normalize = normalize))
  }
  engine <- if ("upos" %in% names(tokens)) "udpipe" else "spacy"
  pseudobibeR.es:::extract_spanish_features(tokens, match.arg(measure), normalize, engine)
}

audit_fixture <- function(tokens, feature = NULL, category = NULL, sample_n = NULL,
                          seed = NULL, max_per_doc = NULL, window = 5) {
  if (inherits(tokens, c("udpipe_connlu", "spacyr_parsed"))) {
    return(pseudobibeR.es::audit_features(tokens, feature, category, sample_n, seed,
                                          max_per_doc, window))
  }
  engine <- if ("upos" %in% names(tokens)) "udpipe" else "spacy"
  pseudobibeR.es:::audit_spanish_features(tokens, feature, category, sample_n, seed,
                                          max_per_doc, window, engine)
}

# Per-document normalization denominator (word rows without punctuation,
# symbols, spaces or digit-final numerals).
word_tokens <- function(tokens) {
  prepared <- pseudobibeR.es:::prepare_parsed_tokens(tokens)
  doc_ids <- tibble::tibble(doc_id = unique(prepared$doc_id))
  pseudobibeR.es:::document_word_counts(prepared, doc_ids)
}

# Convert a UDPipe-style fixture table to a genuine udpipe_connlu annotation.
as_udpipe_annotation <- function(tokens) {
  rows <- data.frame(
    doc_id = tokens$doc_id, paragraph_id = 1L, sentence_id = tokens$sentence_id,
    sentence = NA_character_, token_id = tokens$token_id, token = tokens$token,
    lemma = tokens$lemma, upos = tokens$upos, xpos = tokens$xpos, feats = tokens$feats,
    head_token_id = tokens$head_token_id, dep_rel = tokens$dep_rel,
    deps = NA_character_, misc = NA_character_
  )
  structure(list(x = NA_character_, conllu = udpipe::as_conllu(rows), error = character()),
            class = "udpipe_connlu")
}
