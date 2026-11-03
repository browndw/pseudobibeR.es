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
