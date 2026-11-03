biber_es_traced <- function(tokens,
                            measure = c("MATTR", "TTR", "CTTR", "MSTTR", "none"),
                            normalize = TRUE) {
  engine <- if (inherits(tokens, "udpipe_connlu") || "upos" %in% names(tokens)) {
    "udpipe"
  } else {
    "spacy"
  }
  extract_spanish_features(tokens, match.arg(measure), normalize, engine, traced = TRUE)
}
