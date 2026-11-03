# Run from the package root. The model is local and is never downloaded.
if (!requireNamespace("yaml", quietly = TRUE) ||
    !requireNamespace("udpipe", quietly = TRUE)) {
  stop("Install 'yaml' and 'udpipe' before rebuilding package data.")
}
normalize_values <- function(values, phrases = FALSE) {
  values <- stringi::stri_trans_tolower(trimws(as.character(values)))
  values <- values[!is.na(values) & nzchar(values)]
  if (phrases) values <- gsub("\\s+", "_", values)
  sort(unique(values))
}
spec <- yaml::read_yaml("data-raw/dict.yaml")
dict <- quanteda::dictionary(lapply(spec, normalize_values, phrases = TRUE))
word_lists <- lapply(yaml::read_yaml("data-raw/word_lists.yaml"), normalize_values)
phrases <- unlist(as.list(dict), use.names = FALSE)
phrases <- phrases[grepl("_", phrases, fixed = TRUE)]
word_lists$multiword_patterns <- sort(unique(gsub(
  "_", " ", c(word_lists$multiword_patterns, phrases), fixed = TRUE
)))
examples <- yaml::read_yaml("data-raw/spanish_examples.yaml")
spanish_examples <- tibble::tibble(
  feature = vapply(examples, `[[`, character(1), "feature"),
  example = vapply(examples, `[[`, character(1), "example"),
  count = vapply(examples, function(x) as.numeric(x$count), numeric(1))
)
model_path <- Sys.getenv(
  "PSEUDOBIBER_ES_UDPIPE_MODEL",
  "tests/models/spanish-gsd-ud-2.5-191206.udpipe"
)
if (!file.exists(model_path)) stop("Local Spanish-GSD model not found: ", model_path)
model <- udpipe::udpipe_load_model(model_path)
texts <- c(
  "El informe fue redactado por el equipo de investigación.",
  "Yo creo que nosotros podemos mejorar el método.",
  "María llegó ayer y explicó los resultados."
)
udpipe_samples <- udpipe::udpipe_annotate(
  model, x = texts, doc_id = c("informe", "opinion", "relato"),
  tagger = "default", parser = "default"
)
for (dataset in c("dict", "word_lists", "spanish_examples", "udpipe_samples")) {
  save(list = dataset, file = file.path("data", paste0(dataset, ".rda")), compress = "xz")
}
regression_samples <- udpipe::udpipe_annotate(
  model,
  x = c(
    "El informe fue cuidadosamente revisado ayer por el equipo.",
    "El informe fue revisado. Por otra parte, el equipo descansó.",
    "A través del país, viajamos.",
    "Sin embargo, nosotros pensamos."
  ),
  doc_id = c("distant_agent", "next_sentence", "preposition", "conjunct")
)
dir.create("tests/testthat/fixtures", showWarnings = FALSE)
saveRDS(
  as.data.frame(regression_samples),
  "tests/testthat/fixtures/spanish-gsd-regressions.rds",
  compress = "xz"
)
