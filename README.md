# pseudobibeR.es

Spanish adaptations of Biber's lexicogrammatical features, with the same basic
interface as the English `pseudobibeR` and French `pseudobibeR.fr` packages.
This development package is not yet a CRAN release.

## Pipeline

1. Annotate Spanish text with UDPipe or spaCy.
2. Call `biber()` to obtain one row per document.
3. Use `audit_features()` to inspect the occurrences behind a feature count.
4. Perform statistical analysis and file export in your own workflow.

```r
library(udpipe)

# Download once, outside the package. No models are downloaded by biber().
model_file <- udpipe_download_model("spanish-gsd")$file_model
model <- udpipe_load_model(model_file)
parsed <- udpipe_annotate(
  model,
  x = c("El informe fue redactado por el equipo.",
        "Yo creo que podemos mejorar."),
  doc_id = c("doc_1", "doc_2"),
  tagger = "default", parser = "default"
)

features <- pseudobibeR.es::biber(parsed, measure = "MATTR", normalize = TRUE)
pseudobibeR.es::audit_features(parsed, feature = "f_18_by_passives")
```

The equivalent spaCy workflow uses `spacyr::spacy_parse()` with `dependency`,
`tag`, and `pos` enabled, followed by the same `biber()` call. Spanish-GSD is
the reference integration model; other Spanish parsers require independent
validation. Both adapters share UD-based detectors rather than assuming
identical accuracy across parsers.

For several documents, pass the complete annotated corpus to `biber()`.
Use an ordinary data frame for results and your preferred export tools, such
as `utils::write.csv(features, "features.csv", row.names = FALSE)`.

## Output and compatibility

- The usual output contains the same 67 feature identifiers as the companion
  packages, followed by `n_tokens` and `n_lex_tokens`.
- `measure = "none"` omits `f_43_type_token`, as in the companion interfaces.
- Ten unavailable or merged Spanish features are constant-zero compatibility
  columns: f_09, f_12, f_15, f_28, f_31, f_32, f_59, f_60, f_61, and f_62.
- Normalized counts use **1,000 UD word tokens**: punctuation, symbols, spaces,
  and multiword-token container rows are excluded; function words are included.
  Expanded contractions and clitics count as their UD component words.
  `n_tokens` reports the denominator.
- `n_lex_tokens` counts nouns, proper nouns, verbs, adjectives, and adverbs;
  it is not the normalization denominator.
- Lexical diversity uses lower-case surface tokens, including punctuation,
  before phrase compounding. MATTR and MSTTR fall back to TTR separately for
  documents shorter than 200 tokens, with a message. CTTR is not replaced.
- Mean word length and lexical diversity are never normalized.
- All non-metric counts are derived from the same occurrences used by auditing.
  Dictionary phrases and their component detections are reconciled before
  counting, within sentence boundaries.

Shared interfaces and identifiers do **not** establish cross-language
measurement equivalence. See the [Spanish feature table](TABLA_RASGOS_ES.md)
for linguistic interpretations and parser limitations. English factor norms
must not be treated as validated Spanish norms.

## Development

Requires R >= 4.1. Install development tools separately, then run:

```r
pkgload::load_all()
testthat::test_local()
devtools::check()
```

Tests use small synthetic annotations without a parser installation. Optional
integration tests use a locally supplied Spanish-GSD model; they skip if it is
unavailable and never download it. Models, validation output, and the temporary
French reference are excluded from package builds.
Set `PSEUDOBIBER_ES_UDPIPE_MODEL` to an absolute local model path, or put the
reference model in `tests/models/`. Frozen Spanish annotations also exercise
correctness regressions offline. Dataset rebuild instructions are in
[data-raw](data-raw/README.md).
The development validation script uses the same local model setting and never
installs packages or downloads models.

Shiny applications are archived in the sibling `pseudobibeR.es-tools` directory.
The package no longer exports an Excel writer or language-specific extraction
entry points. See [NEWS](NEWS.md) for migration and result-changing corrections.
