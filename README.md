[![R-CMD-check](https://github.com/browndw/pseudobibeR.es/workflows/R-CMD-check/badge.svg)](https://github.com/browndw/pseudobibeR.es/actions)
[![Tests](https://github.com/browndw/pseudobibeR.es/workflows/Tests/badge.svg)](https://github.com/browndw/pseudobibeR.es/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/pseudobibeR.es)](https://CRAN.R-project.org/package=pseudobibeR.es)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/pseudobibeR.es)](https://cran.r-project.org/package=pseudobibeR.es)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

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
library(pseudobibeR.es)

# Download and load French model (only needed once)
model <- udpipe_download_model(language = "spanish-gsd")
ud_model <- udpipe_load_model(model$file_model)

# Prepare your text data
text_data <- data.frame(
  doc_id = c("doc_1", "doc_2"),
  text = c(
    "El informe fue redactado por el equipo.",
    "Yo creo que podemos mejorar."
  )
)

# Parse the text
parsed_data <- udpipe_annotate(
  ud_model,
  x = text_data$text,
  doc_id = text_data$doc_id,
  tagger = "default",
  parser = "default"
)

# Extract features
features <- biber(parsed_data, measure = "none", normalize = FALSE)
print(features)
```

For spaCy, use an existing Python environment containing spaCy and an installed
Spanish model. This example uses the `r-spacyr` conda environment; select it
before initializing Python in the R session:

```r
reticulate::use_condaenv("r-spacyr", required = TRUE)
spacyr::spacy_initialize(model = "es_core_news_sm")
parsed <- spacyr::spacy_parse(
  c(doc_1 = "El informe fue redactado por el equipo.",
    doc_2 = "Yo he terminado el informe."),
  dependency = TRUE, tag = TRUE, pos = TRUE, entity = FALSE,
  additional_attributes = "morph"
)
features <- pseudobibeR.es::biber(parsed, measure = "MATTR", normalize = TRUE)
pseudobibeR.es::audit_features(parsed, feature = "f_18_by_passives")
spacyr::spacy_finalize()
```

**Morphology is required:** `spacy_parse()` does not return it by default.
Without `additional_attributes = "morph"`, Spanish extraction reports an error.
Some spacyr versions issue a blanket warning about non-English lemmatization;
this is upstream behavior, not a feature-extraction failure.

Integration tests cover Spanish-GSD and spaCy `es_core_news_sm` (3.8.0 with
spaCy 3.8.7). Both adapters share Spanish detectors, with narrow support for
native root/passive conventions. This does not imply identical accuracy:
model mistakes in POS, lemmas, morphology, and dependencies remain visible.

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
  Expanded contractions and clitics count as their supplied component words.
  spaCy may retain forms such as `al`, `del`, and `verlo` as single tokens;
  the extractor does not invent a UD expansion, so denominators can differ
  between parsers even for identical text.
  `n_tokens` reports the denominator.
- `n_lex_tokens` counts nouns, proper nouns, verbs, adjectives, and adverbs;
  it is not the normalization denominator.
- Lexical diversity uses lower-case **surface-plus-tag types**, not lemmas.
  Identical surface words with different parser tags are different types.
  Fine-grained tags are retained; missing or `_` tags use UPOS. Punctuation,
  symbols, and spaces are excluded by UD POS; the English numeric pattern
  `\d_` removes numeric matches from the tagged stream, including digit-final
  surface forms. Spelled-out numerals remain words. Dictionary phrases are
  not compounded for this metric.
- MATTR is the default; if any document has fewer than 200 retained diversity
  tokens, the entire supplied corpus uses TTR, with one message.
  This filtered population differs from `n_tokens`, which includes number words.
  Measures are never mixed within a corpus. Explicitly selected TTR,
  CTTR, and MSTTR are retained for every document. MSTTR may trigger a
  `quanteda.textstats` warning when its common segment length is shortened.
  For multivariate comparisons, supply the whole corpus together and use the
  same measure across any separately processed corpora.
- Documents with no retained diversity tokens receive `NA` for
  `f_43_type_token`, with an identifying warning.
- Mean word length and lexical diversity are never normalized.
- All non-metric counts are derived from the same occurrences used by auditing.
  Dictionary phrases and their component detections are reconciled before
  counting, within sentence boundaries.
- Total adverbs (`f_42_adverbs`) excludes ADV occurrences counted as place/time
  adverbials, downtoners, hedges, or amplifiers (f_04, f_05, f_46, f_47, f_48).
  Exclusions cover ADV members of counted phrases, not just their first tokens.
  Conjuncts, emphatics, discourse particles, and negation are not additional
  exclusions. Specialized counts and the word-token denominator are unchanged.
- Analytic negation (`f_67_neg_analytic`) counts the scoped `no` word, not its
  full phrase. ADV/PART `no` with `advmod` or `neg` can negate nominal,
  pronominal, numeral, and other word constituents as well as predicates.
  Nouns remain available to their own features. Guarded exclusions cover
  `no obstante`, identifiable replies, nominalized `no`, and quoted standalone
  `no`; a quoted negative clause still counts. These guards use local syntax
  and context, not general semantic disambiguation. Expletive `no` and
  non-negative `no más` readings remain unresolved. The f_66 inventory,
  negative-concord counts, and f_42 ADV/PART mapping are unchanged.
- Extraction and auditing use this package's Spanish dictionaries and word lists,
  even when companion packages are attached. Calls through `pseudobibeR.es::`
  do not require `library(pseudobibeR.es)`.

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

Tests use small synthetic annotations and frozen native UDPipe/spaCy annotations
without a parser installation. Optional live integration tests use locally
installed models; test code never installs dependencies or downloads models.
Models and validation output are excluded from package builds. The temporary
French reference checkout is no longer kept inside the Spanish project.
Set `PSEUDOBIBER_ES_UDPIPE_MODEL` to an absolute local model path, or put the
reference model in `tests/models/`. Frozen Spanish annotations also exercise
correctness regressions offline. Dataset rebuild instructions are in
[data-raw](data-raw/README.md).
To enable live spaCy tests, set `RETICULATE_PYTHON` to the Python executable in
`r-spacyr` and `PSEUDOBIBER_ES_SPACY_MODEL=es_core_news_sm`, then run
`testthat::test_local(filter = "spacy-pipeline")`. Without the model opt-in,
only the live test skips; frozen spaCy regressions still run. The Tests GitHub
Action provisions both reference models and enables live spaCy testing in an
isolated `r-spacyr` environment.
The development validation script uses the same local model setting and never
installs packages or downloads models.

Shiny applications are archived in the sibling `pseudobibeR.es-tools` directory.
The package no longer exports an Excel writer or language-specific extraction
entry points. See [NEWS](NEWS.md) for migration and result-changing corrections.
