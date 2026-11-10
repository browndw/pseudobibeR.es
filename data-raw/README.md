# Rebuilding Spanish package data

The source resources are `dict.yaml`, `word_lists.yaml`, and
`spanish_examples.yaml`. To regenerate the four documented datasets, run
`source("data-raw/build_spanish_data.R")` from the package root.

The script requires `yaml`, `udpipe`, and the package's imported dependencies.
Supply the reference model at
`tests/models/spanish-gsd-ud-2.5-191206.udpipe`, or set
`PSEUDOBIBER_ES_UDPIPE_MODEL` to its absolute path. No downloads or dependency
installations are performed.

The sample sentences are original examples included in the script. UDPipe
samples preserve the native annotation class. Do not label converted UDPipe
annotations as spaCy output.

`feature_names.yaml` holds the Spanish display names of output columns
(shared identifier: display name); each display name must keep its `f_NN_`
number. Rebuild it with `source("data-raw/build_feature_names.R")`.

Rebuild after changing lexical resources, then run `devtools::document()`,
`testthat::test_local()`, and `devtools::check()`. Changes to expected
linguistic counts must be reviewed independently of the extractor's output.

## Frozen spaCy regression annotations

`build_spacy_fixture.R` creates
`tests/testthat/fixtures/spanish-spacy-regressions.rds` from original sentences
parsed by an actual Spanish spaCy model. It preserves native tokenization,
POS, lemmas, dependency labels, and head IDs; only Python morphology objects
are serialized to strings for offline use. The fixture records model, spaCy,
and spacyr versions and its source texts.

Set `RETICULATE_PYTHON` to the Python executable in an existing `r-spacyr`
environment containing spaCy and `es_core_news_sm`, then run
`source("data-raw/build_spacy_fixture.R")`. The reference fixture uses
spaCy 3.8.7 and `es_core_news_sm` 3.8.0. No models or dependencies are installed
by the builder. Review model changes and gold expectations before replacing
this fixture; do not generate expected counts from the extractor.
