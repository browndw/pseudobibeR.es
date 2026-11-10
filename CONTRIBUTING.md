# Contributing to pseudobibeR.es

Thank you for helping improve the package. Please report problems and
suggestions on [GitHub](https://github.com/browndw/pseudobibeR.es/issues).

## Reporting issues

Please include:

- a short text that shows the problem and the counts you expected;
- the parser and model used (for example udpipe `spanish-gsd-ud-2.5-191206`
  or spaCy `es_core_news_sm`), with their versions;
- your R and package versions.

`audit_features()` output for the feature in question is very helpful: it
shows exactly which words were counted.

Proposals to change what a feature counts are welcome. Please explain the
linguistic reasoning; accepted changes are recorded in the family decision
log (`CROSS_LANGUAGE_ALIGNMENT.md`) so that the three packages stay parallel.

## Package layout

- `R/`: `biber()` and `audit_features()` (`biber.R`, `audit_features.R`), token
  preparation, and feature detectors grouped by category (`features_*.R`).
  Each detector returns the occurrences it found; counts are tabulated from
  those occurrences.
- `data-raw/`: editable sources and build scripts (see
  [`data-raw/README.md`](data-raw/README.md)):
  - `dict.yaml`, `word_lists.yaml`: word lists and phrases used by detectors;
  - `spanish_examples.yaml`: example sentences with expected counts;
  - `feature_names.yaml`: Spanish display names for output columns.
- `tests/testthat/`: unit tests on controlled annotations and live parser tests.
- `FEATURES_TECHNICAL.md`: the exact rule behind each feature. Update it when a
  rule changes.

## Rebuilding package data

After editing a YAML file, rebuild from the package root:

```r
source("data-raw/build_spanish_data.R")   # dict, word_lists, examples, samples
source("data-raw/build_feature_names.R")  # feature_names
```

`build_spanish_data.R` needs the udpipe model at
`tests/models/spanish-gsd-ud-2.5-191206.udpipe` (or the path in
`PSEUDOBIBER_ES_UDPIPE_MODEL`). Commit the YAML sources together with the
regenerated files in `data/`.

## Running tests and checks

```r
devtools::document()
devtools::test()
devtools::check()
```

Tests that parse text need the udpipe model above; they are skipped without
it. Live spaCy tests run only when `PSEUDOBIBER_ES_SPACY_MODEL` is set (for
example to `es_core_news_sm`) and `RETICULATE_PYTHON` points to a Python with
spaCy installed. Tests use the shared feature identifiers
(`options(pseudobibeR.feature_names = "shared")` is set in a test helper).

Expected counts in examples and tests should come from linguistic judgment,
not from the extractor's own output.
