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

Rebuild after changing lexical resources, then run `devtools::document()`,
`testthat::test_local()`, and `devtools::check()`. Changes to expected
linguistic counts must be reviewed independently of the extractor's output.
