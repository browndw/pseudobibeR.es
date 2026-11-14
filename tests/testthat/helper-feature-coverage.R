# nolint start: line_length_linter, object_name_linter
# Helper for test-feature-coverage.R — audit Phase 2 (57 Spanish features).
# In the root tests/testthat/ so that testthat loads it automatically.

.fc_model_filename <- "spanish-gsd-ud-2.5-191206.udpipe"

.fc_model_path <- function() {
  td <- testthat::test_path()
  supplied <- Sys.getenv("PSEUDOBIBER_ES_UDPIPE_MODEL")
  if (nzchar(supplied)) {
    if (!file.exists(supplied)) stop("Configured UDPipe model does not exist: ", supplied)
    return(normalizePath(supplied, mustWork = TRUE))
  }
  cands <- unique(c(
    file.path(td, "..", "models", .fc_model_filename),
    file.path(td, "..", .fc_model_filename),
    file.path(td, "fixtures", .fc_model_filename),
    file.path(td, "..", "..", .fc_model_filename),
    file.path(td, "..", "..", "..", .fc_model_filename),
    file.path(".", .fc_model_filename)
  ))
  hit <- cands[file.exists(cands)]
  if (length(hit) == 0) return(NULL)
  normalizePath(hit[[1]], mustWork = TRUE)
}

# Single lazy load of the UDPipe model
.fc_ud <- local({
  m <- NULL
  function() {
    testthat::skip_if_not_installed("udpipe")
    if (is.null(m)) {
      p <- .fc_model_path()
      if (is.null(p)) testthat::skip("modelo UDPipe spanish-gsd no disponible")
      m <<- udpipe::udpipe_load_model(p)
    }
    m
  }
})

.fc_ud_or_null <- function() {
  if (!requireNamespace("udpipe", quietly = TRUE) || is.null(.fc_model_path())) return(NULL)
  .fc_ud()
}

# Parses a text and returns the feature row. By default normalize = FALSE
# (raw counts). Regression tests on derived metrics (f_43/f_44)
# can pass normalize = TRUE to check invariance.
run_biber <- function(text, normalize = FALSE) {
  skip_if_not_installed("udpipe")
  ud <- .fc_ud()
  parsed <- as.data.frame(udpipe::udpipe_annotate(ud, x = text))
  suppressMessages(biber_fixture(parsed, measure = "TTR", normalize = normalize))
}

# Batched UDPipe sentences. Each pipeline call has a large fixed cost, so a
# file declares its sentences once; they are parsed as separate documents and
# traced in one call on first use (inside a test, so missing models skip).
# Counts are per document, so batching gives the same values as one call per
# sentence with measure = "TTR" and normalize = FALSE.
#
#   b <- ud_batch(c("Llueve.", "Hay un problema."))
#   b$count("Llueve.", "f_09_pronoun_it")
#   b$row("Llueve.")             # one-row counts data frame
#   b$evidence("Llueve.", "f_09_pronoun_it")
ud_batch <- function(texts) {
  texts <- unique(texts)
  ids <- sprintf("s%04d", seq_along(texts))
  result <- NULL
  get <- function() {
    if (is.null(result)) {
      parsed <- udpipe::udpipe_annotate(.fc_ud(), x = texts, doc_id = ids)
      result <<- suppressMessages(
        pseudobibeR.es:::biber_es_traced(parsed, measure = "TTR", normalize = FALSE)
      )
    }
    result
  }
  id_of <- function(text) {
    id <- ids[match(text, texts)]
    if (is.na(id)) stop("Sentence not declared in ud_batch(): ", text, call. = FALSE)
    id
  }
  list(
    texts = texts,
    result = get,
    row = function(text) {
      counts <- get()$counts
      counts[counts$doc_id == id_of(text), , drop = FALSE]
    },
    count = function(text, feature) {
      counts <- get()$counts
      as.numeric(counts[[feature]][counts$doc_id == id_of(text)])
    },
    evidence = function(text, feature) {
      evidence <- get()$evidence
      evidence[evidence$doc_id == id_of(text) & evidence$feature == feature, ]
    }
  )
}

# nolint end
