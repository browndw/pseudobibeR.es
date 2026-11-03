# evidence_helpers.R
#
# Shared counts and evidence infrastructure for feature extraction and auditing.

# ----------------------------------------------------------------------
# Canonical evidence schema (long format E1)
# ----------------------------------------------------------------------

#' Columns of the E1 evidence schema
#'
#' One row per token that triggered a count detection. The order and
#' the types are fixed so that bind_rows() works without coercion.
#'
#' @keywords internal
#' @noRd
.EVIDENCE_COLS <- c(
  "doc_id", "feature", "sentence_id", "token_id",
  "token", "lemma", "upos", "feats", "head_token_id"
)

#' Empty tibble with the canonical evidence schema
#'
#' Used as the default value in \code{make_block_result()} when a
#' block is not migrated yet (its evidence is empty) and as the base
#' for \code{bind_evidence()} when there is nothing to stack.
#'
#' @return A 0-row tibble with the 9 columns of \code{.EVIDENCE_COLS}
#'   and the right types: \code{character} for doc_id/feature/token/lemma/upos/feats,
#'   \code{integer} for sentence_id/token_id/head_token_id.
#' @keywords internal
#' @noRd
empty_evidence_tibble <- function() {
  tibble::tibble(
    doc_id        = character(0),
    feature       = character(0),
    sentence_id   = integer(0),
    token_id      = integer(0),
    token         = character(0),
    lemma         = character(0),
    upos          = character(0),
    feats         = character(0),
    head_token_id = integer(0)
  )
}

# ----------------------------------------------------------------------
# Constructor and accessors of the block result contract
# ----------------------------------------------------------------------

#' Builds a canonical block_result
#'
#' The contract adopted by the blocks migrated in Phase 2:
#' return \code{list(counts, evidence)} instead of the plain data.frame.
#' Non-migrated blocks still return a data.frame and are normalized
#' via \code{as_block_result()} when consumed.
#'
#' @param counts data.frame with \code{doc_id} and one or more
#'   numeric columns \code{f_NN_*}.
#' @param evidence long tibble with the \code{.EVIDENCE_COLS} schema.
#'   Default: empty tibble.
#' @return A \code{list} object of class \code{"block_result"} with two
#'   elements: \code{counts} and \code{evidence}.
#' @keywords internal
#' @noRd
make_block_result <- function(counts, evidence = empty_evidence_tibble()) {
  if (!is.data.frame(counts)) {
    stop("'counts' debe ser un data.frame.", call. = FALSE)
  }
  if (!is.data.frame(evidence)) {
    stop("'evidence' debe ser un data.frame.", call. = FALSE)
  }
  if (nrow(evidence) > 0L) {
    missing_cols <- setdiff(.EVIDENCE_COLS, colnames(evidence))
    if (length(missing_cols) > 0L) {
      stop(
        "'evidence' no tiene el schema canonico. Faltan columnas: ",
        paste(missing_cols, collapse = ", "),
        call. = FALSE
      )
    }
  }
  structure(
    list(counts = counts, evidence = evidence),
    class = c("block_result", "list")
  )
}

#' Detects whether x is a block_result under the new contract
#'
#' @keywords internal
#' @noRd
is_block_result <- function(x) {
  inherits(x, "block_result") ||
    (is.list(x) && !is.data.frame(x) &&
       all(c("counts", "evidence") %in% names(x)))
}

#' Coerces a block return value to the block_result contract
#'
#' Accepts two formats:
#' \itemize{
#'   \item Old (pre-migration): \code{data.frame} with \code{doc_id} and
#'         \code{f_NN_*} columns. It is wrapped with empty evidence.
#'   \item New (post-migration): \code{list(counts, evidence)} already under
#'         the contract. It is returned unchanged.
#' }
#'
#' This function is the bridge that allows migrating blocks one by one without
#' breaking \code{parse_biber_features()}.
#'
#' @keywords internal
#' @noRd
as_block_result <- function(x) {
  if (is_block_result(x)) return(x)
  if (is.data.frame(x))   return(make_block_result(counts = x))
  stop(
    "El retorno de un bloque debe ser un data.frame (formato antiguo) ",
    "o list(counts, evidence) (contrato block_result). Tipo recibido: ",
    paste(class(x), collapse = "/"),
    call. = FALSE
  )
}

#' Extracts the counts data.frame from a block_result or data.frame
#'
#' Idempotent: if \code{x} is already a data.frame, it is returned unchanged.
#'
#' @keywords internal
#' @noRd
extract_counts <- function(x) {
  if (is_block_result(x)) return(x$counts)
  if (is.data.frame(x))   return(x)
  stop("extract_counts(): tipo no reconocido: ",
       paste(class(x), collapse = "/"), call. = FALSE)
}

#' Extracts the evidence tibble from a block_result or data.frame
#'
#' If \code{x} is a data.frame (old, non-migrated format), it returns
#' a canonical empty evidence tibble. This lets the code
#' that consumes evidence work uniformly on migrated
#' and non-migrated blocks.
#'
#' @keywords internal
#' @noRd
extract_evidence <- function(x) {
  if (is_block_result(x)) return(x$evidence)
  if (is.data.frame(x))   return(empty_evidence_tibble())
  stop("extract_evidence(): tipo no reconocido: ",
       paste(class(x), collapse = "/"), call. = FALSE)
}

#' Builds E1 evidence from token rows
#'
#' Helper for cases where the evidence is not derived from the same filter that
#' produces the count (e.g., passives where the count is at the
#' construction level and the evidence is the participle recovered via a join).
#' It explicitly maps the UD columns of the input data.frame to the
#' canonical E1 schema.
#'
#' @param rows data.frame with standard UD columns (or equivalents
#'   renamed via the col_* parameters).
#' @param feature_name string with the canonical name of the feature.
#' @param token_id_col,token_col,lemma_col,pos_col,feats_col,head_id_col
#'   column names in \code{rows} that map to the E1 schema. The defaults
#'   match the native UD columns after lower-casing the POS.
#' @return A tibble with the \code{.EVIDENCE_COLS} schema.
#' @keywords internal
#' @noRd
evidence_from_rows <- function(rows, feature_name,
                               token_id_col = "token_id_int",
                               token_col    = "token",
                               lemma_col    = "lemma",
                               pos_col      = "pos",
                               feats_col    = "feats",
                               head_id_col  = "head_token_id_int") {
  if (nrow(rows) == 0L) return(empty_evidence_tibble())
  tibble::tibble(
    doc_id        = as.character(rows$doc_id),
    feature       = feature_name,
    sentence_id   = as.integer(rows$sentence_id),
    token_id      = as.integer(rows[[token_id_col]]),
    token         = as.character(rows[[token_col]]),
    lemma         = as.character(rows[[lemma_col]]),
    upos          = as.character(rows[[pos_col]]),
    feats         = as.character(rows[[feats_col]]),
    head_token_id = as.integer(rows[[head_id_col]])
  )
}

#' Stacks evidence tibbles into a single one
#'
#' Wrapper around \code{dplyr::bind_rows()} that returns the canonical empty
#' tibble when there is nothing to stack, avoiding intermediate NULLs.
#'
#' @param ... one or more tibbles with the \code{.EVIDENCE_COLS} schema.
#' @keywords internal
#' @noRd
bind_evidence <- function(...) {
  args <- list(...)
  if (length(args) == 0L) return(empty_evidence_tibble())
  # Filter out NULLs and empty tibbles to avoid odd coercions
  args <- args[!vapply(args, is.null, logical(1))]
  if (length(args) == 0L) return(empty_evidence_tibble())
  dplyr::bind_rows(args)
}

# ----------------------------------------------------------------------
# Helper parallel to count_feature() that returns both: counts + evidence
# ----------------------------------------------------------------------

#' Variant of count_feature() that also returns evidence
#'
#' Same parameters and semantics as \code{count_feature()} in
#' \code{features_tense_pronouns.R}: takes an already filtered tbl, deduplicates
#' by (doc_id, sentence_id, token_id_int) with the same logic for
#' MWTs with NA token_id, and aggregates at the doc_id level.
#'
#' The difference is the return value: instead of a data.frame, it returns a
#' \code{block_result} with \code{counts} (same format as count_feature)
#' and \code{evidence} (long format E1, one row per token that survives
#' deduplication).
#'
#' @param tbl data.frame of already filtered tokens. It must have the
#'   standard UD columns: doc_id, sentence_id, token_id_int, token, lemma, pos,
#'   feats, head_token_id_int.
#' @param feature_name string with the canonical name of the feature
#'   (e.g. \code{"f_01_past_tense"}). It will be the value of the
#'   \code{feature} column in the evidence and the name of the count column.
#' @return A \code{block_result}.
#' @keywords internal
#' @noRd
count_feature_traced <- function(tbl, feature_name) {
  # Same dedup as count_feature() -- exact replica of the logic to
  # preserve count parity.
  dedup <- tbl %>%
    dplyr::mutate(
      .tid_dedup = dplyr::if_else(
        is.na(.data$token_id_int),
        -.Machine$integer.max + dplyr::row_number(),
        .data$token_id_int
      )
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$.tid_dedup, .keep_all = TRUE)

  counts <- dedup %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(!!feature_name := "n")

  evidence <- if (nrow(dedup) == 0L) {
    empty_evidence_tibble()
  } else {
    tibble::tibble(
      doc_id        = as.character(dedup$doc_id),
      feature       = feature_name,
      sentence_id   = as.integer(dedup$sentence_id),
      token_id      = as.integer(dedup$token_id_int),
      token         = as.character(dedup$token),
      lemma         = as.character(dedup$lemma),
      upos          = as.character(dedup$pos),
      feats         = as.character(dedup$feats),
      head_token_id = as.integer(dedup$head_token_id_int)
    )
  }

  make_block_result(counts = counts, evidence = evidence)
}
