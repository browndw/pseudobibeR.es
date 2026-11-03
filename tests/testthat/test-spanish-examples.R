# test-spanish-examples.R
# Examples labeled by feature, validated against biber() via UDPipe.
#
# Loads data-raw/spanish_examples.yaml (one row per example with feature/example/count),
# parses each sentence with the UD spanish-gsd model, runs biber(normalize = FALSE)
# and compares the observed count with the expected one.
#
# Some features diverge occasionally between parsers or need discourse context:
# they are marked in `relaxed_features` and only reported as information.
#
# nolint start: line_length_linter, object_name_linter

test_that("UDPipe spanish examples align with expected feature counts", {
  skip_if_not_installed("udpipe")
  skip_if_not_installed("yaml")
  skip_on_cran()

  yaml_path <- testthat::test_path("..", "..", "data-raw", "spanish_examples.yaml")
  if (!file.exists(yaml_path)) skip("spanish_examples.yaml no encontrado")

  # R in the "C" locale does not handle UTF-8 in readLines; open the connection with an
  # explicit encoding before passing it to yaml so that accented characters are not lost.
  yaml_con     <- file(yaml_path, encoding = "UTF-8")
  examples_raw <- yaml::yaml.load(paste(readLines(yaml_con, warn = FALSE), collapse = "\n"))
  close(yaml_con)
  spanish_examples <- tibble::tibble(
    feature = vapply(examples_raw, function(x) x$feature, character(1)),
    example = vapply(examples_raw, function(x) x$example, character(1)),
    count   = vapply(examples_raw, function(x) as.numeric(x$count), numeric(1))
  )

  # Features with known divergences in UDPipe spanish-gsd or that depend on
  # discourse context — reported as informational.
  relaxed_features <- c(
    "f_01_past_tense",
    "f_04_place_adverbials",
    "f_05_time_adverbials",
    "f_23_wh_clause",          # lemmatization of quién/cuándo varies with the accent
    "f_26_past_participle",    # absolute participle: deprel varies with the parser
    "f_39_prepositions",       # count varies with the structure of the sentence
    "f_45_conjuncts",
    "f_50_discourse_particles",
    "f_52_modal_possibility",
    "f_53_modal_necessity",
    "f_54_modal_predictive",
    "f_63_split_auxiliary"
  )

  # bug_skipped: list of features with an open bug that are excluded from the
  # comparison loop and reported as "SKIP BUG f_XX" via message().
  # Empty at the close of audit Phase 3 (commits fix(f_06/f_23/f_24/f_47)).
  # Grep-able marker: "BUG f_". If a bug is reopened, add it here.
  bug_skipped <- character(0)

  model_path <- .fc_model_path()
  skip_if(is.null(model_path) || !nzchar(model_path) || !file.exists(model_path),
          "UDPipe Spanish (spanish-gsd) no disponible")

  model <- udpipe::udpipe_load_model(model_path)
  if ("udpipe_free_model" %in% getNamespaceExports("udpipe")) {
    on.exit(udpipe::udpipe_free_model(model))
  }

  # One per feature (the first one) to keep the test reasonably fast
  sample_examples <- spanish_examples |>
    dplyr::group_by(.data$feature) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup()

  # unique doc_id per row (a feature can appear once after slice_head)
  annotations <- udpipe::udpipe_annotate(
    model,
    x      = sample_examples$example,
    doc_id = sample_examples$feature,
    parser = "default"
  )

  feature_counts <- biber(annotations, measure = "none", normalize = FALSE)

  missing_columns <- character()

  lookup_count <- function(feature_id) {
    doc_row <- dplyr::filter(feature_counts, .data$doc_id == feature_id)
    if (nrow(doc_row) != 1L) {
      return(NA_real_)
    }
    if (!feature_id %in% colnames(doc_row)) {
      missing_columns <<- unique(c(missing_columns, feature_id))
      return(0)
    }
    as.numeric(doc_row[[feature_id]])
  }

  # Report and exclude the features in bug_skipped before any comparison.
  # (Empty list at the close of Phase 3; the block stays as an anchor in case
  # a bug is reopened in the future.)
  bug_seen <- intersect(bug_skipped, sample_examples$feature)
  if (length(bug_seen) > 0) {
    for (bf in bug_seen) {
      message(paste0("SKIP BUG ", bf, " (ver audit/FEATURE_AUDIT.md)"))
    }
  }
  sample_examples <- sample_examples |>
    dplyr::filter(!(.data$feature %in% bug_skipped))

  comparison <- sample_examples |>
    dplyr::mutate(observed = purrr::map_dbl(.data$feature, lookup_count)) |>
    dplyr::mutate(matches = !is.na(.data$observed) & .data$observed == .data$count)

  strict <- comparison |> dplyr::filter(!(.data$feature %in% relaxed_features))
  mismatches <- strict |> dplyr::filter(!.data$matches)

  ignored_mismatches <- comparison |>
    dplyr::filter(.data$feature %in% relaxed_features) |>
    dplyr::filter(!.data$matches)

  problems <- character()

  if (length(missing_columns) > 0) {
    problems <- c(
      problems,
      paste0("Columnas de rasgo ausentes: ", paste(missing_columns, collapse = ", "))
    )
  }

  if (nrow(mismatches) > 0) {
    details <- mismatches |>
      dplyr::transmute(
        detail = paste0(
          .data$feature,
          ": esperado ", .data$count,
          ", observado ", .data$observed,
          " [\"", .data$example, "\"]"
        )
      ) |>
      dplyr::pull("detail")
    problems <- c(problems, paste(c("Divergencias:", details), collapse = "\n  "))
  }

  info_text <- if (length(problems) == 0) "" else paste(problems, collapse = "\n  ")
  testthat::expect_true(length(problems) == 0, info = info_text)

  if (nrow(ignored_mismatches) > 0) {
    ignored_details <- ignored_mismatches |>
      dplyr::transmute(
        detail = paste0(
          .data$feature,
          ": esperado ", .data$count,
          ", observado ", .data$observed
        )
      ) |>
      dplyr::pull("detail")
    message(paste(
      c("Divergencias UDPipe conocidas (informativas):", ignored_details),
      collapse = "\n  "
    ))
  }
})

# nolint end
