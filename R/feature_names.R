# Language-specific display names for feature columns

#' Map shared feature identifiers to display names
#'
#' Detectors, evidence and the family decision log use shared identifiers
#' (`f_15_gerunds`). Output shows the display names in [feature_names], built
#' from `data-raw/feature_names.yaml`, unless `feature_names = "shared"`. The
#' `f_NN_` number is the same feature in every package of the family.
#'
#' @param ids Column names or feature identifiers.
#' @param feature_names `"language"` (display names) or `"shared"`.
#' @return `ids` with mapped identifiers replaced.
#' @keywords internal
display_feature_names <- function(ids, feature_names = "language") {
  if (identical(feature_names, "shared")) return(ids)
  map <- pseudobibeR.es::feature_names
  hit <- ids %in% names(map)
  ids[hit] <- unname(map[ids[hit]])
  ids
}

#' Map display names back to shared feature identifiers
#'
#' Lets [audit_features()] accept either name (`f_15_nominal_infinitives` or
#' `f_15_gerunds`).
#'
#' @param names Feature names.
#' @return Shared identifiers.
#' @keywords internal
shared_feature_ids <- function(names) {
  if (is.null(names)) return(names)
  map <- pseudobibeR.es::feature_names
  idx <- match(names, map)
  names[!is.na(idx)] <- names(map)[idx[!is.na(idx)]]
  names
}

#' Validate the feature_names argument
#'
#' @param feature_names Argument value.
#' @return `"language"` or `"shared"`.
#' @keywords internal
match_feature_names <- function(feature_names) {
  if (!is.character(feature_names) || length(feature_names) != 1L ||
      !feature_names %in% c("language", "shared")) {
    stop("'feature_names' must be \"language\" or \"shared\".", call. = FALSE)
  }
  feature_names
}
