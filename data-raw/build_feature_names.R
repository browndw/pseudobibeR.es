# Build data/feature_names.rda from data-raw/feature_names.yaml.
# Run from the package root: Rscript data-raw/build_feature_names.R

mapping <- yaml::read_yaml(file.path("data-raw", "feature_names.yaml"))
feature_names <- vapply(mapping, as.character, character(1))

pattern <- "^f_[0-9]{2}_[a-z0-9_]+$"
bad <- !grepl(pattern, names(feature_names)) | !grepl(pattern, feature_names)
if (any(bad)) {
  stop("Malformed feature names: ", paste(names(feature_names)[bad], collapse = ", "))
}
renumbered <- substr(names(feature_names), 1, 5) != substr(feature_names, 1, 5)
if (any(renumbered)) {
  stop("A display name must keep its f_NN_ number: ",
       paste(names(feature_names)[renumbered], collapse = ", "))
}
if (anyDuplicated(names(feature_names)) || anyDuplicated(unname(feature_names))) {
  stop("Feature names must be unique.")
}

save(feature_names, file = file.path("data", "feature_names.rda"), compress = "xz")
message("Saved ", length(feature_names), " display names to data/feature_names.rda")
