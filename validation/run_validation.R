#!/usr/bin/env Rscript
# Validation script for pseudobibeR.es
# Usage: Rscript validation/run_validation.R

Sys.setlocale("LC_ALL", "en_US.UTF-8")

suppressPackageStartupMessages({
  library(udpipe)
  library(yaml)
  library(dplyr)
})

# Load the local package in dev mode
if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Install 'pkgload' before running development validation.")
}
pkgload::load_all(quiet = TRUE)

# ─── CONFIGURATION ───────────────────────────────────────────────────────────

CORPUS_FILE  <- "validation/test_corpus.yaml"
OUTPUT_DIR   <- "validation/output"
PER_TEXT_DIR <- file.path(OUTPUT_DIR, "per_text_results")

# Default tolerances
DEFAULT_COUNT_TOLERANCE <- 1   # ±1 token for counts
DEFAULT_RATIO_TOLERANCE <- 0.1 # ±0.1 for ratios (TTR, mean length)

# Features that are ratios or metrics (not counts)
RATIO_FEATURES <- c("f_43_type_token", "f_44_mean_word_length")

# ─── PREPARATION ─────────────────────────────────────────────────────────────

dir.create(OUTPUT_DIR,   showWarnings = FALSE, recursive = TRUE)
dir.create(PER_TEXT_DIR, showWarnings = FALSE, recursive = TRUE)

cat("=== Validación de pseudobibeR.es ===\n\n")

# Load the UDPipe model
model_path <- Sys.getenv(
  "PSEUDOBIBER_ES_UDPIPE_MODEL",
  "tests/models/spanish-gsd-ud-2.5-191206.udpipe"
)
if (!file.exists(model_path)) stop("Local Spanish-GSD model not found: ", model_path)
ud_model <- udpipe_load_model(model_path)
cat("Modelo cargado:", basename(model_path), "\n\n")

# Load the corpus
corpus <- yaml::read_yaml(CORPUS_FILE)
cat("Textos de prueba cargados:", length(corpus$texts), "\n\n")

# ─── COMPARISON FUNCTIONS ────────────────────────────────────────────────────

classify_result <- function(expected, observed, tolerance, is_ratio = FALSE) {
  # Returns: "OK", "TOLERANCE", or "FAIL"
  if (is.na(observed)) return("MISSING")

  if (is_ratio) {
    diff <- abs(observed - expected)
    if (diff <= tolerance) return("OK")
    if (diff <= tolerance * 2) return("TOLERANCE")
    return("FAIL")
  } else {
    diff <- abs(observed - expected)
    if (diff <= tolerance) return("OK")
    if (diff <= tolerance + 1) return("TOLERANCE")
    # Systematic false positives: if 0 was expected and it returns >2, FAIL
    if (expected == 0 && observed > 2) return("FAIL")
    if (expected > 0 && observed == 0) return("FAIL")
    if (diff > tolerance + 1) return("FAIL")
    return("FAIL")
  }
}

get_tolerance <- function(feature, custom_tolerances) {
  if (!is.null(custom_tolerances) && feature %in% names(custom_tolerances)) {
    return(custom_tolerances[[feature]])
  }
  if (feature %in% RATIO_FEATURES) return(DEFAULT_RATIO_TOLERANCE)
  return(DEFAULT_COUNT_TOLERANCE)
}

# ─── TEXT PROCESSING ─────────────────────────────────────────────────────────

all_results <- list()

for (i in seq_along(corpus$texts)) {
  test <- corpus$texts[[i]]
  cat(sprintf("[%d/%d] %s — %s\n", i, length(corpus$texts), test$id, test$registro))

  # Annotate the text
  parsed <- udpipe_annotate(
    ud_model,
    x = test$text,
    doc_id = test$id,
    tagger = "default",
    parser = "default"
  )
  parsed_df <- as.data.frame(parsed)

  # Extract features (raw counts, not normalized)
  features <- biber(parsed_df, measure = "TTR", normalize = FALSE)

  # Compare with the expected values
  expected <- test$expected
  custom_tolerances <- test$tolerance

  comparison <- data.frame(
    text_id = test$id,
    registro = test$registro,
    feature = names(expected),
    expected = unlist(expected),
    observed = NA_real_,
    tolerance = NA_real_,
    status = NA_character_,
    delta = NA_real_,
    notes = NA_character_,
    stringsAsFactors = FALSE
  )

  for (j in seq_len(nrow(comparison))) {
    feat <- comparison$feature[j]
    exp_val <- comparison$expected[j]

    # Get the observed value
    if (feat %in% names(features)) {
      obs_val <- as.numeric(features[[feat]][1])
    } else {
      obs_val <- NA_real_
    }

    tol <- get_tolerance(feat, custom_tolerances)
    is_ratio <- feat %in% RATIO_FEATURES
    status <- classify_result(exp_val, obs_val, tol, is_ratio)

    comparison$observed[j]  <- obs_val
    comparison$tolerance[j] <- tol
    comparison$status[j]    <- status
    comparison$delta[j]     <- if (!is.na(obs_val)) obs_val - exp_val else NA
  }

  # Save the results per text
  per_text_file <- file.path(PER_TEXT_DIR, paste0(test$id, ".csv"))
  write.csv(comparison, per_text_file, row.names = FALSE)

  all_results[[test$id]] <- comparison

  # Summary of the text
  status_counts <- table(comparison$status)
  cat(sprintf("    OK: %d  |  TOLERANCE: %d  |  FAIL: %d  |  MISSING: %d\n",
              ifelse("OK" %in% names(status_counts), status_counts[["OK"]], 0),
              ifelse("TOLERANCE" %in% names(status_counts), status_counts[["TOLERANCE"]], 0),
              ifelse("FAIL" %in% names(status_counts), status_counts[["FAIL"]], 0),
              ifelse("MISSING" %in% names(status_counts), status_counts[["MISSING"]], 0)))
}

# ─── GLOBAL REPORT ───────────────────────────────────────────────────────────

cat("\n=== Generando reporte global ===\n")

# Combine all the results
combined <- bind_rows(all_results)

# Global CSV
write.csv(combined, file.path(OUTPUT_DIR, "validation_report.csv"), row.names = FALSE)

# Analysis per feature (which features fail systematically)
by_feature <- combined %>%
  group_by(feature) %>%
  summarise(
    total_tests = n(),
    ok = sum(status == "OK"),
    tolerance = sum(status == "TOLERANCE"),
    fail = sum(status == "FAIL"),
    missing = sum(status == "MISSING"),
    fail_rate = round(fail / total_tests * 100, 1),
    avg_delta = round(mean(delta, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(fail_rate), desc(fail))

# Analysis per text
by_text <- combined %>%
  group_by(text_id, registro) %>%
  summarise(
    total = n(),
    ok = sum(status == "OK"),
    tolerance = sum(status == "TOLERANCE"),
    fail = sum(status == "FAIL"),
    missing = sum(status == "MISSING"),
    pass_rate = round((ok + tolerance) / total * 100, 1),
    .groups = "drop"
  )

# ─── MARKDOWN REPORT ─────────────────────────────────────────────────────────

report <- c(
  "# Reporte de validación: pseudobibeR.es",
  "",
  paste("Generado:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste("Modelo UDPipe:", basename(model_path[1])),
  paste("Textos evaluados:", length(corpus$texts)),
  "",
  "## Resumen global",
  ""
)

total_tests   <- nrow(combined)
total_ok      <- sum(combined$status == "OK")
total_tol     <- sum(combined$status == "TOLERANCE")
total_fail    <- sum(combined$status == "FAIL")
total_missing <- sum(combined$status == "MISSING")

report <- c(report,
  sprintf("- **Total comparaciones**: %d", total_tests),
  sprintf("- **OK**: %d (%.1f%%)", total_ok, total_ok / total_tests * 100),
  sprintf("- **TOLERANCE**: %d (%.1f%%)", total_tol, total_tol / total_tests * 100),
  sprintf("- **FAIL**: %d (%.1f%%)", total_fail, total_fail / total_tests * 100),
  sprintf("- **MISSING**: %d (%.1f%%)", total_missing, total_missing / total_tests * 100),
  "",
  "**Leyenda**:",
  "- OK: |observed - expected| <= tolerance",
  "- TOLERANCE: dentro de 2x tolerance",
  "- FAIL: fuera de tolerancia",
  "- MISSING: rasgo no presente en output del paquete",
  ""
)

# Table per text
report <- c(report, "## Resultados por texto", "")
report <- c(report,
  "| Texto | Registro | Total | OK | Tol | Fail | Miss | Pass rate |",
  "|-------|----------|-------|-----|-----|------|------|-----------|"
)
for (k in seq_len(nrow(by_text))) {
  row <- by_text[k, ]
  report <- c(report,
    sprintf("| %s | %s | %d | %d | %d | %d | %d | %s%% |",
            row$text_id, row$registro, row$total,
            row$ok, row$tolerance, row$fail, row$missing,
            row$pass_rate))
}
report <- c(report, "")

# Table of problematic features (with at least one FAIL)
problematic <- by_feature %>% filter(fail > 0 | missing > 0) %>% arrange(desc(fail), desc(missing))
report <- c(report, "## Rasgos con problemas (al menos 1 fallo)", "")
if (nrow(problematic) == 0) {
  report <- c(report, "No hay rasgos con fallos.", "")
} else {
  report <- c(report,
    "| Rasgo | Tests | OK | Tol | Fail | Miss | Fail rate | Δ medio |",
    "|-------|-------|-----|-----|------|------|-----------|---------|"
  )
  for (k in seq_len(nrow(problematic))) {
    row <- problematic[k, ]
    report <- c(report,
      sprintf("| `%s` | %d | %d | %d | %d | %d | %s%% | %+.2f |",
              row$feature, row$total_tests, row$ok, row$tolerance,
              row$fail, row$missing, row$fail_rate, row$avg_delta))
  }
  report <- c(report, "")
}

# Detail per text
report <- c(report, "## Detalle por texto", "")
for (id in names(all_results)) {
  res <- all_results[[id]]
  reg <- res$registro[1]
  report <- c(report, sprintf("### %s — %s", id, reg), "")

  # Only features with problems
  fails <- res %>% filter(status %in% c("FAIL", "MISSING"))
  warns <- res %>% filter(status == "TOLERANCE")

  if (nrow(fails) > 0) {
    report <- c(report, "#### Fallos", "")
    report <- c(report,
      "| Rasgo | Esperado | Observado | Δ | Tolerancia | Estado |",
      "|-------|----------|-----------|---|------------|--------|"
    )
    for (k in seq_len(nrow(fails))) {
      f <- fails[k, ]
      obs_str <- ifelse(is.na(f$observed), "—", as.character(f$observed))
      delta_str <- ifelse(is.na(f$delta), "—", sprintf("%+.0f", f$delta))
      report <- c(report,
        sprintf("| `%s` | %s | %s | %s | %s | %s |",
                f$feature, f$expected, obs_str, delta_str,
                f$tolerance, f$status))
    }
    report <- c(report, "")
  }

  if (nrow(warns) > 0) {
    report <- c(report, "#### Dentro de tolerancia (revisar)", "")
    report <- c(report,
      "| Rasgo | Esperado | Observado | Δ |",
      "|-------|----------|-----------|---|"
    )
    for (k in seq_len(nrow(warns))) {
      f <- warns[k, ]
      report <- c(report,
        sprintf("| `%s` | %s | %s | %+.0f |",
                f$feature, f$expected, f$observed, f$delta))
    }
    report <- c(report, "")
  }

  if (nrow(fails) == 0 && nrow(warns) == 0) {
    report <- c(report, "Todos los rasgos correctos.", "")
  }
}

# Section on systemic problems
sistemicos <- problematic %>% filter(fail >= 3)  # fails in 3+ texts
report <- c(report, "## Problemas sistémicos (fallan en >=3 textos)", "")
if (nrow(sistemicos) == 0) {
  report <- c(report, "No hay problemas sistémicos.", "")
} else {
  report <- c(report,
    "Estos rasgos fallan en múltiples textos y deben corregirse con prioridad:",
    ""
  )
  for (k in seq_len(nrow(sistemicos))) {
    row <- sistemicos[k, ]
    report <- c(report,
      sprintf("- **`%s`**: falla en %d/%d textos (Δ medio: %+.2f)",
              row$feature, row$fail, row$total_tests, row$avg_delta))
  }
  report <- c(report, "")
}

# Write the report
report_file <- file.path(OUTPUT_DIR, "validation_report.md")
writeLines(report, report_file)

cat("\n=== Validación completada ===\n")
cat("Reporte:", report_file, "\n")
cat("CSV:", file.path(OUTPUT_DIR, "validation_report.csv"), "\n")
cat("Por texto:", PER_TEXT_DIR, "\n\n")

# Print a summary to the console
cat(sprintf("Total: %d | OK: %d | Tol: %d | Fail: %d | Miss: %d\n",
            total_tests, total_ok, total_tol, total_fail, total_missing))
