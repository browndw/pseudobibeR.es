resource_isolation_case <- function() {
  tokens <- rbind(
    contract_tokens("private"),
    contract_clause(
      "conjunct", c("Sin", "embargo"), c("sin", "embargo"),
      c("ADP", "NOUN"), c(2, 0), c("case", "root")
    ),
    contract_clause(
      "preposition", c("a", "causa", "de", "Ana"),
      c("a", "causa", "de", "Ana"), c("ADP", "NOUN", "ADP", "PROPN"),
      c(2, 4, 4, 0), c("case", "nmod", "case", "root")
    )
  )
  spacy <- dplyr::rename(tokens, pos = "upos", tag = "xpos", morph = "feats")
  class(spacy) <- c("spacyr_parsed", "data.frame")
  inputs <- list(udpipe = tokens, spacy = spacy)
  features <- c("f_56_verb_private", "f_45_conjuncts", "f_39_prepositions")
  expected <- lapply(inputs, function(input) {
    list(
      raw = pseudobibeR.es::biber(input, measure = "none", normalize = FALSE),
      normalized = pseudobibeR.es::biber(input, measure = "none"),
      audit = as.data.frame(pseudobibeR.es::audit_features(input, feature = features))
    )
  })
  foreign <- list(
    dict = quanteda::dictionary(lapply(as.list(pseudobibeR.es::dict),
                                       function(x) "unmatched_resource")),
    word_lists = lapply(pseudobibeR.es::word_lists, function(x) "unmatched resource")
  )
  list(inputs = inputs, features = features, expected = expected, foreign = foreign)
}

with_resource_mask <- function(resources, position, code) {
  attach(resources, pos = position, name = "pseudobibeR_es_foreign_resources",
         warn.conflicts = FALSE)
  on.exit(detach("pseudobibeR_es_foreign_resources"), add = TRUE)
  force(code)
}

test_that("counts and audits ignore datasets from other attached environments", {
  case <- resource_isolation_case()
  raw <- case$expected$udpipe$raw
  expect_equal(raw$f_56_verb_private[raw$doc_id == "private"], 1)
  expect_equal(raw$f_45_conjuncts[raw$doc_id == "conjunct"], 1)
  expect_equal(raw$f_39_prepositions[raw$doc_id == "preposition"], 1)

  for (position in c(2L, length(search()))) {
    with_resource_mask(case$foreign, position, {
      for (name in names(case$inputs)) {
        input <- case$inputs[[name]]
        expect_equal(
          pseudobibeR.es::biber(input, measure = "none", normalize = FALSE),
          case$expected[[name]]$raw
        )
        expect_equal(pseudobibeR.es::biber(input, measure = "none"),
                     case$expected[[name]]$normalized)
        expect_equal(
          as.data.frame(pseudobibeR.es::audit_features(input, feature = case$features)),
          case$expected[[name]]$audit
        )
      }
    })
  }
})

check_installed_resource_isolation <- function(case) {
  input <- tempfile(fileext = ".rds")
  output <- tempfile(fileext = ".log")
  on.exit(unlink(c(input, output)), add = TRUE)
  saveRDS(case, input)
  script <- r"---(
    args <- commandArgs(trailingOnly = TRUE)
    .libPaths(c(args[-1L], .libPaths()))
    case <- readRDS(args[1L])
    check <- function() {
      for (name in names(case$inputs)) {
        input <- case$inputs[[name]]
        stopifnot(
          identical(
            pseudobibeR.es::biber(input, measure = "none", normalize = FALSE),
            case$expected[[name]]$raw
          ),
          identical(
            pseudobibeR.es::biber(input, measure = "none"),
            case$expected[[name]]$normalized
          ),
          identical(
            as.data.frame(pseudobibeR.es::audit_features(input, feature = case$features)),
            case$expected[[name]]$audit
          )
        )
      }
    }
    stopifnot(!"package:pseudobibeR.es" %in% search())
    check()
    stopifnot(!"package:pseudobibeR.es" %in% search())
    attach(case$foreign, name = "foreign_resources", warn.conflicts = FALSE)
    suppressPackageStartupMessages(library(pseudobibeR.es))
    check()
    detach("package:pseudobibeR.es")
    detach("foreign_resources")
    suppressPackageStartupMessages(library(pseudobibeR.es))
    attach(case$foreign, name = "foreign_resources", warn.conflicts = FALSE)
    check()
  )---"
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", "-e", shQuote(script), shQuote(input), shQuote(.libPaths())),
    stdout = output, stderr = output
  )
  expect_equal(status, 0L, info = paste(readLines(output, warn = FALSE), collapse = "\n"))
}

test_that("installed namespace-only calls and both attachment orders are isolated", {
  skip_if_not(
    file.exists(file.path(find.package("pseudobibeR.es"), "Meta", "package.rds")),
    "Requires an installed package; exercised by R CMD check."
  )
  check_installed_resource_isolation(resource_isolation_case())
})
