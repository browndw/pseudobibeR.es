diversity_tokens <- function(doc_id, words) {
  contract_clause(
    doc_id, words, words, rep("NOUN", length(words)),
    rep(0, length(words)), rep("root", length(words))
  )
}

test_that("default MATTR falls back once to TTR for the entire corpus", {
  long <- diversity_tokens("z_long", c(rep("libro", 120), rep("texto", 120)))
  short <- diversity_tokens("a_short", c("libro", "texto"))
  expect_message(alone <- biber(long, normalize = FALSE), NA)
  expect_equal(alone$f_43_type_token, 4 / 235)

  tokens <- rbind(long, short)
  spacy <- dplyr::rename(tokens, pos = "upos", tag = "xpos", morph = "feats")
  class(spacy) <- c("spacyr_parsed", "data.frame")
  for (input in list(tokens, spacy)) {
    for (normalize in c(FALSE, TRUE)) {
      messages <- character()
      result <- withCallingHandlers(
        biber(input, normalize = normalize),
        message = function(condition) {
          messages <<- c(messages, conditionMessage(condition))
          invokeRestart("muffleMessage")
        }
      )
      expect_identical(messages, "Setting type-to-token ratio to TTR\n")
      expect_equal(result$doc_id, c("z_long", "a_short"))
      expect_equal(result$f_43_type_token, c(2 / 240, 1))
    }
  }
  expect_message(
    explicit <- biber(tokens, measure = "MATTR", normalize = FALSE),
    "Setting type-to-token ratio to TTR"
  )
  expect_equal(explicit$f_43_type_token, c(2 / 240, 1))
})

test_that("the MATTR fallback threshold is strictly below 200 diversity tokens", {
  short <- diversity_tokens("short", c(rep("libro", 100), rep("texto", 99)))
  boundary <- diversity_tokens("boundary", c(rep("libro", 100), rep("texto", 100)))
  expect_message(
    below <- biber(short, normalize = FALSE),
    "Setting type-to-token ratio to TTR"
  )
  expect_equal(below$f_43_type_token, 2 / 199)
  expect_message(at <- biber(boundary, normalize = FALSE), NA)
  expect_equal(at$f_43_type_token, 2 / 101)

  punctuation <- boundary
  punctuation$token[200] <- punctuation$lemma[200] <- "."
  punctuation$upos[200] <- punctuation$xpos[200] <- "PUNCT"
  punctuation$dep_rel[200] <- "punct"
  punctuation$head_token_id[200] <- "199"
  expect_message(
    with_punctuation <- biber(punctuation, normalize = FALSE),
    "Setting type-to-token ratio to TTR"
  )
  expect_equal(with_punctuation$n_tokens, 199)
  expect_equal(with_punctuation$f_43_type_token, 2 / 199)

  numbers <- boundary
  numbers$token[200] <- numbers$lemma[200] <- "42"
  numbers$upos[200] <- numbers$xpos[200] <- "NUM"
  expect_message(
    with_numbers <- biber(numbers, normalize = FALSE),
    "Setting type-to-token ratio to TTR"
  )
  expect_equal(with_numbers$n_tokens, 200)
  expect_equal(with_numbers$f_43_type_token, 2 / 199)
})

test_that("explicit TTR and CTTR are retained across mixed document lengths", {
  tokens <- rbind(
    diversity_tokens("long", c(rep("libro", 120), rep("texto", 120))),
    diversity_tokens("short", c("libro", "texto"))
  )
  expected <- list(TTR = c(2 / 240, 1), CTTR = c(2 / sqrt(480), 1))
  for (measure in names(expected)) {
    expect_message(
      result <- biber(tokens, measure = measure, normalize = FALSE), NA
    )
    expect_equal(result$f_43_type_token, expected[[measure]])
  }
  expect_message(omitted <- biber(tokens, measure = "none"), NA)
  expect_false("f_43_type_token" %in% names(omitted))
})

test_that("explicit MSTTR uses common corpus settings without switching to TTR", {
  tokens <- rbind(
    diversity_tokens("long", c(rep("libro", 120), rep("texto", 120))),
    diversity_tokens("short", c("libro", "texto"))
  )
  expect_warning(
    result <- biber(tokens, measure = "MSTTR", normalize = FALSE),
    "MSTTR_segment.*resetting to 2"
  )
  expect_equal(result$f_43_type_token, c(0.5, 1))
})

test_that("lexical diversity retains first-appearance order beyond nine documents", {
  documents <- lapply(seq_len(12), function(i) {
    diversity_tokens(paste0("doc_", 13 - i), rep(c("libro", "texto"), i))
  })
  expect_message(
    result <- biber(do.call(rbind, documents), measure = "CTTR", normalize = FALSE),
    NA
  )
  expect_equal(result$doc_id, paste0("doc_", 12:1))
  expect_equal(result$f_43_type_token, 1 / sqrt(seq_len(12)))
})
