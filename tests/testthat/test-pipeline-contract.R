test_that("biber supports both parser schemas without requiring a parser runtime", {
  ud <- contract_tokens()
  sp <- dplyr::rename(ud, pos = "upos", tag = "xpos")
  sp$morph <- sp$feats
  sp$feats <- NULL
  class(sp) <- c("spacyr_parsed", "data.frame")
  expect_equal(biber(ud, measure = "TTR", normalize = FALSE),
               biber(sp, measure = "TTR", normalize = FALSE))
})

test_that("calls with the same effective measure agree in document order", {
  first <- contract_tokens("z_first")
  second <- contract_tokens("a_second")
  second$token[2] <- second$lemma[2] <- "llegar"
  second$feats[2] <- "VerbForm=Inf"
  documents <- list(first, second)
  tokens <- do.call(rbind, documents)

  for (measure in c("MATTR", "TTR", "CTTR", "none")) {
    for (normalize in c(FALSE, TRUE)) {
      together <- suppressMessages(biber(
        tokens, measure = measure, normalize = normalize
      ))
      separate <- suppressMessages(dplyr::bind_rows(lapply(documents, biber,
        measure = measure, normalize = normalize
      )))
      expect_equal(together$doc_id, c("z_first", "a_second"))
      expect_equal(together, separate, ignore_attr = TRUE)
    }
  }
})

test_that("normalization uses UD words, not punctuation or lexical categories", {
  tokens <- contract_tokens()
  raw <- biber(tokens, measure = "TTR", normalize = FALSE)
  rate <- biber(tokens, measure = "TTR", normalize = TRUE)
  expect_equal(raw$n_tokens, 3)
  expect_equal(rate$f_03_present_tense, 1000 / 3)
  expect_equal(rate$f_43_type_token, raw$f_43_type_token)
  expect_equal(rate$f_44_mean_word_length, raw$f_44_mean_word_length)
  expect_named(rate, c("doc_id", names(.feature_categories), "n_tokens", "n_lex_tokens"))
})

test_that("dictionary and grammatical paths do not count an infinitive twice", {
  tokens <- contract_tokens()
  tokens$token[2] <- tokens$lemma[2] <- "pensar"
  tokens$feats[2] <- "VerbForm=Inf"
  tokens$dep_rel[2] <- "xcomp"
  tokens$head_token_id[2] <- "1"
  expect_equal(biber(tokens, measure = "none", normalize = FALSE)$f_56_verb_private, 1)
})

test_that("adjective fallback is document-local", {
  tokens <- rbind(contract_tokens("a"), contract_tokens("b"))
  tokens$token[c(1, 5)] <- c("bonito", "bueno")
  tokens$lemma[c(1, 5)] <- c("bonito", "bueno")
  tokens$upos[c(1, 5)] <- "ADJ"
  tokens$upos[c(2, 6)] <- "NOUN"
  tokens$dep_rel[c(1, 5)] <- c("dep", "amod")
  alone <- biber(tokens[tokens$doc_id == "a", ], measure = "none", normalize = FALSE)
  together <- biber(tokens, measure = "none", normalize = FALSE)
  expect_equal(alone$f_40_adj_attr, 1)
  expect_equal(together$f_40_adj_attr[together$doc_id == "a"], alone$f_40_adj_attr)
})

test_that("input validation identifies missing columns and duplicate token keys", {
  tokens <- contract_tokens()
  expect_error(biber(tokens[, names(tokens) != "head_token_id"]), "head_token_id")
  expect_error(biber(rbind(tokens, tokens[1, ])), "Duplicate token")
  tokens$doc_id[1] <- NA
  expect_error(biber(tokens), "doc_id")
  expect_error(biber(contract_tokens(), normalize = NA), "normalize")
})
