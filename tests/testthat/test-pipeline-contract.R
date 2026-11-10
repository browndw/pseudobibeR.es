test_that("biber supports both parser schemas without requiring a parser runtime", {
  ud <- contract_tokens()
  sp <- dplyr::rename(ud, pos = "upos", tag = "xpos")
  sp$morph <- sp$feats
  sp$feats <- NULL
  class(sp) <- c("spacyr_parsed", "data.frame")
  expect_equal(biber_fixture(ud, measure = "TTR", normalize = FALSE),
               biber_fixture(sp, measure = "TTR", normalize = FALSE))
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
      together <- suppressMessages(biber_fixture(
        tokens, measure = measure, normalize = normalize
      ))
      separate <- suppressMessages(dplyr::bind_rows(lapply(documents, biber_fixture,
        measure = measure, normalize = normalize
      )))
      expect_equal(together$doc_id, c("z_first", "a_second"))
      expect_equal(together, separate, ignore_attr = TRUE)
    }
  }
})

test_that("normalization uses UD words, not punctuation or lexical categories", {
  tokens <- contract_tokens()
  raw <- biber_fixture(tokens, measure = "TTR", normalize = FALSE)
  rate <- biber_fixture(tokens, measure = "TTR", normalize = TRUE)
  expect_equal(word_tokens(tokens), 3)
  expect_equal(rate$f_03_present_tense, 1000 / 3)
  expect_equal(rate$f_43_type_token, raw$f_43_type_token)
  expect_equal(rate$f_44_mean_word_length, raw$f_44_mean_word_length)
  expect_named(rate, c("doc_id", names(.feature_categories)))
})

test_that("dictionary and grammatical paths do not count an infinitive twice", {
  tokens <- contract_tokens()
  tokens$token[2] <- tokens$lemma[2] <- "pensar"
  tokens$feats[2] <- "VerbForm=Inf"
  tokens$dep_rel[2] <- "xcomp"
  tokens$head_token_id[2] <- "1"
  expect_equal(biber_fixture(tokens, measure = "none", normalize = FALSE)$f_56_verb_private, 1)
})

test_that("adjective fallback is document-local", {
  tokens <- rbind(contract_tokens("a"), contract_tokens("b"))
  tokens$token[c(1, 5)] <- c("bonito", "bueno")
  tokens$lemma[c(1, 5)] <- c("bonito", "bueno")
  tokens$upos[c(1, 5)] <- "ADJ"
  tokens$upos[c(2, 6)] <- "NOUN"
  tokens$dep_rel[c(1, 5)] <- c("dep", "amod")
  alone <- biber_fixture(tokens[tokens$doc_id == "a", ], measure = "none", normalize = FALSE)
  together <- biber_fixture(tokens, measure = "none", normalize = FALSE)
  expect_equal(alone$f_40_adj_attr, 1)
  expect_equal(together$f_40_adj_attr[together$doc_id == "a"], alone$f_40_adj_attr)
})

test_that("input validation identifies missing columns and duplicate token keys", {
  tokens <- contract_tokens()
  expect_error(biber_fixture(tokens[, names(tokens) != "head_token_id"]), "head_token_id")
  expect_error(biber_fixture(rbind(tokens, tokens[1, ])), "Duplicate token")
  tokens$doc_id[1] <- NA
  expect_error(biber_fixture(tokens), "doc_id")
  expect_error(biber_fixture(contract_tokens(), normalize = NA), "normalize")
})

test_that("exported functions accept parser annotations, not plain data frames", {
  plain <- contract_tokens()
  expect_error(pseudobibeR.es::biber(plain), "no applicable method")
  expect_error(pseudobibeR.es::audit_features(plain, feature = "f_03_present_tense"),
               "no applicable method")
  skip_if_not_installed("udpipe")
  annotation <- as_udpipe_annotation(plain)
  expect_equal(pseudobibeR.es::biber(annotation, measure = "none", normalize = FALSE),
               biber_fixture(plain, measure = "none", normalize = FALSE))
})
