lexical_input_tokens <- function(doc_id, words, pos = rep("NOUN", length(words)),
                                tags = pos) {
  tokens <- contract_clause(doc_id, words, words, pos,
                            rep(0, length(words)), rep("root", length(words)))
  tokens$xpos <- tags
  tokens
}

test_that("lexical diversity uses lower-case tagged surfaces, not lemmas", {
  tokens <- lexical_input_tokens(
    "types", c("Canto", "canto", "cant\u00f3", "42", ".", "\u20ac"),
    c("NOUN", "VERB", "VERB", "NUM", "PUNCT", "SYM")
  )
  tokens$lemma <- c("canto", "cantar", "cantar", "42", ".", "\u20ac")
  spacy <- dplyr::rename(tokens, pos = "upos", tag = "xpos", morph = "feats")
  class(spacy) <- c("spacyr_parsed", "data.frame")
  original <- quanteda::as.tokens(spacy, include_pos = "tag", concatenator = "_") %>%
    quanteda::tokens_tolower() %>%
    quanteda::tokens_remove("^\\W_", valuetype = "regex") %>%
    quanteda::tokens_remove("\\d_", valuetype = "regex")
  reference <- quanteda.textstats::textstat_lexdiv(original, measure = "TTR")
  expect_equal(reference$TTR, 1)
  for (input in list(tokens, spacy)) {
    for (normalize in c(FALSE, TRUE)) {
      result <- biber(input, measure = "TTR", normalize = normalize)
      expect_equal(result$f_43_type_token, reference$TTR)
      expect_equal(result$n_tokens, 4)
    }
  }
})

test_that("fine-grained tags are retained and missing tags use UPOS", {
  tokens <- lexical_input_tokens(
    "fine_tags", c("Lunes", "lunes"), c("NOUN", "NOUN"),
    c("NCMS000", "ncmp000")
  )
  fine <- biber(tokens, measure = "TTR")
  expect_equal(fine$f_43_type_token, 1)
  expect_equal(fine$f_44_mean_word_length, 5)
  tokens$xpos <- c("NCMS000", "ncms000")
  expect_equal(biber(tokens, measure = "TTR")$f_43_type_token, 0.5)

  missing <- lexical_input_tokens(
    "missing", c("Canto", "canto"), c("NOUN", "VERB")
  )
  for (tag in list(NA_character_, "", "_")) {
    missing$xpos <- rep(tag, 2)
    expect_equal(biber(missing, measure = "TTR")$f_43_type_token, 1)
  }
})

test_that("non-lexical padding does not affect any lexical-diversity measure", {
  base <- lexical_input_tokens(
    "padding", c(rep("libro", 120), rep("texto", 120))
  )
  padding <- lexical_input_tokens(
    "padding", c(".", "...", "\u2014", "42", "2026", "\u20ac", "+", " "),
    c("PUNCT", "PUNCT", "PUNCT", "NUM", "NUM", "SYM", "SYM", "SPACE"),
    c(".", "fp", "_", "CD", "NUM", "SYM", "_", "SPACE")
  )
  padding$sentence_id <- 2L
  for (measure in c("MATTR", "TTR", "CTTR", "MSTTR")) {
    expect_message(alone <- biber(base, measure = measure, normalize = FALSE), NA)
    expect_message(
      padded <- biber(rbind(base, padding), measure = measure, normalize = FALSE),
      NA
    )
    expect_equal(padded$f_43_type_token, alone$f_43_type_token)
  }
})

test_that("English-style numeric filtering keeps spelled-out numerals", {
  tokens <- lexical_input_tokens(
    "numbers", c("dos", "III", "12", "x2", "dos"),
    c("NUM", "NUM", "NUM", "NOUN", "NUM")
  )
  result <- biber(tokens, measure = "TTR", normalize = FALSE)
  expect_equal(result$f_43_type_token, 2 / 3)
  expect_equal(result$n_tokens, 5)
})

test_that("dictionary phrases do not compound lexical-diversity tokens", {
  tokens <- lexical_input_tokens(
    "phrase", c("Sin", "embargo", "sin", "otro"),
    c("ADP", "NOUN", "ADP", "ADJ")
  )
  result <- biber(tokens, measure = "TTR", normalize = FALSE)
  expect_equal(result$f_43_type_token, 3 / 4)
  expect_equal(result$f_45_conjuncts, 1)
  expect_equal(audit_features(tokens, feature = "f_45_conjuncts")$keyword,
               "Sin embargo")
})

test_that("filtered-empty documents retain an explicit undefined metric", {
  empty <- lexical_input_tokens(
    "empty", c(".", "42", "\u20ac"), c("PUNCT", "NUM", "SYM")
  )
  long <- lexical_input_tokens(
    "long", c(rep("libro", 100), rep("texto", 100))
  )
  for (measure in c("MATTR", "TTR", "CTTR", "MSTTR")) {
    expect_warning(
      result <- suppressMessages(biber(rbind(long, empty), measure = measure,
                                        normalize = FALSE)),
      "no lexical-diversity tokens.*empty"
    )
    expect_equal(result$doc_id, c("long", "empty"))
    expect_true(is.finite(result$f_43_type_token[1]))
    expect_true(is.na(result$f_43_type_token[2]))
  }
  expect_warning(
    first_empty <- biber(rbind(empty, long), measure = "TTR", normalize = FALSE),
    "no lexical-diversity tokens.*empty"
  )
  expect_equal(first_empty$doc_id, c("empty", "long"))
  expect_equal(first_empty$f_43_type_token, c(NA_real_, 2 / 200))
  expect_warning(
    all_empty <- biber(empty, measure = "TTR", normalize = FALSE),
    "no lexical-diversity tokens.*empty"
  )
  expect_true(is.na(all_empty$f_43_type_token))
  expect_warning(omitted <- biber(empty, measure = "none", normalize = FALSE), NA)
  expect_false("f_43_type_token" %in% names(omitted))
})
