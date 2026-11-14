aspect_clause <- function(doc_id, token, lemma, verbform, heads, relations,
                          upos = c(rep("AUX", length(token) - 1L), "VERB")) {
  data.frame(
    doc_id = doc_id, sentence_id = 1L, token_id = as.character(seq_along(token)),
    token = token, lemma = lemma, upos = upos, xpos = upos,
    feats = ifelse(is.na(verbform), NA_character_, paste0("VerbForm=", verbform)),
    head_token_id = as.character(heads), dep_rel = relations
  )
}

perfect_aspect_clauses <- function() {
  clauses <- list(
    aspect_clause("active", c("ha", "revisado"), c("haber", "revisar"),
                  c("Fin", "Part"), c(2, 0), c("aux", "root")),
    aspect_clause("progressive", c("esta", "revisando"), c("estar", "revisar"),
                  c("Fin", "Ger"), c(2, 0), c("aux", "root")),
    aspect_clause("stative", c("esta", "cerrada"), c("estar", "cerrado"),
                  c("Fin", "Part"), c(2, 0), c("cop", "root"), c("AUX", "ADJ")),
    aspect_clause("progressive_passive", c("esta", "siendo", "revisado"),
                  c("estar", "ser", "revisar"), c("Fin", "Ger", "Part"),
                  c(3, 3, 0), c("aux", "aux:pass", "root")),
    aspect_clause("perfect_resultative", c("ha", "estado", "cerrada"),
                  c("haber", "estar", "cerrar"), c("Fin", "Part", "Part"),
                  c(3, 3, 0), c("aux", "aux", "root")),
    aspect_clause("perfect_passive", c("ha", "sido", "revisado"),
                  c("haber", "ser", "revisar"), c("Fin", "Part", "Part"),
                  c(3, 3, 0), c("aux", "aux:pass", "root")),
    aspect_clause("modal_passive", c("ha", "de", "ser", "revisado"),
                  c("haber", "de", "ser", "revisar"), c("Fin", NA, "Inf", "Part"),
                  c(4, 3, 4, 0), c("aux", "mark", "aux:pass", "root"),
                  c("AUX", "ADP", "AUX", "VERB")),
    aspect_clause("perfect_progressive", c("ha", "estado", "revisando"),
                  c("haber", "estar", "revisar"), c("Fin", "Part", "Ger"),
                  c(3, 3, 0), c("aux", "aux", "root")),
    aspect_clause("perfect_copula", c("ha", "estado", "enferma"),
                  c("haber", "estar", "enfermo"), c("Fin", "Part", NA),
                  c(3, 3, 0), c("aux", "cop", "root"), c("AUX", "AUX", "ADJ")),
    aspect_clause("nested_modal", c("ha", "de", "haber", "sido", "revisado"),
                  c("haber", "de", "haber", "ser", "revisar"),
                  c("Fin", NA, "Inf", "Part", "Part"), c(5, 3, 5, 5, 0),
                  c("aux", "mark", "aux", "aux:pass", "root"),
                  c("AUX", "ADP", "AUX", "AUX", "VERB")),
    aspect_clause("other_auxiliary", c("tengo", "revisado"), c("tener", "revisar"),
                  c("Fin", "Part"), c(2, 0), c("aux", "root")),
    aspect_clause("nested_dependencies", c("ha", "sido", "revisado"),
                  c("haber", "ser", "revisar"), c("Fin", "Part", "Part"),
                  c(2, 3, 0), c("aux", "aux:pass", "root")),
    aspect_clause("fronted_participle", c("revisado", "ha"), c("revisar", "haber"),
                  c("Part", "Fin"), c(0, 1), c("root", "aux"), c("VERB", "AUX")),
    aspect_clause("separated", c("ha", "probablemente", "revisado"),
                  c("haber", "probablemente", "revisar"), c("Fin", NA, "Part"),
                  c(3, 3, 0), c("aux", "advmod", "root"), c("AUX", "ADV", "VERB"))
  )
  clauses[[6]]$feats[3] <- "VerbForm=Part|Voice=Pass"
  clauses
}

tense_fixture <- function(i, feats) {
  x <- contract_tokens(paste0("tense_", i))
  x$feats[2] <- feats
  x
}

tense_feats <- c(
  "Mood=Ind|Tense=Past|VerbForm=Fin", "Mood=Ind|Tense=Imp|VerbForm=Fin",
  "Mood=Sub|Tense=Imp|VerbForm=Fin", "Mood=Ind|Tense=Pqp|VerbForm=Fin",
  "Mood=Ind|Tense=Pres|VerbForm=Fin", "Mood=Sub|Tense=Pres|VerbForm=Fin",
  "Mood=Cnd|VerbForm=Fin", "Mood=Ind|Tense=Fut|VerbForm=Fin",
  "Tense=Past|VerbForm=Part", "VerbForm=Inf", NA_character_
)

person_tokens <- function() {
  data.frame(
    doc_id = "persons", sentence_id = 1L, token_id = as.character(1:10),
    token = c("yo", "mi", "mio", "usted", "vos", "tu", "ella", "su", "se", "el"),
    lemma = c("yo", "mi", "m\u00edo", "t\u00fa", "t\u00fa", "tu", "\u00e9l",
              "su", "\u00e9l", "el"),
    upos = c("PRON", "DET", "NOUN", "PRON", "NOUN", "DET", "PRON", "DET",
             "PRON", "DET"),
    xpos = "X",
    feats = c("Person=1|PronType=Prs", "Poss=Yes|Person=1", NA,
              "Person=2|Polite=Form|PronType=Prs", NA, "Poss=Yes|Person=2",
              "Person=3|PronType=Prs", "Poss=Yes|Person=3",
              "Person=3|Reflex=Yes", "PronType=Art"),
    head_token_id = "0", dep_rel = "dep"
  )
}

indefinite_tokens <- function() {
  data.frame(
    doc_id = paste0("indefinite_", 1:6), sentence_id = 1L, token_id = "1",
    token = c("uno", "un", "cualquiera", "todo", "todos", "alguien"),
    lemma = c("uno", "uno", "cualquiera", "todo", "todo", "alguien"),
    upos = c("PRON", "DET", "ADJ", "DET", "DET", "PRON"), xpos = "X",
    feats = c("PronType=Ind", "PronType=Art", "PronType=Ind",
              "PronType=Ind", "PronType=Ind", "PronType=Ind"),
    head_token_id = "0", dep_rel = c("obj", "det", "amod", "det", "det:predet", "nsubj")
  )
}

demonstrative_tokens <- function() {
  data.frame(
    doc_id = "demonstratives", sentence_id = 1L, token_id = as.character(1:3),
    token = c("esto", "este", "este"), lemma = c("este", "este", "este"),
    upos = c("PRON", "PRON", "DET"), xpos = "X", feats = "PronType=Dem",
    head_token_id = "0", dep_rel = c("nsubj", "obj", "det")
  )
}

# Pipeline calls have a large fixed cost: the independent UDPipe-schema
# fixtures above run as one corpus (one raw call, one audit call); counts and
# audit rows are per document, so each test reads its own documents.
tense_batch <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      tokens <- rbind(
        do.call(rbind, perfect_aspect_clauses()),
        do.call(rbind, lapply(seq_along(tense_feats), function(i) {
          tense_fixture(i, tense_feats[i])
        })),
        person_tokens(), indefinite_tokens(), demonstrative_tokens()
      )
      features <- c("f_02_perfect_aspect", "f_01_past_tense", "f_03_present_tense",
                    "f_06_first_person_pronouns", "f_07_second_person_pronouns",
                    "f_08_third_person_pronouns", "f_11_indefinite_pronouns",
                    "f_10_demonstrative_pronoun")
      cache <<- list(raw = biber_fixture(tokens, measure = "none", normalize = FALSE),
                     audit = audit_fixture(tokens, feature = features))
    }
    cache
  }
})

# Rows of a batched result for the documents of `tokens`, in result order.
tense_rows <- function(tokens) {
  raw <- tense_batch()$raw
  raw <- raw[raw$doc_id %in% tokens$doc_id, , drop = FALSE]
  rownames(raw) <- NULL
  raw
}

tense_audit <- function(tokens, feature) {
  audit <- tense_batch()$audit
  audit[audit$doc_id %in% tokens$doc_id & audit$feature == feature, ]
}

test_that("perfect aspect counts haber compounds, not progressive or modal chains", {
  clauses <- perfect_aspect_clauses()
  tokens <- do.call(rbind, clauses)
  raw <- tense_rows(tokens)
  expected <- c(1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1)
  expect_equal(raw$doc_id, vapply(clauses, function(x) x$doc_id[1], character(1)))
  expect_equal(raw$f_02_perfect_aspect, expected)

  audit <- tense_audit(tokens, "f_02_perfect_aspect")
  expect_equal(tabulate(match(audit$doc_id, raw$doc_id), nbins = nrow(raw)), expected)
  expect_equal(audit$keyword, ifelse(audit$doc_id == "nested_modal", "haber", "ha"))
  expect_equal(biber_fixture(tokens, measure = "none")$f_02_perfect_aspect,
               1000 * expected / word_tokens(tokens))

  spacy <- dplyr::rename(tokens, pos = "upos", tag = "xpos", morph = "feats")
  spacy$dep_rel[spacy$dep_rel == "aux:pass"] <- "auxpass"
  class(spacy) <- c("spacyr_parsed", "data.frame")
  expect_equal(biber_fixture(spacy, measure = "none", normalize = FALSE)$f_02_perfect_aspect,
               expected)
})

test_that("perfect complements stay within their sentence and document", {
  first <- aspect_clause("first", c("ha", "revisado"), c("haber", "revisar"),
                         c("Fin", "Part"), c(2, 0), c("aux", "root"))
  first$sentence_id <- c(1L, 2L)
  second <- first
  second$doc_id <- c("second", "third")
  raw <- biber_fixture(rbind(first, second), measure = "none", normalize = FALSE)
  expect_equal(raw$f_02_perfect_aspect, c(0, 0, 0))
})

test_that("finite past and present rules count every mood but exclude nonfinite forms", {
  feats <- tense_feats
  tokens <- do.call(rbind, lapply(seq_along(feats), function(i) {
    tense_fixture(i, feats[i])
  }))
  raw <- tense_rows(tokens)
  expect_equal(raw$f_01_past_tense, c(rep(1, 4), rep(0, 7)))
  # The present subjunctive counts in f_03, as the imperfect subjunctive does in f_01.
  expect_equal(raw$f_03_present_tense, c(rep(0, 4), 1, 1, rep(0, 5)))
  for (feature in c("f_01_past_tense", "f_03_present_tense")) {
    audit <- tense_audit(tokens, feature)
    expect_equal(tabulate(match(audit$doc_id, raw$doc_id), nbins = nrow(raw)),
                 raw[[feature]])
  }
})

test_that("personal pronouns and possessives retain the documented person routing", {
  tokens <- person_tokens()
  raw <- tense_rows(tokens)
  features <- c("f_06_first_person_pronouns", "f_07_second_person_pronouns",
                "f_08_third_person_pronouns")
  expect_equal(as.numeric(raw[1, features]), c(3, 3, 2))
  for (feature in features) {
    audit <- tense_audit(tokens, feature)
    expect_equal(sum(audit$count), raw[[feature]])
  }
  expect_equal(raw$f_09_pronoun_it, 0)
})

test_that("indefinite pronouns exclude articles, adjectives, and determiner subtypes", {
  tokens <- indefinite_tokens()
  raw <- tense_rows(tokens)
  expect_equal(raw$f_11_indefinite_pronouns, c(1, 0, 0, 0, 0, 1))
  audit <- tense_audit(tokens, "f_11_indefinite_pronouns")
  expect_equal(audit$keyword, c("uno", "alguien"))
})

test_that("demonstrative pronouns do not count as demonstrative determiners", {
  tokens <- demonstrative_tokens()
  raw <- tense_rows(tokens)
  expect_equal(raw$f_10_demonstrative_pronoun, 2)
  expect_equal(raw$f_51_demonstratives, 1)
  expect_equal(sum(tense_audit(tokens, "f_10_demonstrative_pronoun")$count), 2)
})

test_that("Spanish-GSD preserves the validated aspect contrasts", {
  model <- .fc_ud()
  texts <- c(
    progressive_passive = "El informe est\u00e1 siendo revisado.",
    perfect_resultative = "La puerta ha estado cerrada.",
    perfect_passive = "El informe ha sido revisado.",
    pluperfect = "El informe hab\u00eda sido revisado.",
    modal_passive = "El informe ha de ser revisado.",
    stative = "La puerta est\u00e1 cerrada."
  )
  tokens <- udpipe::udpipe_annotate(model, x = texts, doc_id = names(texts))
  raw <- biber_fixture(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$doc_id, names(texts))
  expect_equal(raw$f_02_perfect_aspect, c(0, 1, 1, 1, 0, 0))
  expect_equal(raw$f_01_past_tense, c(0, 0, 0, 1, 0, 0))
  audit <- audit_fixture(tokens, feature = "f_02_perfect_aspect")
  expect_equal(audit$keyword, ifelse(audit$doc_id == "pluperfect", "hab\u00eda", "ha"))
  expect_equal(sum(audit$count), 3)
})
