# test-spanish-basic.R
# Synthetic tests for Biber features in Spanish (without UDPipe)
#
# The tokens are built by hand in UDPipe format (upos/xpos columns).
# biber_fixture() renames upos→pos and xpos→tag before calling parse_biber_features().
#
# Convention: each test checks ONE feature with a minimal sentence.
# nolint start: line_length_linter, object_name_linter

# ─── Helper: build a tibble in UDPipe format ──────────────────────────────
make_es_tokens <- function(...) {
  tbl <- tibble::tribble(...)
  if (!"feats" %in% colnames(tbl)) tbl$feats <- NA_character_
  tbl
}

run_es <- function(tokens, normalize = FALSE) {
  biber_fixture(tokens, measure = "none", normalize = normalize)
}

# Single-document fixtures for the one-feature tests below. Each pipeline
# call has a large fixed cost, so they run as one corpus (counts are
# per document) on first use.
basic_fixtures <- list(
  past = make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token,  ~lemma,   ~upos,  ~xpos,  ~dep_rel, ~head_token_id, ~feats,
    "d1",    1L,           1L,        "llegó", "llegar", "VERB", "VERB", "root",   NA_integer_,    "Mood=Ind|Number=Sing|Person=3|Tense=Past|VerbForm=Fin",
    "d1",    1L,           2L,        ".",     ".",      "PUNCT","PUNCT","punct",  1L,             NA_character_
  ),
  present = make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token,    ~lemma,    ~upos,  ~xpos,  ~dep_rel, ~head_token_id, ~feats,
    "d1",    1L,           1L,        "estudia", "estudiar","VERB", "VERB", "root",   NA_integer_,    "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "d1",    1L,           2L,        ".",       ".",       "PUNCT","PUNCT","punct",  1L,             NA_character_
  ),
  perfect = make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token,      ~lemma,      ~upos,  ~xpos,  ~dep_rel, ~head_token_id, ~feats,
    "d1",    1L,           1L,        "he",        "haber",     "AUX",  "AUX",  "aux",    2L,             "Mood=Ind|Number=Sing|Person=1|Tense=Pres|VerbForm=Fin",
    "d1",    1L,           2L,        "terminado", "terminar",  "VERB", "VERB", "root",   NA_integer_,    "Gender=Masc|Number=Sing|Tense=Past|VerbForm=Part",
    "d1",    1L,           3L,        ".",         ".",         "PUNCT","PUNCT","punct",  2L,             NA_character_
  ),
  first_person = make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token,      ~lemma,      ~upos,  ~xpos,  ~dep_rel, ~head_token_id, ~feats,
    "d1",    1L,           1L,        "yo",        "yo",        "PRON", "PRON", "nsubj",  2L,             "Case=Nom|Number=Sing|Person=1|PronType=Prs",
    "d1",    1L,           2L,        "creo",      "creer",     "VERB", "VERB", "root",   NA_integer_,    "Mood=Ind|Number=Sing|Person=1|Tense=Pres|VerbForm=Fin",
    "d1",    1L,           3L,        ".",         ".",         "PUNCT","PUNCT","punct",  2L,             NA_character_
  ),
  second_person = make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token,  ~lemma,   ~upos,  ~xpos,  ~dep_rel, ~head_token_id, ~feats,
    "d1",    1L,           1L,        "tú",    "tú",     "PRON", "PRON", "nsubj",  2L,             "Case=Nom|Number=Sing|Person=2|PronType=Prs",
    "d1",    1L,           2L,        "sabes", "saber",  "VERB", "VERB", "root",   NA_integer_,    "Mood=Ind|Number=Sing|Person=2|Tense=Pres|VerbForm=Fin",
    "d1",    1L,           3L,        ".",     ".",      "PUNCT","PUNCT","punct",  2L,             NA_character_
  ),
  third_person = make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token,  ~lemma,   ~upos,  ~xpos,  ~dep_rel, ~head_token_id, ~feats,
    "d1",    1L,           1L,        "él",    "él",     "PRON", "PRON", "nsubj",  2L,             "Case=Nom|Gender=Masc|Number=Sing|Person=3|PronType=Prs",
    "d1",    1L,           2L,        "sabe",  "saber",  "VERB", "VERB", "root",   NA_integer_,    "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "d1",    1L,           3L,        ".",     ".",      "PUNCT","PUNCT","punct",  2L,             NA_character_
  ),
  agentless_passive = make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token,     ~lemma,    ~upos,  ~xpos,  ~dep_rel,   ~head_token_id, ~feats,
    "d1",    1L,           1L,        "fue",      "ser",     "AUX",  "AUX",  "aux:pass", 2L,             "Mood=Ind|Number=Sing|Person=3|Tense=Past|VerbForm=Fin",
    "d1",    1L,           2L,        "aprobada", "aprobar", "VERB", "VERB", "root",     NA_integer_,    "Gender=Fem|Number=Sing|Tense=Past|VerbForm=Part|Voice=Pass",
    "d1",    1L,           3L,        ".",        ".",       "PUNCT","PUNCT","punct",    2L,             NA_character_
  ),
  by_passive = make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token,   ~lemma,    ~upos,  ~xpos,  ~dep_rel,   ~head_token_id, ~feats,
    "d1",    1L,           1L,        "fue",    "ser",     "AUX",  "AUX",  "aux:pass", 2L,             "Mood=Ind|Number=Sing|Person=3|Tense=Past|VerbForm=Fin",
    "d1",    1L,           2L,        "escrito","escribir","VERB", "VERB", "root",     NA_integer_,    "Gender=Masc|Number=Sing|Tense=Past|VerbForm=Part|Voice=Pass",
    "d1",    1L,           3L,        "por",   "por",     "ADP",  "ADP",  "case",     4L,             NA_character_,
    "d1",    1L,           4L,        "él",    "él",      "PRON", "PRON", "obl:agent",2L,             "Case=Nom|Gender=Masc|Number=Sing|Person=3|PronType=Prs",
    "d1",    1L,           5L,        ".",     ".",       "PUNCT","PUNCT","punct",    2L,             NA_character_
  ),
  neg_synthetic = make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token,  ~lemma,  ~upos,  ~xpos,  ~dep_rel, ~head_token_id, ~feats,
    "d1",    1L,           1L,        "nadie", "nadie", "PRON", "PRON", "nsubj",  2L,             "PronType=Neg",
    "d1",    1L,           2L,        "sabe",  "saber", "VERB", "VERB", "root",   NA_integer_,    "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "d1",    1L,           3L,        ".",     ".",     "PUNCT","PUNCT","punct",  2L,             NA_character_
  ),
  neg_analytic = make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token,  ~lemma,  ~upos,  ~xpos,  ~dep_rel, ~head_token_id, ~feats,
    "d1",    1L,           1L,        "no",    "no",    "ADV",  "ADV",  "advmod", 2L,             "Polarity=Neg",
    "d1",    1L,           2L,        "sabe",  "saber", "VERB", "VERB", "root",   NA_integer_,    "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "d1",    1L,           3L,        ".",     ".",     "PUNCT","PUNCT","punct",  2L,             NA_character_
  ),
  contraction = make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token, ~lemma, ~upos,  ~xpos,  ~dep_rel, ~head_token_id, ~feats,
    "d1",    1L,           1L,        "voy",  "ir",   "VERB", "VERB", "root",   NA_integer_,    "Mood=Ind|Number=Sing|Person=1|Tense=Pres|VerbForm=Fin",
    "d1",    1L,           2L,        "al",   "al",   "ADP",  "ADP",  "case",   3L,             NA_character_,
    "d1",    1L,           3L,        "mercado","mercado","NOUN","NOUN","obl",   1L,             NA_character_,
    "d1",    1L,           4L,        ".",    ".",    "PUNCT","PUNCT","punct",  1L,             NA_character_
  )
)

basic_row <- local({
  counts <- NULL
  function(name) {
    if (is.null(counts)) counts <<- biber_docs(basic_fixtures, measure = "none", normalize = FALSE)
    counts[match(name, names(basic_fixtures)), , drop = FALSE]
  }
})

# ─── Verb tense ───────────────────────────────────────────────────────────

test_that("f_01_past_tense cuenta pretérito indefinido (Tense=Past)", {
  res <- basic_row("past")
  expect_equal(res$f_01_past_tense, 1)
  # f_71_preterit REMOVED (Q1=A, audit): pseudobibeR.es exposes only
  # Biber's 57 features; the imperfect past is no longer a column.
  expect_false("f_71_preterit" %in% colnames(res))
})

test_that("f_03_present_tense cuenta presente de indicativo", {
  res <- basic_row("present")
  expect_equal(res$f_03_present_tense, 1)
})

test_that("f_02_perfect_aspect cuenta haber + participio", {
  res <- basic_row("perfect")
  expect_equal(res$f_02_perfect_aspect, 1)
})

# ─── Personal pronouns (pro-drop: explicit ones only) ─────────────────────

test_that("f_06 cuenta pronombres de primera persona explícitos", {
  res <- basic_row("first_person")
  expect_equal(res$f_06_first_person_pronouns, 1)
})

test_that("f_07 cuenta pronombres de segunda persona", {
  res <- basic_row("second_person")
  expect_equal(res$f_07_second_person_pronouns, 1)
})

test_that("f_08 cuenta pronombres de tercera persona", {
  res <- basic_row("third_person")
  expect_equal(res$f_08_third_person_pronouns, 1)
})

# ─── Passive ──────────────────────────────────────────────────────────────

test_that("f_17 cuenta pasiva perifrástica sin agente", {
  res <- basic_row("agentless_passive")
  expect_equal(res$f_17_agentless_passives, 1)
  expect_equal(res$f_18_by_passives,        0)
})

test_that("f_18 cuenta pasiva perifrástica con agente (por)", {
  res <- basic_row("by_passive")
  expect_equal(res$f_18_by_passives, 1)
})

# ─── Negation ─────────────────────────────────────────────────────────────

test_that("f_66 cuenta negación sintética (nadie, nunca, nada)", {
  res <- basic_row("neg_synthetic")
  expect_equal(res$f_66_neg_synthetic, 1)
})

test_that("f_67 cuenta negación analítica (no + verbo)", {
  res <- basic_row("neg_analytic")
  expect_equal(res$f_67_neg_analytic, 1)
})

# ─── Contractions ─────────────────────────────────────────────────────────
# f_59_contractions is untranslatable into Spanish (in UD the contractions del/al
# are tokenized as two tokens; there is no orthographic contraction equivalent to
# English). Since v0.024 the column DOES appear in the output with a permanent
# value of 0 for surface parity with pseudobibeR.fr.

test_that("f_59_contractions presente y siempre 0 (paridad FR)", {
  res <- basic_row("contraction")
  expect_true("f_59_contractions" %in% colnames(res))
  expect_equal(as.numeric(res$f_59_contractions[1]), 0)
})

# ─── Output structure ─────────────────────────────────────────────────────

test_that("biber devuelve una fila por documento sin errores", {
  tks <- make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token,    ~lemma,    ~upos,  ~xpos,  ~dep_rel, ~head_token_id, ~feats,
    "doc1",  1L,           1L,        "estudia", "estudiar","VERB", "VERB", "root",   NA_integer_,    "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "doc1",  1L,           2L,        "mucho",   "mucho",   "ADV",  "ADV",  "advmod", 1L,             NA_character_,
    "doc2",  1L,           1L,        "llegó",   "llegar",  "VERB", "VERB", "root",   NA_integer_,    "Mood=Ind|Number=Sing|Person=3|Tense=Past|VerbForm=Fin",
    "doc2",  1L,           2L,        "tarde",   "tarde",   "ADV",  "ADV",  "advmod", 1L,             NA_character_
  )
  res <- run_es(tks)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_true("doc_id" %in% colnames(res))
  expect_true("f_01_past_tense" %in% colnames(res))
  expect_false("f_71_preterit"  %in% colnames(res))  # extension removed (Q1=A)
  expect_true("f_03_present_tense" %in% colnames(res))
})

test_that("biber con normalize=TRUE devuelve valores numéricos sin NA en columnas f_", {
  tks <- make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token,      ~lemma,      ~upos,  ~xpos,  ~dep_rel, ~head_token_id, ~feats,
    "d1",    1L,           1L,        "he",        "haber",     "AUX",  "AUX",  "aux",    2L,             "Mood=Ind|Number=Sing|Person=1|Tense=Pres|VerbForm=Fin",
    "d1",    1L,           2L,        "terminado", "terminar",  "VERB", "VERB", "root",   NA_integer_,    "Gender=Masc|Number=Sing|Tense=Past|VerbForm=Part",
    "d1",    1L,           3L,        "el",        "el",        "DET",  "DET",  "det",    4L,             "Definite=Def|Gender=Masc|Number=Sing|PronType=Art",
    "d1",    1L,           4L,        "trabajo",   "trabajo",   "NOUN", "NOUN", "obj",    2L,             "Gender=Masc|Number=Sing",
    "d1",    1L,           5L,        ".",         ".",         "PUNCT","PUNCT","punct",  2L,             NA_character_
  )
  res <- run_es(tks, normalize = TRUE)
  feat_cols <- grep("^f_", colnames(res), value = TRUE)
  expect_true(length(feat_cols) > 10)
  for (col in feat_cols) {
    expect_true(is.numeric(res[[col]]),
                info = paste("Columna no numérica:", col))
  }
})

# nolint end
