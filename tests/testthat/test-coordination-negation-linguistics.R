test_that("split auxiliaries exclude lexical complements and use the immediate chain link", {
  modal <- contract_clause(
    "modal", c("puede", "facilmente", "resolver"), c("poder", "facilmente", "resolver"),
    c("VERB", "ADV", "VERB"), c(0, 3, 1), c("root", "advmod", "xcomp"),
    c("VerbForm=Fin", NA, "VerbForm=Inf")
  )
  lexical <- modal
  lexical$doc_id <- "lexical"
  lexical$token[1] <- "quiere"
  lexical$lemma[1] <- "querer"
  perfect <- contract_clause(
    "perfect", c("ha", "realmente", "terminado"), c("haber", "realmente", "terminar"),
    c("AUX", "ADV", "VERB"), c(3, 3, 0), c("aux", "advmod", "root"),
    c("VerbForm=Fin", NA, "VerbForm=Part")
  )
  chain <- contract_clause(
    "chain", c("ha", "sido", "cuidadosamente", "revisado"),
    c("haber", "ser", "cuidadosamente", "revisar"), c("AUX", "AUX", "ADV", "VERB"),
    c(4, 4, 4, 0), c("aux", "aux:pass", "advmod", "root"),
    c("VerbForm=Fin", "VerbForm=Part", NA, "VerbForm=Part")
  )
  tokens <- rbind(modal, lexical, perfect, chain)
  raw <- biber_fixture(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_63_split_auxiliary, c(1, 0, 1, 1))
  audit <- audit_fixture(tokens, feature = "f_63_split_auxiliary")
  expect_equal(audit$keyword[match(c("modal", "perfect", "chain"), audit$doc_id)],
               c("puede", "ha", "sido"))
  expect_equal(biber_fixture(tokens, measure = "none")$f_63_split_auxiliary,
               1000 * raw$f_63_split_auxiliary / word_tokens(tokens))
  double <- contract_clause(
    "double", c("ha", "probablemente", "sido", "cuidadosamente", "revisado"),
    c("haber", "probablemente", "ser", "cuidadosamente", "revisar"),
    c("AUX", "ADV", "AUX", "ADV", "VERB"), c(5, 5, 5, 5, 0),
    c("aux", "advmod", "aux:pass", "advmod", "root"),
    c("VerbForm=Fin", NA, "VerbForm=Part", NA, "VerbForm=Part")
  )
  expect_equal(biber_fixture(double, measure = "none", normalize = FALSE)$f_63_split_auxiliary, 2)
})

test_that("the analytic negator alone does not split an auxiliary (D-49)", {
  parsed <- as.data.frame(udpipe::udpipe_annotate(
    .fc_ud(),
    x = c("\u00c9l puede no venir ma\u00f1ana.", "Puede no realmente entenderlo.",
          "No ha venido."),
    doc_id = c("neg_only", "neg_adv", "preverbal")
  ))
  raw <- biber_fixture(parsed, measure = "none", normalize = FALSE)
  expect_equal(raw$f_63_split_auxiliary, c(0, 1, 0))
})

test_that("nominal coordination permits common and proper nouns", {
  tokens <- contract_clause(
    "nominals", c("Juan", "y", "hermana"), c("Juan", "y", "hermana"),
    c("PROPN", "CCONJ", "NOUN"), c(0, 3, 1), c("root", "cc", "conj")
  )
  raw <- biber_fixture(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_64_phrasal_coordination, 1)
  expect_equal(raw$f_65_clausal_coordination, 0)
  expect_equal(audit_fixture(tokens, feature = "f_64_phrasal_coordination")$keyword, "y")
})

test_that("copular clauses with their own subjects are clausal coordination", {
  tokens <- contract_clause(
    "copular", c("ella", "esta", "contenta", "y", "el", "esta", "triste"),
    c("ella", "estar", "contento", "y", "el", "estar", "triste"),
    c("PRON", "AUX", "ADJ", "CCONJ", "PRON", "AUX", "ADJ"),
    c(3, 3, 0, 7, 7, 7, 3), c("nsubj", "cop", "root", "cc", "nsubj", "cop", "conj"),
    c(NA, "VerbForm=Fin", NA, NA, NA, "VerbForm=Fin", NA)
  )
  raw <- biber_fixture(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_64_phrasal_coordination, 0)
  expect_equal(raw$f_65_clausal_coordination, 1)
  expect_equal(audit_fixture(tokens, feature = "f_65_clausal_coordination")$keyword, "y")
  shared <- contract_clause(
    "shared", c("ella", "canta", "y", "esta", "contenta"),
    c("ella", "cantar", "y", "estar", "contento"),
    c("PRON", "VERB", "CCONJ", "AUX", "ADJ"), c(2, 0, 5, 5, 2),
    c("nsubj", "root", "cc", "cop", "conj"),
    c(NA, "VerbForm=Fin", NA, "VerbForm=Fin", NA)
  )
  raw_shared <- biber_fixture(shared, measure = "none", normalize = FALSE)
  expect_equal(raw_shared$f_64_phrasal_coordination, 1)
  expect_equal(raw_shared$f_65_clausal_coordination, 0)
  clausal_subjects <- contract_clause(
    "clausal_subjects", c("estudiar", "ayuda", "y", "dormir", "relaja"),
    c("estudiar", "ayudar", "y", "dormir", "relajar"),
    c("VERB", "VERB", "CCONJ", "VERB", "VERB"), c(2, 0, 5, 5, 2),
    c("csubj", "root", "cc", "csubj", "conj"),
    c("VerbForm=Inf", "VerbForm=Fin", NA, "VerbForm=Inf", "VerbForm=Fin")
  )
  raw_subjects <- biber_fixture(clausal_subjects, measure = "none", normalize = FALSE)
  expect_equal(raw_subjects$f_64_phrasal_coordination, 0)
  expect_equal(raw_subjects$f_65_clausal_coordination, 1)
})

test_that("sentence-initial coordination uses sentence position and needs a finite predicate", {
  clause <- contract_clause(
    "clause", c("Y", "vino"), c("y", "venir"), c("CCONJ", "VERB"),
    c(2, 0), c("cc", "root"), c(NA, "VerbForm=Fin")
  )
  clause$token_id <- c("7", "8")
  clause$head_token_id[1] <- "8"
  fragment <- contract_clause(
    "fragment", c("Y", "Juan"), c("y", "Juan"), c("CCONJ", "PROPN"),
    c(2, 0), c("cc", "root")
  )
  raw <- biber_fixture(rbind(clause, fragment), measure = "none", normalize = FALSE)
  expect_equal(raw$f_65_clausal_coordination, c(1, 0))
  quoted <- contract_clause(
    "quoted", c("\"", "Y", "vino", "\""), c("\"", "y", "venir", "\""),
    c("PUNCT", "CCONJ", "VERB", "PUNCT"), c(3, 3, 0, 3),
    c("punct", "cc", "root", "punct"), c(NA, NA, "VerbForm=Fin", NA)
  )
  expect_equal(biber_fixture(quoted, measure = "none", normalize = FALSE)$f_65_clausal_coordination, 1)
})

test_that("negation preserves concord and accepts standard and spaCy negative relations", {
  tokens <- contract_clause(
    "negation", c("No", "vino", "nadie", "ni", "tampoco", "llamo"),
    c("no", "venir", "nadie", "ni", "tampoco", "llamar"),
    c("ADV", "VERB", "PRON", "CCONJ", "ADV", "VERB"),
    c(2, 0, 2, 6, 6, 2), c("advmod", "root", "nsubj", "cc", "advmod", "conj"),
    c(NA, "VerbForm=Fin", NA, NA, NA, "VerbForm=Fin")
  )
  raw <- biber_fixture(tokens, measure = "none", normalize = FALSE)
  expect_equal(raw$f_66_neg_synthetic, 3)
  expect_equal(raw$f_67_neg_analytic, 1)
  expect_setequal(audit_fixture(tokens, feature = "f_66_neg_synthetic")$keyword,
                  c("nadie", "ni", "tampoco"))
  spacy <- dplyr::rename(tokens, pos = "upos", tag = "xpos", morph = "feats")
  spacy$pos[1] <- "PART"
  spacy$dep_rel[1] <- "neg"
  class(spacy) <- c("spacyr_parsed", "data.frame")
  expect_equal(biber_fixture(spacy, measure = "none", normalize = FALSE)$f_67_neg_analytic, 1)
  noun <- tokens[1:2, ]
  noun$upos[1] <- noun$xpos[1] <- "NOUN"
  expect_equal(biber_fixture(noun, measure = "none", normalize = FALSE)$f_67_neg_analytic, 0)
})

test_that("Spanish-GSD contrasts lexical splits, nominal coordination and copular clauses", {
  parsed <- as.data.frame(udpipe::udpipe_annotate(
    .fc_ud(),
    x = c("Quiero realmente estudiar.", "Puede f\u00e1cilmente hacerlo.",
          "Ha sido cuidadosamente revisado el informe.", "Juan y su hermana llegaron.",
          "Ella est\u00e1 contenta y \u00e9l est\u00e1 triste.",
          "No vino nadie ni tampoco llam\u00f3."),
    doc_id = c("lexical", "modal", "chain", "nominal", "clausal", "negation")
  ))
  raw <- biber_fixture(parsed, measure = "none", normalize = FALSE)
  expect_equal(raw$f_63_split_auxiliary, c(0, 1, 1, 0, 0, 0))
  expect_equal(raw$f_64_phrasal_coordination, c(0, 0, 0, 1, 0, 1))
  expect_equal(raw$f_65_clausal_coordination, c(0, 0, 0, 0, 1, 0))
  expect_equal(raw$f_66_neg_synthetic, c(0, 0, 0, 0, 0, 3))
  expect_equal(raw$f_67_neg_analytic, c(0, 0, 0, 0, 0, 1))
})
