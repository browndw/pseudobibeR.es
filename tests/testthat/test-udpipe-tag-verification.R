# test-udpipe-tag-verification.R
#
# Empirical gate of §1.2 of INSTRUCCIONES_CLAUDE_CODE_pseudobiber_es.md:
# "Verify the UD tags empirically before writing any
#  condition." This file ANNOTATES diagnostic sentences with the real
# spanish-gsd model and ASSERTS the tags that the extraction rules
# depend on. If a future version of the model changes a tag, or if
# someone assumes a tag that the model does not produce, these tests fail.
#
# It also DOCUMENTS (through comments and expectations) the known limits
# of the model that forced deviations from Hernan's linguistic review:
#   - argumental reflexive `se` vs. reflexive passive: INDISTINGUISHABLE.
#   - passive agent (`por`) vs. cause (`por`): INDISTINGUISHABLE (both `obl`).
#   - relative `que`: SCONJ/mark, without PronType=Rel (subject/object not separable).
#   - Tense=Pqp: the model does not emit it (the pluperfect comes out as Tense=Imp).
#
# nolint start: line_length_linter, object_name_linter

# Reuses the memoized model of helper-feature-coverage.R (.fc_ud), which
# calls skip() if the model is not available. That way it does not depend on helpers in
# subdirectories that testthat does not load automatically.
annotate_es <- function(text) {
  ud <- .fc_ud()
  as.data.frame(udpipe::udpipe_annotate(ud, x = text, doc_id = "d1"))
}

feat_of <- function(df, token_lc) {
  row <- df[tolower(df$token) == token_lc, , drop = FALSE]
  if (nrow(row) == 0) return(NA_character_)
  row$feats[[1]]
}

has_feat <- function(feats, key_eq) {
  !is.na(feats) && grepl(key_eq, feats, fixed = TRUE)
}

get_model_or_skip <- function() {
  skip_if_not_installed("udpipe")
  invisible(NULL)
}

test_that("posesivos: DET con Poss=Yes y Person (base de f_06/f_07/f_08)", {
  mp <- get_model_or_skip()
  df <- annotate_es("Nuestro equipo revisó mi propuesta y tu informe y su plan.")

  # mi/tu/su/nuestro are DET, Poss=Yes, with a reliable morphological Person.
  expect_true(has_feat(feat_of(df, "nuestro"), "Poss=Yes"))
  expect_true(has_feat(feat_of(df, "nuestro"), "Person=1"))
  expect_true(has_feat(feat_of(df, "mi"),  "Person=1"))
  expect_true(has_feat(feat_of(df, "tu"),  "Person=2"))
  expect_true(has_feat(feat_of(df, "su"),  "Person=3"))
})

test_that("f_01: pluscuamperfecto e imperfecto de subjuntivo salen como Tense=Imp, no Pqp", {
  mp <- get_model_or_skip()
  df <- annotate_es("Ya había corrido y ojalá hubiera corrido; quería que corriera.")

  # 'había' (pluperfect ind.) and 'hubiera'/'corriera' (subjunctive) → Tense=Imp.
  expect_true(has_feat(feat_of(df, "había"),   "Tense=Imp"))
  expect_true(has_feat(feat_of(df, "hubiera"), "Tense=Imp"))
  expect_true(has_feat(feat_of(df, "corriera"),"Tense=Imp"))
  # The model does NOT emit Tense=Pqp: the Pqp branch of the f_01 rule is harmless
  # but practically a dead branch with spanish-gsd.
  expect_false(any(grepl("Tense=Pqp", df$feats, fixed = TRUE), na.rm = TRUE))
})

test_that("LIMITACIÓN: los usos de 'se' son morfológicamente indistinguibles", {
  mp <- get_model_or_skip()
  refl  <- annotate_es("Ella se vio en el espejo.")
  prefl <- annotate_es("Se publicaron los informes.")

  se_refl  <- refl[tolower(refl$token) == "se", , drop = FALSE]
  se_prefl <- prefl[tolower(prefl$token) == "se", , drop = FALSE]

  # Argumental reflexive and reflexive passive: SAME tag (Reflex=Yes, iobj).
  # That is why f_06-f_08 exclude every 'se' with Reflex=Yes (documented): counting the
  # argumental reflexive would require separating the reflexive passive, and there is no signal.
  expect_true(has_feat(se_refl$feats[[1]],  "Reflex=Yes"))
  expect_true(has_feat(se_prefl$feats[[1]], "Reflex=Yes"))
  expect_identical(se_refl$dep_rel[[1]], se_prefl$dep_rel[[1]])
})

test_that("LIMITACIÓN: agente y causa con 'por' son indistinguibles (f_18)", {
  mp <- get_model_or_skip()
  ag <- annotate_es("El informe fue aprobado por el comité.")
  ca <- annotate_es("El informe fue sancionado por incumplimiento.")

  # spanish-gsd does NOT use obl:agent; agent and cause both come out as 'obl'.
  expect_false(any(ag$dep_rel == "obl:agent"))
  expect_true(any(ag$dep_rel == "obl"))
  expect_true(any(ca$dep_rel == "obl"))
})

test_that("relativas: 'que' es SCONJ/mark; 'quien' es PronType=Rel (f_29-f_34)", {
  mp <- get_model_or_skip()
  q  <- annotate_es("Leí el libro que está en la mesa.")
  wh <- annotate_es("La autora, quien presentó el proyecto, respondió.")

  que_row <- q[tolower(q$token) == "que", , drop = FALSE]
  expect_identical(que_row$upos[[1]], "SCONJ")
  expect_identical(que_row$dep_rel[[1]], "mark")
  expect_false(has_feat(que_row$feats[[1]], "PronType=Rel"))
  expect_true(any(q$dep_rel == "acl:relcl"))

  quien_row <- wh[tolower(wh$token) == "quien", , drop = FALSE]
  expect_true(grepl("PronType=Int,Rel|PronType=Rel", quien_row$feats[[1]]))
})

test_that("f_57 vs f_55: subjuntivo vs condicional en la subordinada son separables", {
  mp <- get_model_or_skip()
  sub <- annotate_es("Me pidió que volviera pronto.")
  cnd <- annotate_es("Me dijo que volvería pronto.")

  expect_true(has_feat(feat_of(sub, "volviera"), "Mood=Sub"))
  expect_true(has_feat(feat_of(cnd, "volvería"), "Mood=Cnd"))
})

test_that("f_21/f_22: 'que' SIEMPRE tiene como head el verbo de su propia cláusula, nunca el ADJ", {
  mp <- get_model_or_skip()
  # The real predicate (matrix verb or adjective) lives in the head OF THE VERB
  # marked by 'que' -- the "grandparent" of 'que' -- not in the immediate head of
  # 'que'. This is what tells f_21 (VERB/AUX grandparent) from f_22 (ADJ grandparent).
  adj_df  <- annotate_es("Es importante que vengas pronto.")
  verb_df <- annotate_es("Dijo que vendría mañana.")

  # Adjectival case: 'que' -> head='vengas' (VERB), 'vengas' -> head='importante' (ADJ)
  que_adj <- adj_df[tolower(adj_df$token) == "que", , drop = FALSE]
  expect_identical(que_adj$dep_rel[[1]], "mark")
  v_adj_id <- que_adj$head_token_id[[1]]
  v_adj <- adj_df[adj_df$token_id == v_adj_id, , drop = FALSE]
  expect_identical(v_adj$upos[[1]], "VERB")  # immediate head of 'que': a VERB, not an ADJ
  gp_adj <- adj_df[adj_df$token_id == v_adj$head_token_id[[1]], , drop = FALSE]
  expect_identical(gp_adj$upos[[1]], "ADJ")  # the real ADJ is one level higher

  # Verbal case: 'que' -> head='vendría' (VERB), 'vendría' -> head='Dijo' (VERB)
  que_v <- verb_df[tolower(verb_df$token) == "que", , drop = FALSE]
  v_v_id <- que_v$head_token_id[[1]]
  v_v <- verb_df[verb_df$token_id == v_v_id, , drop = FALSE]
  expect_identical(v_v$upos[[1]], "VERB")
  gp_v <- verb_df[verb_df$token_id == v_v$head_token_id[[1]], , drop = FALSE]
  expect_identical(gp_v$upos[[1]], "VERB")   # here the grandparent is also a VERB
})

# nolint end
