# pseudobibeR.es technical feature reference

Documentation of Biber's (1988) 67 features as adapted to Spanish, after a
linguistic review of the package. It describes the **actual behavior** of the
extractor on the UDPipe `spanish-gsd` model, not only the theoretical intent.

The **Label** column is English; the **Spanish label** column keeps the original
Spanish name. Decision IDs (D-NN) refer to `CROSS_LANGUAGE_ALIGNMENT.md` in the
family repository. For a user-oriented overview, see the README.

## Feature names

- Output columns use **Spanish display names** by default (D-54), defined in
  `data-raw/feature_names.yaml` and exported as the `feature_names` dataset
  (for example `f_15_nominal_infinitives`); the other features keep the shared
  identifier.
- `feature_names = "shared"` in `biber()` or `audit_features()` returns the
  identifiers shared by `pseudobibeR`, `pseudobibeR.es` and `pseudobibeR.fr`
  (for example `f_15_gerunds`). `options(pseudobibeR.feature_names = "shared")`
  changes the default. `audit_features(feature = )` accepts either name.
- The `f_NN` number identifies the **same feature in all three packages**.
  Detectors, evidence, tests and the decision log use the shared identifiers.
- In the tables below, the **Code** column shows the display name, followed by
  the shared identifier in parentheses when the two differ.

## Invariants (non-negotiable)

1. **The default output has 67 feature columns** in the shared order, named with
   the Spanish display names unless `feature_names = "shared"` (D-54). Display
   names keep the shared `f_NN` number. `measure = "none"` omits
   `f_43_type_token`, as in the companion packages.
2. Subtotals (mood/tense/aspect) are computed inside the function and **added up
   before returning**: they are not exposed as new columns.
3. Raw nonmetric counts equal the number of deduplicated evidence occurrences.
   Phrase evidence is positional and sentence-local. Normalized counts use all
   UD word tokens, including function words and spelled-out numerals but
   excluding punctuation, symbols, spaces, digit-final numerals (`42`), and
   multiword-token container rows, as in the English and French packages.
   The output contains `doc_id` and the feature columns only.
4. Parser-sensitive distinctions require independent linguistic validation.
   Known `spanish-gsd` limitations are documented below (marked ⚠️).

---

## A. Tense and aspect

| Code | Label | Spanish label | Actual rule | Notes / limits |
|---|---|---|---|---|
| `f_01_past_tense` | Past tenses of the indicative and subjunctive | Tiempos del pasado de indicativo y subjuntivo | Finite `VERB/AUX` with `Tense ∈ {Past, Imp, Pqp}`, with no mood filter. Covers the simple past (preterite), the imperfect (indicative and subjunctive) and the auxiliary of the pluperfect. | ⚠️ `spanish-gsd` does not emit `Tense=Pqp`; some imperfects in `-ía` (`corría`) are confused with the conditional and lost. Deliberate overlap with f_02 (the auxiliary `había` counts in both). |
| `f_02_perfect_aspect` | Compound tenses (auxiliary *haber* + participle) | Tiempos compuestos (conjugación del verbo auxiliar haber + participio) | One occurrence per auxiliary `haber` governing a participial complement. For flat UD chains, inspect the first following nonfinite auxiliary/copula sharing its head, otherwise the lexical head. | Includes perfect passives (`ha sido revisado`), perfect progressives (`ha estado trabajando`) and perfect copulas (`ha estado enferma`). Excludes progressive `estar` (`está siendo revisado`), modal `haber de` + infinitive (`ha de ser revisado`), and *tener/llevar* + participle. Evidence is anchored on `haber`. |
| `f_03_present_tense` | Present-tense verbs (indicative and subjunctive) | Verbos en presente de indicativo y subjuntivo | Finite `VERB/AUX` with `Tense=Pres`, with no mood filter (parallel to f_01 and English VBZ/VBP). | Includes the present subjunctive (`venga`) and the present auxiliary of compound tenses (`ha` in `ha comido`). |

## B. Place and time adverbials

| Code | Label | Spanish label | Actual rule | Notes / limits |
|---|---|---|---|---|
| `f_04_place_adverbials` | Place adverbs and adverbial phrases | Adverbios y locuciones de lugar | Dictionary and structural detections (single adverbs + multiword phrases: `aquí`, `en mitad de`, `a lo lejos`…). Sentence-local phrases replace covered single-token detections. | Exact positional evidence does not disambiguate spatial from figurative senses or surface homographs. |
| `f_05_time_adverbials` | Time adverbs and adverbial phrases | Adverbios y locuciones de tiempo | Same, with a time inventory (`ayer`, `de vez en cuando`, `hace poco`…). Repeated occurrences count separately; contained matches within a phrase count once. | Surface inventory matches can include nonadverbial uses; this is not sense-disambiguated annotation. |

## C. Pronouns

| Code | Label | Spanish label | Actual rule | Notes / limits |
|---|---|---|---|---|
| `f_06_first_person_pronouns` | First-person pronominal and possessive forms | Formas pronominales y posesivas de primera persona | Pronouns (`yo`, `me`, `nos`…) **+ possessives** with `Poss=Yes` + `Person=1` (`mi`, `nuestro`) and stressed possessives by lemma (`mío`). | — |
| `f_07_second_person_pronouns` | Second-person pronominal and possessive forms | Formas pronominales y posesivas de segunda persona | `tú`, `usted(es)`, `te`, `os`… + possessives with `Person=2` (`tu`, `vuestro`) + recovery of *voseo* (`vos`, mis-tagged as NOUN). | `usted(es)` count here even though they agree in the 3rd person. |
| `f_08_third_person_pronouns` | Third-person pronominal and possessive forms | Formas pronominales y posesivas de tercera persona | `él/ella/ellos`, clitics `lo/la/le`… + possessives with `Person=3` (`su`, including the one for *usted*). | ⚠️ Argumental reflexive `se` is **not counted**: it cannot be told apart from the reflexive passive / impersonal *se* (all `Reflex=Yes`, `iobj`). |
| `f_09_impersonal` (`f_09_pronoun_it`) | Impersonal/expletive counterpart | Predicados impersonales | One occurrence per existing predicate word: weather verbs (`llover`, `nevar`, `granizar`, `tronar`, `relampaguear`), meteorological `hacer` with dependent `frío/calor`, copular ADJ with postposed infinitival or finite que-marked `csubj`, selected verbs (`parecer`, `resultar`, `ocurrir`, `suceder`, `bastar`, `convenir`) with postposed finite que-marked `csubj/ccomp`, and `bastar/convenir` with postposed infinitival `csubj/xcomp`. | Exclude overt nominal subjects and known non-third-singular agreement. Recover only listed root weather forms when mistagged, excluding dependent determiners. No inferred null subjects or empty-node counts. Personal pro-drop, generic complements, associated `se`, temporal verbs, and neuter `lo` are outside this baseline; existential `haber` stays in f_20. Source annotations and existing pronoun features are unchanged. |
| `f_10_demonstrative_pronoun` | Demonstratives used as pronouns | Demostrativos en uso pronominal | `esto/eso/aquello/este/ese` without a noun. | Not to be confused with f_51 (demonstrative determiners). |
| `f_11_indefinite_pronouns` | Indefinites used as pronouns | Indefinidos en uso pronominal | `alguien/nadie/algo/nada/cualquiera…` with an **independent pronominal** function; excludes determiner use (`todo el día`, including `det:*` relations), articles and adjectival use (`libro cualquiera`). It is *code-only*, so that the surface dictionary does not override the control. | ⚠️ The model mis-tags degree adverb `algo` (`está algo cansado`) as `PRON nsubj` → cannot be separated. |
| `f_12_proverb_hacer` (`f_12_proverb_do`) | Anaphoric pro-verb | Pro-verbo anafórico `hacer` | One occurrence per lexical VERB `hacer` with attached direct-object action `lo`, integrated enclitic `lo`, or linked `lo mismo`. Infinitival/progressive/perfect lexical predicates are eligible, not their auxiliaries. | Exclude `aux/cop`, verbal `xcomp/ccomp` causatives, identifiable reflexive `se`, and overt NOUN/PROPN objects (`obj/iobj/dislocated`), except recognized `lo mismo`. Native compound lemma `hacer él` and Ger-tagged split `haciéndo` are supported without retagging. Other clitics/proforms are outside the baseline. Bare `lo hizo` remains structurally eligible: morphology does not resolve action versus distant noun antecedents. Existing pronoun/noun counts and supplied tokenization remain unchanged. |

## D. Questions

| Code | Label | Spanish label | Actual rule | Notes / limits |
|---|---|---|---|---|
| `f_13_wh_question` | Direct partial (wh-) questions | Interrogativas directas parciales | Interrogative word in a sentence with `?`; the interrogative does **not** have to be the first token (`¿Y por qué…?` counts). | — |

## E. Nominal forms

| Code | Label | Spanish label | Actual rule | Notes / limits |
|---|---|---|---|---|
| `f_14_nominalizations` | Derived nominalizations | Nominalizaciones derivadas | NOUN lemmas ending in an inventoried suffix (`-ción`, `-idad`, `-miento`…), excluding `nominalization_stoplist` and actual f_15 occurrences. | A morphological heuristic, not verified derivational history. Remaining stoplisted forms and PROPN go to f_16; nominal infinitives take precedence over suffix matches caused by parser lemma errors. |
| `f_15_nominal_infinitives` (`f_15_gerunds`) | Infinitives functioning as nouns | Infinitivos con función nominal | Infinitive-shaped VERB/AUX/NOUN/PROPN words with Inf or missing verb-form annotation and supported verb identity, either (a) with an attached determiner/possessive or attributive adjective, or (b) heading an `nsubj`/`csubj` placed before its head (`Fumar perjudica`, D-43). Reject internal verbal objects/subjects, auxiliaries/complements and directly attached ADV modifiers. Share nominal cues across eligible infinitive coordination; verbal structure on a coordinand keeps the group verbal. | Noun-tagged forms lacking Inf morphology need a known identity from Spanish verb inventories or `nominal_infinitive_lemmas`; neither endings nor an article alone suffice. Genitive de-modifiers and nominal copular predicates remain eligible. Each word is excluded from f_14/f_16/f_24 and from f_09 clausal-subject licensing. Preserve annotations and tokenization. Postposed infinitival subjects (`Es importante descansar`) stay f_24. Gerunds and participles are not nominal in Spanish and are never counted. Plural lexical nouns, unfamiliar noun-tagged forms and semantic/attachment ambiguities are outside or limited by this baseline. |
| `f_16_other_nouns` | Other nouns | Otros sustantivos | Every `NOUN/PROPN` word token not counted in f_14 or f_15, including hyphenated words. | Noun categories are disjoint. Their sum can include nominal VERB/AUX occurrences recovered in f_15, so it is not simply the number of raw noun POS rows. Multiword-token containers are not words. |

## F. Passives

| Code | Label | Spanish label | Actual rule | Notes / limits |
|---|---|---|---|---|
| `f_17_agentless_passives` | Passives without an agent phrase | Pasivas sin complemento agente | Periphrastic passive (`fue redactado`) **+ reflexive passive** with a nominal subject and third-person finite verb or auxiliary (`se publicaron/se han publicado los informes`), without an agent phrase. SpaCy also permits `ser` + plain `aux` attached to a VERB participle; plain `estar aux` is not sufficient. Explicit `expl:pv`/`expl:impers` are excluded. | ⚠️ A subjectless impersonal (`se entrevistó a los candidatos`, `se recomienda leer`) is excluded. An `nsubj` alone does not establish a patient role: personal reflexive, reciprocal and pronominal verbs can still be false positives when the parser does not distinguish them. |
| `f_18_agent_passives` (`f_18_by_passives`) | Passives with an agent phrase introduced by *por* | Pasivas con complemento agente introducido por *por* | Passive with a same-sentence `por` case marker whose object is attached as `obl`/`obl:*` (or `agent`) to the passive predicate; native spaCy also accepts nominal NOUN/PROPN/PRON `obj` agents. Not a fixed token window. Nominal modifiers and `por` introducing an `advcl` are excluded. | ⚠️ Agent (`por el comité`) and cause (`por incumplimiento`) cannot be told apart reliably: Spanish-GSD uses `obl`, and spaCy can use `obj`. Causal false positives remain possible. |

## G. Stative forms

| Code | Label | Spanish label | Actual rule | Notes / limits |
|---|---|---|---|---|
| `f_19_copula_main_verb` (`f_19_be_main_verb`) | Copular uses of *ser* and *estar* | Usos copulativos de *ser* y *estar* | Nominal, adjectival, prepositional (`es de madera`), adverbial (`está bien`), infinitival (`su objetivo es ganar`) and locative (`está en Lima`) copula. Excludes auxiliary uses (passive/progressive) and *parecer* (f_58). | — |
| `f_20_existential` (`f_20_existential_there`) | Impersonal *haber* in existential constructions | Haber impersonal en construcciones existenciales | Non-auxiliary `haber` in a clausal relation: `hay/había/hubo/habrá/haya…`, perfect `ha habido` and infinitival `puede haber`. Forms are compared without accents. Excludes auxiliary `haber` and `haber que` (→ f_53). | — |

## H. Subordination

| Code | Label | Spanish label | Actual rule | Notes / limits |
|---|---|---|---|---|
| `f_21_que_verb_comp` (`f_21_that_verb_comp`) | *que* complement clauses depending on a verb | Completivas con *que* dependientes de un verbo | Complementizer *que* whose clause is `ccomp/xcomp/csubj` (including subtypes) of a VERB/AUX, including prepositional complements (`insistió en que…`). The clause may be headed by any predicate, including copular ones (`Creo que es una buena idea`). The governing predicate is the "grandparent" of *que* (see f_22). | Adverbial purpose/conditional clauses and relative clauses are not verb complements. Components of f_37 conditional phrases and the *que* of *lo que* free relatives (f_23) are excluded. |
| `f_22_que_adj_comp` (`f_22_that_adj_comp`) | *que* complement clauses depending on an adjectival predicate | Completivas con *que* dependientes de un predicado adjetival | `que` (SCONJ, mark) always has as its head the verb of its own clause (V); the real adjective is the head OF THAT VERB (the "grandparent" of *que*, via `csubj`/`advcl`). The rule checks that this grandparent is an ADJ: `es probable que llueva`, `estoy seguro de que ganaremos`. | Fixed (before: 0% recall, it always returned 0 — the rule checked the immediate head of *que*, which is never an ADJ). Verified on 8 different constructions. |
| `f_23_wh_clause` | Indirect questions and verb-complement free relatives | Interrogativas indirectas y relativas libres completivas | **Accented** interrogative word in a subordinate clause, plus *lo que* free relatives that are a verb's argument (`Sé lo que hiciste`, `Me gusta lo que haces`), counted once as the span *lo que*. This follows Biber's definition (*I believed what he told me*) and the French *ce que/ce qui* counterpart. | Excludes sentence relatives after punctuation (f_34), free relatives governed by a copula or preposition (`Lo que dices es verdad`, `en lo que…`), and other free relatives (`donde me indiques`). Spanish-GSD analyses *que* as a complementizer of a `ccomp` clause; that *que* is not also counted in f_21. |
| `f_24_infinitives` | Verbal infinitives and infinitive clauses | Infinitivos verbales y oraciones de infinitivo | VERB/AUX with `VerbForm=Inf`, excluding actual nominal f_15 occurrences: bare roots, auxiliary infinitives, xcomp, periphrases, complements, purpose and subject clauses remain eligible. | ⚠️ Source NOUN/PROPN infinitives without nominal f_15 evidence may still be missed. A finite verb mis-tagged Inf remains a possible false positive; roots are not suppressed to hide model errors. |
| `f_25_gerund_clause` (`f_25_present_participle`) | Gerund in adverbial function | Gerundio en función adverbial | Gerund `advcl/ccomp`; excludes predicative (`lo vi entrando`) and periphrastic (`está trabajando`) gerunds. | — |
| `f_26_past_participle` | Participle in absolute or adverbial construction | Participio en construcción absoluta o adverbial | Participle `advcl/ccomp/acl`, without `Voice=Pass`, excluding postnominal reduced relatives counted in f_27 (`terminada la reunión…`). | ⚠️ Detection varies with the dependency parse and the existing morphological voice restriction. |
| `f_27_past_participle_reduced_relative` (`f_27_past_participle_whiz`) | Postnominal participles as reduced relatives | Participios postnominales con valor de relativa reducida | Participle `acl` with a NOUN head (`el artículo publicado ayer`). | — |
| `f_28_gerund_reduced_relative` (`f_28_present_participle_whiz`) | *(0)* Postnominal gerund | Gerundio postnominal | Always 0. | English f_28 is a productive, grammatical reduced relative (`the man standing there`). Standard Spanish has no such mechanism: the specifying postnominal gerund (`una caja conteniendo libros`) is normatively incorrect (RAE/ASALE, NGLE) and mostly a calque. Accepted postnominal uses are not reduced relatives: lexicalized `hirviendo`/`ardiendo`, captions (`Colón llegando a América`) and predicative gerunds with perception/representation verbs (`una foto de niños jugando`). The relative function is expressed by full relatives (f_29–f_32); remapping it would double count. French f_28 differs because its adjectival present participle is grammatical. |
| `f_29_que_subj` (`f_29_that_subj`) | *que* subject relatives | Relativas de *que* en funcion de sujeto | NOUN/PROPN-headed relative: explicit nsubj que without a bounded conflict, or roleless SCONJ/mark que with finite intransitive support (`relative_subject_intransitives`) or locative estar with an en-marked oblique and no other subject. f_33/f_34 take precedence. | An article before que stays in this family; no automatic subject assignment from absence of another nsubj. Other predicate valencies and unresolved pro-drop gaps can be missed. |
| `f_30_que_obj` (`f_30_that_obj`) | *que* object/complement relatives | Relativas de *que* en funcion de objeto/complemento | Nominal relative que with obj/iobj/obl/nmod roles (including subtypes), a bounded conflicting nsubj assignment, or SCONJ/mark plus another subject. f_33/f_34 take precedence. | Source annotation errors remain possible. Coordinated subjects block conflict/agreement repairs. |
| `f_31_quien_cual_subj` (`f_31_wh_subj`) | *quien/cual* subject relatives | Relativas de *quien/cual* en funcion de sujeto | quien/quienes/cual/cuales with a subject role in a NOUN/PROPN-headed relative, excluding conflicting-subject or bounded agreement cases routed to f_32. | Marker family and role, not commas or a restrictive/nonrestrictive distinction. Shared recognition accepts acl subtypes and narrowly recovers finite appos predicates with an explicit PRON relative carrying Rel morphology. Free relatives/questions and f_33/f_34 occurrences are excluded. |
| `f_32_quien_cual_obj` (`f_32_wh_obj`) | *quien/cual* object/complement relatives | Relativas de *quien/cual* en funcion de objeto/complemento | The same marker family with object/complement roles, or nsubj contradicted by another uncoordinated subject or singular-relative/singular-antecedent agreement against a first/second-person plural finite predicate. | First/second person alone is not an object diagnostic; coordinated subjects are protected. Prepositional relatives go exclusively to f_33, unlike overlapping French dont counts. No general retagging or exhaustive ambiguity resolution. |
| `f_33_pied_piping` | Relatives with a preposition before the relative pronoun | Relativas con preposición antepuesta al relativo | ADP + (DET) + relative pronoun (`por el cual`, `con quien`, `a quien`) in a nominal `acl`/`acl:*` clause in the same sentence. | Prepositional complements and indirect questions do not count here. *a quien* counts here because pied-piping takes precedence over f_29/f_30. |
| `f_34_sentence_relatives` | Relatives with a sentential antecedent | Relativas con antecedente oracional | `lo que`/`lo cual` with a propositional antecedent (`…, lo cual molestó…`). | Free relatives without a propositional antecedent do not count here. |
| `f_35_causal` (`f_35_because`) | Causal clauses introduced by *porque* | Cláusulas causales introducidas por *porque* | *porque*. | — |
| `f_36_concessive` (`f_36_though`) | Concessive clauses introduced by *aunque* | Cláusulas concesivas introducidas por *aunque* | *aunque*. | — |
| `f_37_conditional` (`f_37_if`) | Conditional clauses (*si*, *a menos que*, *salvo que*) | Cláusulas condicionales (*si*, *a menos que*, *salvo que*) | Conditional *si*, excluding indirect-question `ccomp/xcomp` under a listed communication/cognition verb. An `advcl` is not excluded merely because its main verb is `decir/saber`. Full `a menos que` / `salvo que` sequences count once per marked, sentence-local phrase. | Phrase evidence spans the full expression; all components are excluded from f_21/f_22/f_38. The main-clause conditional tense (f_54) is not required. ⚠️ `spanish-gsd` misparses `Preguntó si vendría mañana` as `advcl` (false positive), but correctly uses `ccomp` with `a la fiesta`. |
| `f_38_other_adv_sub` | Other adverbial subordinators | Otros subordinadores adverbiales | `cuando`, `mientras`, `según`, `para que`… (SCONJ/ADP/ADV mark not counted in f_21/f_22/f_35/f_36/f_37). | Excludes the components of `a menos que`/`salvo que`. |

## I. Prepositional phrases, adjectives and adverbs

| Code | Label | Spanish label | Actual rule | Notes / limits |
|---|---|---|---|---|
| `f_39_prepositions` | Prepositions | Preposiciones | Every ADP; phrases in `word_lists$prepositions_compound` replace their covered ADP occurrences with **one** full-span occurrence (`a causa de`, `a excepción de`…). Former detector-local entries are preserved in that shared inventory. | Matching is sentence-local. Expanded UD contraction components are used, not multiword-token containers. A locution may start on a non-ADP token; audit evidence spans the entire phrase. |
| `f_40_adj_attr` | Adjectives modifying a noun | Adjetivos en función de modificador nominal | ADJ `amod` and adjective conjuncts inheriting that role, including hyphenated forms. The existing fallback (ADJ before NOUN/PROPN/ADJ when the document has no `amod` adjective) excludes recognized predicates. | Dependency-based classification takes precedence over fallback. The fallback remains heuristic and document-local. |
| `f_41_adj_pred` | Predicative adjectives: subject complements and predicative complements | Adjetivos en función predicativa: atributos y complementos predicativos | ADJ `xcomp/acomp`, an adjective heading a `cop` dependent, or legacy `attr` attached to a listed linking verb, plus adjective conjuncts inheriting a predicative role. | Subjects or other noncomplements of a linking verb do not count merely because of that head lemma. Coordination propagation is sentence-local; the `cop`-headed branch retains the review-v2 extension to noncanonical copulas. |
| `f_42_adverbs` | Total adverbs | Total de adverbios | `ADV` occurrences excluding those counted as place/time adverbials, downtoners, hedges, or amplifiers (f_04, f_05, f_46, f_47, f_48). Use actual detections and all ADV members of counted phrases; overlapping exclusions remove a word once. | Conjuncts, emphatics, discourse particles, and negation are not additional exclusions. ADV `no` remains eligible; PART mapping is unresolved. |

## J. Lexical specificity

| Code | Label | Spanish label | Actual rule | Notes / limits |
|---|---|---|---|---|
| `f_43_type_token` | Lexical diversity | Diversidad léxica | Lower-case surface-plus-tag types, not lemmas. Retain fine-grained tags, falling back to UPOS when missing or `_`. Exclude PUNCT/SYM/SPACE and English-style numeric matches (`\d_`) before calculation. Spelled-out numerals remain words; dictionary expressions are not compounded. **MATTR** by default; any document below 200 retained tokens switches the entire corpus to TTR. Explicit alternatives are retained. | One measure per corpus. The filtered threshold population is the same word-token set as the normalization denominator. A shared `textstat_lexdiv()` call uses library defaults; MSTTR may warn when shortening its common segment. Filtered-empty documents receive `NA` and an identifying warning. Parser tag granularity affects type distinctions. |
| `f_44_mean_word_length` | Mean word length | Longitud media de palabras | Mean number of letters of purely alphabetic word tokens (any Unicode letter), unrounded (D-51). | Numerals (`2024`), hyphenated and apostrophe forms are excluded, as in English and French. `NA` for a document without alphabetic words. |

## K. Lexical classes

These are surface inventories, not sense-disambiguated annotations. Matching is
case-insensitive and sentence-local; a full phrase suppresses its contained
matches within the same feature. Entries may overlap across features, which are
not mutually exclusive. Exact span evidence does not establish that an ambiguous
word has the intended discourse, degree, or epistemic function.

| Code | Label | Spanish label | Actual rule | Notes / limits |
|---|---|---|---|---|
| `f_45_conjuncts` | Text connectors | Conectores textuales | `sin embargo`, `por tanto`, `no obstante`… (dictionary). | — |
| `f_46_downtoners` | Degree and approximating downtoners | Atenuadores de grado y aproximativos | `apenas`, `ligeramente`, `un poco`… (`casi` is **no longer** here). | — |
| `f_47_hedges` | Epistemic hedges expressing approximation, vagueness or reservation | Modalizadores epistémicos que corresponden a expresiones de aproximación, imprecisión o reserva | `quizás`, `tal vez`, `probablemente`, `más o menos`, `una especie de`, **`casi`** (for consistency with *almost*). | — |
| `f_48_amplifiers` | Degree intensifiers | Intensificadores de grado | `muy`, `totalmente`, `absolutamente`… | — |
| `f_49_emphatics` | Emphatic or assertion-reinforcing expressions | Expresiones de énfasis o refuerzo asertivo | `de hecho`, `sin duda`, `realmente`… | — |
| `f_50_discourse_particles` | Conversational discourse particles | Partículas discursivas conversacionales | Surface dictionary (`bueno`, `pues`, `claro`, `mira`…). | ⚠️ **No reliable positional filter:** `spanish-gsd` does not mark `discourse` and mis-tags the ambiguous forms (`Bueno`→ADJ, `Mira`→PROPN, `Claro`→PROPN). It counts some false positives (allowed by the base specification). |
| `f_51_demonstratives` | Demonstrative determiners before a noun | Determinantes demostrativos antepuestos a un sustantivo | `este/ese/aquel` + noun. | Not to be confused with f_10 (pronominal use). |

## L. Modals (periphrases)

| Code | Label | Spanish label | Actual rule | Notes / limits |
|---|---|---|---|---|
| `f_52_modal_possibility` | Modal uses of *poder* + infinitive | Usos modales de *poder* + infinitivo | Configured `poder/caber` + infinitive, including passive/progressive/copular chains; `puede que` with fixed `que` or `ccomp` + `que mark` with subjunctive morphology on the predicate or its auxiliary/copula. One occurrence anchored on the modal. | Requires correct lemmas: Spanish-GSD can lemmatize `podría` as `podrer`, which is not patched with surface-form exceptions. |
| `f_53_modal_necessity` | Modal periphrases of necessity, obligation or advisability | Perífrasis modales de necesidad, obligación o conveniencia | `deber` + inf, `tener que`, `haber que`, `haber de` + inf; includes `deber de` (probability). Supports flat/nested auxiliary chains. `tener/haber` require a linked `que/de` marker on the modal, its immediate infinitive, or the shared clause head. | An unrelated purpose infinitive is not a modal complement. Bad POS/dependency annotations can still hide genuine periphrases. |
| `f_54_modal_predictive` | Future, conditional and *ir a* + infinitive | Futuro, condicional e *ir a* + infinitivo | Finite synthetic future, conditional (`Mood=Cnd`) and finite `ir a` + inf with `Tense=Pres/Imp`, including passive chains. Requires linked `a`, with `ir` either auxiliary or head of the complement. | Excludes past motion (`Tense=Past`); tense/dependencies do not fully disambiguate present-tense motion from futurity. |

## M. Specialized verbs

| Code | Label | Spanish label | Actual rule | Notes / limits |
|---|---|---|---|---|
| `f_55_verb_public` | Speech-act or public communication verbs | Verbos de lengua o de comunicación pública | Communication lemmas (`afirmar`, `señalar`, `sostener`, `decir`…). Fix (review v2): `decir` was added to the dictionary (`dict$f_55_verb_public`), after confirming empirically that it was missing (*"El testigo dijo que todo era cierto."* gave 0). | ⚠️ Disambiguation by mood (`decir`+subj→f_57 / `decir`+cnd→f_55) was not implemented: the model mis-tags polysemous verbs (`sostuvo`→ADJ). |
| `f_56_verb_private` | Verbs of thinking | Verbos de pensamiento | `creer`, `pensar`, `saber`, `suponer`… Also `dar` with an attached `Reflex=Yes` pronoun and nominal `cuenta` child (`obj/compound`, including subtypes), anchored on the verb. Excludes individual `ver` occurrences recognized as f_58. | ⚠️ `esperar` always counts ('wait' is not separated from 'expect'). Lexical classes are not general sense disambiguation. Pronominal constructions require reflexive morphology/dependencies. spaCy lemmas containing an enclitic (`verlo` → `ver él`) do not match bare verb inventories automatically. |
| `f_57_verb_suasive` | Suasive verbs | Verbos suasivos | `pedir`, `recomendar`, `sugerir`, `proponer`… | See the f_55 limit (disambiguation by mood not implemented). ⚠️ **Confirmed lemmatization limit:** UDPipe lemmatizes the diphthongized forms of `recomendar` irregularly and unpredictably (`recomienda`→`recomienda`, `recomiendan`→`recomiendar`, `recomiendo`→`recomir`), so *"El jefe recomienda que trabajen más."* gives 0 even though `recomendar` is in the dictionary. It was not patched with a list of surface forms because it is too unpredictable case by case; documented as a model limitation. |
| `f_58_verb_seem` | Verbs of appearance or inference | Verbos de apariencia o inferencia | Configured `parecer` and `resultar` **only in copular use** (`resulta adecuada`); excludes `resultó ganador` and `resultó de`. Configured `verse` is also recovered from `ver` + reflexive pronoun + adjectival predication (`cop` to ADJ or ADJ `xcomp/acomp/attr` child). The pronoun may attach to `ver` or its copular predicate. | Anchored on the verb, not a contiguous phrase. No general separation of appearance from reflexive perception; nominal objects alone do not license `verse`. |

## N. Reduced forms

| Code | Label | Spanish label | Actual rule | Notes / limits |
|---|---|---|---|---|
| `f_59_reduced_forms` (`f_59_contractions`) | Colloquial contractions and written reductions | Contracciones coloquiales y reducciones escritas | Longest recognized surface expression from `colloquial_contractions`, including pa forms, onde/nde/onde's/ontas and ta/tas/toy/tábamos. Straight/curly apostrophes and native splits count once with full-span evidence. | Exclude ordinary al/del, generic apostrophes/names, directly determined nominal uses and short uppercase nominal/acronym forms. No verb-only POS gate, lemma repair, word expansion or automatic paradigm/vowel generation. Original denominator tags remain; unlisted spellings/homographs are not exhaustively resolved. |
| `f_60_reduced_complement` (`f_60_that_deletion`) | Bounded complement-clause reduction | Reducción de cláusulas completivas | Infinitival or unmarked finite ccomp/xcomp complements under creer/pensar/esperar (private) and decir/afirmar/asegurar (public, D-46); clear coordination inherits governor and introducer context. Auxiliary/copular Fin/Inf forms license participial/nonverbal heads. One occurrence per supplied clause head. | Functional analogue, not literal deleted que: Pienso ir, Creo haber entendido, El testigo afirma haber visto al ladrón and Nosotros esperamos lleguen mañana qualify; decir de + infinitive (suasive) does not. pretender is excluded (false friend: "intend"). Exclude overt introducers, accented interrogative introducers, nominal infinitives, unrelated clauses and other governors. f_24 overlap is intentional. No reconstruction, retagging, subject-coreference inference or assertion of equivalent meaning/register. Parser loss of governor/attachment causes misses, including isolated Espero estén bien in both models; spaCy also misses some other finite complements. |
| `f_61_resumptive_relative` (`f_61_stranded_preposition`) | Resumptive prepositional relatives | Relativas preposicionales reasuntivas | Initial hablar-de/depender-de/confiar-en frames; nominal que relative, finite predicate or auxiliary projection, first/second-person or separate-subject evidence and postverbal linked tonic él/ella/ellos/ellas PP. One relative clause counts once with its first matching PP span. | Not literal stranding or unrestricted coreference. Exclude ordinary subject/object relatives, coordinated relative subjects, bare objects and known gender/number conflicts. Detected que markers leave f_29/f_30; f_33 and other relative bins stay unchanged. Exact hablé/confié lookup recovers known errors locally without retagging; other frames, natural-gender conflicts and parser errors (including hablaste) remain limited. French revision is a separate follow-up. |
| `f_62_split_infinitive` | Adverb-interrupted preposition–infinitive unit | Unidad preposición–infinitivo interrumpida por adverbios | Linked introductory ADP, consecutive adverbial interveners and verbal Inf; at least one non-negative adverb required. Full matched span counts once. Direct/adverb-mediated/fixed marker links and compound/copular clause heads are supported. | Trato de realmente comprender qualifies; para no volver and para nunca volver do not, but para no realmente volver does. Exclude subjects, punctuation, nominal infinitives, unlinked prepositions and post-infinitive adverbs. No arbitrary three-token cap or source retagging. Bounded mejor/siquiera lookup handles native ADJ/NOUN errors; spaCy can lose marker attachment in para mejor entender/de muy lentamente comprender. f_24/f_63 are independent; this is not literal word splitting or a register claim. Negation-only exclusion intentionally differs from companion heuristics. |
| `f_63_split_auxiliary` | Adverbs between the auxiliary and its complement | Adverbios interpuestos entre el verbo auxiliar y el verbo auxiliado | ADV or negative PART between the auxiliary/modal and its immediate nonfinite complement, at least one of them other than the analytic negator no (D-49: `puede no venir` does not count; `puede no realmente entenderlo` does), including validated modal periphrases (`puede fácilmente resolver`). One occurrence per split auxiliary/modal, anchored there; a long chain can contain multiple separate splits. | Excludes lexical complements (`quiero realmente estudiar`) and intervening subjects. Uses linear position, not adverb scope disambiguation. Incorrect modal lemmas also affect this feature. |

## O. Coordination

| Code | Label | Spanish label | Actual rule | Notes / limits |
|---|---|---|---|---|
| `f_64_phrasal_coordination` | Coordination of words or non-clausal phrases | Coordinación de palabras o grupos no oracionales | Coordination of nouns (including mixed NOUN/PROPN), adjectives, adverbs **and verbal/copular predicates** with a shared subject (`lee y escribe`, `canta y está contenta`). | `!has_subject` separates it from f_65. The Spanish-GSD fallback for ADJ `conj` attached to a noun with an `amod` adjective is retained. |
| `f_65_clausal_coordination` | Coordination of independent clauses with coordinating conjunctions | Coordinación de cláusulas independientes mediante conjunciones coordinantes | CCONJ between predicates with their own `nsubj/csubj`, including heads of finite copulas; or sentence-initial `y/e` attached to a verbal/copular predicate. Initial means first UD word, ignoring opening punctuation/symbols/spaces, not token ID 1; nominal fragments are excluded. | Verbal predicates explicitly marked nonfinite are excluded; missing verb-form tags retain the previous verbal fallback. Copular heads require a finite copula. Pro-drop and bad attachments still limit phrase/clause distinctions. |

## P. Negation

| Code | Label | Spanish label | Actual rule | Notes / limits |
|---|---|---|---|---|
| `f_66_neg_synthetic` | Negation with negative lexical words | Negación mediante palabras léxicamente negativas | Approved inventory: `ninguno` and forms, `nadie/nada/nunca/jamás/tampoco/ni`, normally PRON/ADV/DET/CCONJ. Count each word, including concord (`No vino nadie`: f_66 = 1, f_67 = 1; `ni...ni`: f_66 = 2). Exclude candidates with a preceding dependent article (`la nada`, `un nadie`). Recover PROPN `nadie` only as an `nsubj`/subtype of a real VERB/AUX head, preserving original annotations. | Idioms and additional classes such as `sin` are deferred. Recovery is feature-local, not global parser retagging; other POS-sensitive counts may retain model errors. Verified with Spanish-GSD and `es_core_news_sm`. |
| `f_67_neg_analytic` | Negation with the adverb *no* | Negación mediante el adverbio *no* | ADV/PART `no` with `advmod` or `neg` and a real nonself word head, including nominal, pronominal, numeral, function-word, verbal, adjectival, and adverbial scope. Count the marker, preserving nouns. Exclude `no obstante`, identifiable replies/nominalizations, and quoted standalone `no`; retain `la no violencia`, `no fumador`, parenthetical negation, and quoted negative clauses. | Contextual guards are not general sense disambiguation: expletive `no`, non-negative `no más`, and ambiguous replies/nominal readings remain limitations. This rule and f_42 mapping are unchanged by the f_66 repair. Verified with Spanish-GSD and `es_core_news_sm`. |

---

## Known limitations (summary)

Situations that reflect parser/model limits, not errors of the extractor.
They are not forced (project methodology):

- **spaCy reference integration**: `es_core_news_sm` 3.8.0 with spaCy 3.8.7
  is tested through spacyr using explicitly requested `morph` attributes.
  `ROOT` is normalized to `root`; native passive conventions are handled
  without rewriting unrelated `aux/obj` dependencies. These integration
  examples do not establish corpus-level precision/recall.
- **spaCy POS and attachment errors**: `Puede siempre revisar el informe`
  attaches `puede` to the ADV `siempre`, hiding both modality and the split;
  `Ella canta y él baila` makes the pronouns heads, hiding clause coordination.
  In `Preguntó dónde vive Ana`, `dónde` is PRON with `PronType=Ind`, hiding
  the indirect question. Frozen regressions preserve these errors, with
  corrected-annotation contrasts where appropriate.
- **Parser tokenization**: spaCy retains some contractions and enclitics as
  single tokens, while UDPipe supplies expanded UD component words. Counts
  and normalization denominators can therefore differ for identical text.
  Missing decompositions and multiword lemmas are not guessed.

- **Argumental `se` vs. reflexive passive/impersonal** (f_08, f_17): morphologically
  identical (`Reflex=Yes`, `iobj`). f_08 excludes every `se`; f_17 uses the presence
  of a patient subject (`nsubj`) to separate the reflexive passive (counts) from the
  impersonal (does not count).
- **Agent vs. cause with *por*** (f_18): the model does not emit `obl:agent`; both are
  `obl`. f_18 may include causal false positives. A number-agreement filter (passive
  subject vs. agent phrase) proposed during the review was tested and **rejected**: it
  fails on legitimate, common cases — *"los proyectos fueron aprobados por el comité"*
  (singular collective agent, plural subject) and *"el informe fue aprobado por los
  ingenieros"* (plural agent, singular subject) would be wrongly excluded — and it does
  not catch the case it was meant to filter (*"el accidente fue causado por la
  lluvia"*, singular-singular, still gets through).
- **Relative roles and attachments** (f_29-f_32): both models tag cual and Maria
  nsubj in `El informe, el cual María revisó...`; the separate-subject conflict
  check repairs this case. GSD's singular cual with first-person plural
  revisamos is handled by the bounded agreement check; spaCy's finite appos
  attachment can be recovered with relative morphology. Other role conflicts
  and attachment errors remain. GSD's `La novela que leí era larga` tags leí
  ADV and attaches roleless que to larga; this is now unclassified rather than
  guessed to be a subject. Positive intransitive/locative support is bounded,
  not an exhaustive valency lexicon.
- **Free relatives** (f_23): only *lo que* clauses that are a verb's argument are
  counted; other free relatives (`donde me indiques`) are not.
- **Imperfects in `-ía`** (f_01): sometimes mis-tagged as `Mood=Cnd`.
- **Polysemous verbs** (f_55/f_57): `sostuvo` is mis-tagged as ADJ; disambiguating
  `decir` by mood was not implemented. `recomendar` (f_57) has a confirmed
  lemmatization limit in its diphthongized forms (see f_57).
- **Ambiguous discourse particles** (f_50): no `discourse` signal; the positional
  filter is not reliable (`Bueno`→ADJ, `Mira`→PROPN).
- **Encoding**: the accented phrases of f_39 need a UTF-8 locale; source-code literals
  use `\uXXXX` escapes instead of accented characters to avoid corruption when the
  package is loaded under non-UTF-8 locales (e.g. `LC_CTYPE=C`).

## Column always at zero (1)

`f_28` — a linguistic compatibility zero, not a missing detector: standard
Spanish has no productive postnominal gerund functioning as a reduced relative
(see section H).
