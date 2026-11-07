# pseudobibeR.es feature table (Spanish features)

Documentation of Biber's (1988) 67 features as adapted to Spanish, after a
linguistic review of the package. It describes the **actual behavior** of the
extractor on the UDPipe `spanish-gsd` model, not only the theoretical intent.

The **Label** column is English; the **Spanish label** column keeps the original
Spanish name. The historical highlighter is archived outside the package.

## Invariants (non-negotiable)

1. **The default output has 67 feature columns**, with **names identical** to the
   English `pseudobibeR` (`f_31_wh_subj`/`f_32_wh_obj` even though they count
   *quien*/*el cual*).
   The renamings in this table affect **only the label and description**, never the
   column identifier or its order. `measure = "none"` omits `f_43_type_token`,
   as in the companion packages.
2. Subtotals (mood/tense/aspect) are computed inside the function and **added up
   before returning**: they are not exposed as new columns.
3. Raw nonmetric counts equal the number of deduplicated evidence occurrences.
   Phrase evidence is positional and sentence-local. Normalized counts use all
   UD word tokens, including function words but excluding punctuation, symbols,
   spaces, and multiword-token container rows.
4. Parser-sensitive distinctions require independent linguistic validation.
   Known `spanish-gsd` limitations are documented below (marked ⚠️).

---

## A. Tense and aspect

| Code | Label | Spanish label | Actual rule | Notes / limits |
|---|---|---|---|---|
| `f_01_past_tense` | Past tenses of the indicative and subjunctive | Tiempos del pasado de indicativo y subjuntivo | Finite `VERB/AUX` with `Tense ∈ {Past, Imp, Pqp}`, with no mood filter. Covers the simple past (preterite), the imperfect (indicative and subjunctive) and the auxiliary of the pluperfect. | ⚠️ `spanish-gsd` does not emit `Tense=Pqp`; some imperfects in `-ía` (`corría`) are confused with the conditional and lost. Deliberate overlap with f_02 (the auxiliary `había` counts in both). |
| `f_02_perfect_aspect` | Compound tenses (auxiliary *haber* + participle) | Tiempos compuestos (conjugación del verbo auxiliar haber + participio) | One occurrence per auxiliary `haber` governing a participial complement. For flat UD chains, inspect the first following nonfinite auxiliary/copula sharing its head, otherwise the lexical head. | Includes perfect passives (`ha sido revisado`), perfect progressives (`ha estado trabajando`) and perfect copulas (`ha estado enferma`). Excludes progressive `estar` (`está siendo revisado`), modal `haber de` + infinitive (`ha de ser revisado`), and *tener/llevar* + participle. Evidence is anchored on `haber`. |
| `f_03_present_tense` | Present indicative verbs | Verbos en presente de indicativo | `VERB/AUX`, `Tense=Pres, Mood=Ind, VerbForm=Fin`. | Excludes the present subjunctive. |

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
| `f_09_pronoun_it` | *(0)* Expletive pronoun *it* | Pronombre expletivo *it* | Always 0. | No Spanish equivalent (null subject). The functions of *it* are spread across null subjects, `lo/ello`, `esto/eso`, `su` and `se` — there is no single form that can be counted without overlapping f_08/f_10. |
| `f_10_demonstrative_pronoun` | Demonstratives used as pronouns | Demostrativos en uso pronominal | `esto/eso/aquello/este/ese` without a noun. | Not to be confused with f_51 (demonstrative determiners). |
| `f_11_indefinite_pronouns` | Indefinites used as pronouns | Indefinidos en uso pronominal | `alguien/nadie/algo/nada/cualquiera…` with an **independent pronominal** function; excludes determiner use (`todo el día`, including `det:*` relations), articles and adjectival use (`libro cualquiera`). It is *code-only*, so that the surface dictionary does not override the control. | ⚠️ The model mis-tags degree adverb `algo` (`está algo cansado`) as `PRON nsubj` → cannot be separated. |
| `f_12_proverb_do` | *(0)* Pro-verb *do* | Pro-verbo *do* | Always 0. | Spanish resolves verbal anaphora by ellipsis; there is no equivalent pro-verb that can be counted unambiguously. |

## D. Questions

| Code | Label | Spanish label | Actual rule | Notes / limits |
|---|---|---|---|---|
| `f_13_wh_question` | Direct partial (wh-) questions | Interrogativas directas parciales | Interrogative word in a sentence with `?`; the interrogative does **not** have to be the first token (`¿Y por qué…?` counts). | — |

## E. Nominal forms

| Code | Label | Spanish label | Actual rule | Notes / limits |
|---|---|---|---|---|
| `f_14_nominalizations` | Derived nominalizations | Nominalizaciones derivadas | NOUN lemmas ending in an inventoried suffix (`-ción`, `-idad`, `-miento`…), excluding `nominalization_stoplist`. | A morphological heuristic, not verified derivational history. Stoplisted lexicalized forms and all PROPN go to f_16. |
| `f_15_gerunds` | *(0)* Infinitives in nominal function | Infinitivos en función nominal | Always 0 (reverted to the original design by the author's decision). | Technical label: the name reflects the function this feature would cover in English (nominalization of a non-finite verb form), not that something is being detected. The Spanish gerund (*-ando/-iendo*) cannot have a nominal function; in Spanish the nominal function of a non-finite verb form is covered by the infinitive (f_24) or by a derived nominalization (f_14). An attempt to activate it through the subject infinitive (`csubj`) was tested during the review and reverted. |
| `f_16_other_nouns` | Other nouns | Otros sustantivos | Every `NOUN/PROPN` word token not counted in f_14, including hyphenated words. | Raw f_14 + f_16 equals the number of UD NOUN/PROPN word rows; multiword-token container rows are not words. |

## F. Passives

| Code | Label | Spanish label | Actual rule | Notes / limits |
|---|---|---|---|---|
| `f_17_agentless_passives` | Passives without an agent phrase | Pasivas sin complemento agente | Periphrastic passive (`fue redactado`) **+ reflexive passive** with a nominal subject and third-person finite verb or auxiliary (`se publicaron/se han publicado los informes`), without an agent phrase. SpaCy also permits `ser` + plain `aux` attached to a VERB participle; plain `estar aux` is not sufficient. Explicit `expl:pv`/`expl:impers` are excluded. | ⚠️ A subjectless impersonal (`se entrevistó a los candidatos`, `se recomienda leer`) is excluded. An `nsubj` alone does not establish a patient role: personal reflexive, reciprocal and pronominal verbs can still be false positives when the parser does not distinguish them. |
| `f_18_by_passives` | Passives with an agent phrase introduced by *por* | Pasivas con complemento agente introducido por *por* | Passive with a same-sentence `por` case marker whose object is attached as `obl`/`obl:*` (or `agent`) to the passive predicate; native spaCy also accepts nominal NOUN/PROPN/PRON `obj` agents. Not a fixed token window. Nominal modifiers and `por` introducing an `advcl` are excluded. | ⚠️ Agent (`por el comité`) and cause (`por incumplimiento`) cannot be told apart reliably: Spanish-GSD uses `obl`, and spaCy can use `obj`. Causal false positives remain possible. |

## G. Stative forms

| Code | Label | Spanish label | Actual rule | Notes / limits |
|---|---|---|---|---|
| `f_19_be_main_verb` | Copular uses of *ser* and *estar* | Usos copulativos de *ser* y *estar* | Nominal, adjectival, prepositional (`es de madera`), adverbial (`está bien`), infinitival (`su objetivo es ganar`) and locative (`está en Lima`) copula. Excludes auxiliary uses (passive/progressive) and *parecer* (f_58). | — |
| `f_20_existential_there` | Impersonal *haber* in existential constructions | Haber impersonal en construcciones existenciales | `hay/había/hubo/habrá/haya…`. Excludes auxiliary `haber` and `haber que` (→ f_53). | — |

## H. Subordination

| Code | Label | Spanish label | Actual rule | Notes / limits |
|---|---|---|---|---|
| `f_21_that_verb_comp` | *que* complement clauses depending on a verb | Completivas con *que* dependientes de un verbo | Complementizer *que* whose clause is `ccomp/xcomp/csubj` (including subtypes) of a VERB/AUX, including prepositional complements (`insistió en que…`). The governing predicate is the "grandparent" of *que* (see f_22). | Adverbial purpose/conditional clauses and relative clauses are not verb complements. Components of f_37 conditional phrases are excluded. |
| `f_22_that_adj_comp` | *que* complement clauses depending on an adjectival predicate | Completivas con *que* dependientes de un predicado adjetival | `que` (SCONJ, mark) always has as its head the verb of its own clause (V); the real adjective is the head OF THAT VERB (the "grandparent" of *que*, via `csubj`/`advcl`). The rule checks that this grandparent is an ADJ: `es probable que llueva`, `estoy seguro de que ganaremos`. | Fixed (before: 0% recall, it always returned 0 — the rule checked the immediate head of *que*, which is never an ADJ). Verified on 8 different constructions. |
| `f_23_wh_clause` | Indirect questions | Interrogativas indirectas | **Accented** interrogative word in a subordinate clause. | ⚠️ **Free relatives** (`lo que dijo`, `donde me indiques`) are NOT included: separating them from f_29–f_34 without coreference is high-risk (deferred). |
| `f_24_infinitives` | Infinitives and infinitive clauses | Infinitivos y oraciones de infinitivo | Every VERB/AUX with `VerbForm=Inf`: bare roots, auxiliary infinitives, xcomp, periphrases, complements, purpose and subject clauses. NOUN-tagged nominalizations are excluded; f_15 remains zero. | ⚠️ A parser that mis-tags a finite verb as `VerbForm=Inf` causes a false positive; root infinitives are not suppressed to hide this model error. |
| `f_25_present_participle` | Gerund in adverbial function | Gerundio en función adverbial | Gerund `advcl/ccomp`; excludes predicative (`lo vi entrando`) and periphrastic (`está trabajando`) gerunds. | — |
| `f_26_past_participle` | Participle in absolute or adverbial construction | Participio en construcción absoluta o adverbial | Participle `advcl/ccomp/acl`, without `Voice=Pass`, excluding postnominal reduced relatives counted in f_27 (`terminada la reunión…`). | ⚠️ Detection varies with the dependency parse and the existing morphological voice restriction. |
| `f_27_past_participle_whiz` | Postnominal participles as reduced relatives | Participios postnominales con valor de relativa reducida | Participle `acl` with a NOUN head (`el artículo publicado ayer`). | — |
| `f_28_present_participle_whiz` | *(0)* Postnominal gerund | Gerundio postnominal | Always 0. | Spanish lacks a general productive mechanism of postnominal modifying gerunds. |
| `f_29_that_subj` | *que* relatives where the relative pronoun is the subject (absorbs f_31) | Relativas introducidas por *que* en las que el relativo funciona como sujeto (absorbe f_31) | Nominal `acl:*` relative: *que* as an explicit subject, or SCONJ/mark with no other `nsubj`, **plus** `quien/cual` with the subject role (merged from f_31). Pied-piping takes precedence. | ⚠️ With pure pro-drop, an object relative marked as SCONJ without an explicit subject is assigned here. |
| `f_30_that_obj` | *que* relatives where the relative pronoun is the direct object (absorbs f_32) | Relativas introducidas por *que* en las que el relativo funciona como complemento directo (absorbe f_32) | Nominal `acl:*` relative: *que* with an object role, or SCONJ/mark whose clause has an explicit `nsubj`, **plus** `quien/cual` with an object/oblique role (merged from f_32). Prepositional relatives assigned to f_33 are excluded. | The explicit-subject heuristic applies only to SCONJ/mark *que*, not to pronouns already annotated with an object role. |
| `f_31_wh_subj` | *(0)* Merged into f_29 | Integrado en f_29 | Always 0. Reverted: merged back into f_29 (the author's decision in review v2, not an artifact). | The count of `quien/cual` with the `nsubj` role lives in f_29 (see f_29). This column is kept only for schema compatibility (67 fixed columns). |
| `f_32_wh_obj` | *(0)* Merged into f_30 | Integrado en f_30 | Always 0. Reverted: merged back into f_30 (the author's decision in review v2, not an artifact). | The count of `quien/cual` with the `obj/iobj` role lives in f_30 (see f_30). This column is kept only for schema compatibility (67 fixed columns). |
| `f_33_pied_piping` | Relatives with a preposition before the relative pronoun | Relativas con preposición antepuesta al relativo | ADP + (DET) + relative pronoun (`por el cual`, `con quien`, `a quien`) in a nominal `acl`/`acl:*` clause in the same sentence. | Prepositional complements and indirect questions do not count here. *a quien* counts here because pied-piping takes precedence over f_29/f_30. |
| `f_34_sentence_relatives` | Relatives with a sentential antecedent | Relativas con antecedente oracional | `lo que`/`lo cual` with a propositional antecedent (`…, lo cual molestó…`). | Free relatives without a propositional antecedent do not count here. |
| `f_35_because` | Causal clauses introduced by *porque* | Cláusulas causales introducidas por *porque* | *porque*. | — |
| `f_36_though` | Concessive clauses introduced by *aunque* | Cláusulas concesivas introducidas por *aunque* | *aunque*. | — |
| `f_37_if` | Conditional clauses (*si*, *a menos que*, *salvo que*) | Cláusulas condicionales (*si*, *a menos que*, *salvo que*) | Conditional *si*, excluding indirect-question `ccomp/xcomp` under a listed communication/cognition verb. An `advcl` is not excluded merely because its main verb is `decir/saber`. Full `a menos que` / `salvo que` sequences count once per marked, sentence-local phrase. | Phrase evidence spans the full expression; all components are excluded from f_21/f_22/f_38. The main-clause conditional tense (f_54) is not required. ⚠️ `spanish-gsd` misparses `Preguntó si vendría mañana` as `advcl` (false positive), but correctly uses `ccomp` with `a la fiesta`. |
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
| `f_43_type_token` | Lexical diversity | Diversidad léxica | Lower-case surface-plus-tag types, not lemmas. Retain fine-grained tags, falling back to UPOS when missing or `_`. Exclude PUNCT/SYM/SPACE and English-style numeric matches (`\d_`) before calculation. Spelled-out numerals remain words; dictionary expressions are not compounded. **MATTR** by default; any document below 200 retained tokens switches the entire corpus to TTR. Explicit alternatives are retained. | One measure per corpus. The filtered threshold population differs from the normalization denominator. A shared `textstat_lexdiv()` call uses library defaults; MSTTR may warn when shortening its common segment. Filtered-empty documents receive `NA` and an identifying warning. Parser tag granularity affects type distinctions. |
| `f_44_mean_word_length` | Mean word length | Longitud media de palabras | Mean number of characters per token, punctuation excluded. | — |

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
| `f_59_contractions` | *(0)* Contractions | Contracciones | Always 0. | `al`/`del` **are** contractions for the RAE; what Spanish lacks is a productive system of verbal/negative contractions (`can't`, `I've`), which is what Biber measures. |
| `f_60_that_deletion` | *(0)* Restricted omission of the complementizer *que* | Omisión restringida de la conjunción completiva *que* | Always 0. | Omitting *que* exists marginally in formal Spanish (`espero sepan disculpar`); it is kept at 0 as a design decision (low productivity/comparability). |
| `f_61_stranded_preposition` | *(0)* Stranded preposition | Preposición pospuesta | Always 0. | Standard Spanish lacks an equivalent productive mechanism. |
| `f_62_split_infinitive` | *(0)* Split infinitive | Infinitivo escindido | Always 0. | The Spanish infinitive is a form with the `-r` affix, not particle + verb. |
| `f_63_split_auxiliary` | Adverbs between the auxiliary and its complement | Adverbios interpuestos entre el verbo auxiliar y el verbo auxiliado | ADV or negative PART between the auxiliary/modal and its immediate nonfinite complement, including validated modal periphrases (`puede fácilmente resolver`). One occurrence per split auxiliary/modal, anchored there; a long chain can contain multiple separate splits. | Excludes lexical complements (`quiero realmente estudiar`) and intervening subjects. Uses linear position, not adverb scope disambiguation. Incorrect modal lemmas also affect this feature. |

## O. Coordination

| Code | Label | Spanish label | Actual rule | Notes / limits |
|---|---|---|---|---|
| `f_64_phrasal_coordination` | Coordination of words or non-clausal phrases | Coordinación de palabras o grupos no oracionales | Coordination of nouns (including mixed NOUN/PROPN), adjectives, adverbs **and verbal/copular predicates** with a shared subject (`lee y escribe`, `canta y está contenta`). | `!has_subject` separates it from f_65. The Spanish-GSD fallback for ADJ `conj` attached to a noun with an `amod` adjective is retained. |
| `f_65_clausal_coordination` | Coordination of independent clauses with coordinating conjunctions | Coordinación de cláusulas independientes mediante conjunciones coordinantes | CCONJ between predicates with their own `nsubj/csubj`, including heads of finite copulas; or sentence-initial `y/e` attached to a verbal/copular predicate. Initial means first UD word, ignoring opening punctuation/symbols/spaces, not token ID 1; nominal fragments are excluded. | Verbal predicates explicitly marked nonfinite are excluded; missing verb-form tags retain the previous verbal fallback. Copular heads require a finite copula. Pro-drop and bad attachments still limit phrase/clause distinctions. |

## P. Negation

| Code | Label | Spanish label | Actual rule | Notes / limits |
|---|---|---|---|---|
| `f_66_neg_synthetic` | Negation with negative words other than *no* | Negación mediante palabras negativas distintas de *no* | `nadie/nunca/jamás/tampoco/ningún(o/a)/nada` + `ni`. Existing overlaps with f_11/f_67 (negative concord) are unchanged. | Inventory and cross-language concord mapping remain pending separate decisions. |
| `f_67_neg_analytic` | Negation with the adverb *no* | Negación mediante el adverbio *no* | ADV/PART `no` with `advmod` or `neg` and a real nonself word head, including nominal, pronominal, numeral, function-word, verbal, adjectival, and adverbial scope. Count the marker, preserving nouns. Exclude `no obstante`, identifiable replies/nominalizations, and quoted standalone `no`; retain `la no violencia`, `no fumador`, parenthetical negation, and quoted negative clauses. | Contextual guards are not general sense disambiguation: expletive `no`, non-negative `no más`, and ambiguous replies/nominal readings remain limitations. f_66 and f_42 are unchanged. Verified with Spanish-GSD and `es_core_news_sm`. |

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
- **Relatives with pro-drop** (f_29/f_30): without an explicit subject, the object
  relative is tagged `nsubj` and falls into f_29 instead of f_30.
- **Free relatives** (f_23): cannot be separated from f_29–f_34 without coreference
  (deferred).
- **Imperfects in `-ía`** (f_01): sometimes mis-tagged as `Mood=Cnd`.
- **Polysemous verbs** (f_55/f_57): `sostuvo` is mis-tagged as ADJ; disambiguating
  `decir` by mood was not implemented. `recomendar` (f_57) has a confirmed
  lemmatization limit in its diphthongized forms (see f_57).
- **Ambiguous discourse particles** (f_50): no `discourse` signal; the positional
  filter is not reliable (`Bueno`→ADJ, `Mira`→PROPN).
- **Encoding**: the accented phrases of f_39 need a UTF-8 locale; source-code literals
  use `\uXXXX` escapes instead of accented characters to avoid corruption when the
  package is loaded under non-UTF-8 locales (e.g. `LC_CTYPE=C`).

## Columns always at zero (10)

`f_09`, `f_12`, `f_15`, `f_28`, `f_31`, `f_32`, `f_59`, `f_60`, `f_61`, `f_62` —
justified linguistically above. (`f_31`/`f_32` were merged back into f_29/f_30 in
review v2, after being briefly split in Phase 2; `f_15` was activated in Phase 3 and
reverted by the author's decision.)
