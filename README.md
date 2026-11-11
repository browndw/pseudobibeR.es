# pseudobibeR.es: Biber's Linguistic Features for Spanish

[![R-CMD-check](https://github.com/browndw/pseudobibeR.es/workflows/R-CMD-check/badge.svg)](https://github.com/browndw/pseudobibeR.es/actions)
[![Tests](https://github.com/browndw/pseudobibeR.es/actions/workflows/tests.yml/badge.svg)](https://github.com/browndw/pseudobibeR.es/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/pseudobibeR.es)](https://CRAN.R-project.org/package=pseudobibeR.es)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/pseudobibeR.es)](https://cran.r-project.org/package=pseudobibeR.es)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

The **pseudobibeR.es** package counts the 67 lexicogrammatical and functional
features described by Biber (1985, 1988) in Spanish texts. These features
(tense, pronouns, passives, relative clauses, modals, negation and many more)
are widely used to compare registers, genres and text types with
multidimensional analysis.

It is the Spanish companion of [pseudobibeR](https://github.com/browndw/pseudobibeR)
(English) and [pseudobibeR.fr](https://github.com/browndw/pseudobibeR.fr)
(French). All three packages share the same interface and the same feature
numbers, so results can be compared across languages.

## Overview

The package does not tag texts itself. It reads the output of a parser,
[udpipe](https://bnosac.github.io/udpipe/en/) or
[spaCy](https://spacy.io/) (via [spacyr](https://spacyr.quanteda.io/)), and
counts features from the parts of speech, morphology and syntactic
dependencies it provides. The counts are only as good as the parse: unusual
spelling, informal punctuation or very specialized language can lower accuracy.

Most features carry over directly from English (passives, relative clauses,
modals). A few English features have no exact Spanish equivalent. In those
cases the package counts the Spanish construction that does the same job. For
example, f_15 counts infinitives used as nouns (*Fumar perjudica la salud*),
because Spanish uses the infinitive where English uses a gerund (*Smoking
harms your health*). One feature, f_28, has no Spanish counterpart and is
always 0. See [Comparing languages](#comparing-languages).

## Installation

```r
# install.packages("remotes")
remotes::install_github("browndw/pseudobibeR.es")
```

## Quick start

A typical workflow has three steps: read and clean the texts, parse them, and
count the features.

```r
library(readtext)
library(quanteda)
library(udpipe)
library(pseudobibeR.es)

# 1. Read the texts and normalize typographic quotes (’ -> ')
docs <- readtext("corpus/*.txt", encoding = "UTF-8")
docs$text <- utf8::utf8_normalize(docs$text, map_quote = TRUE)
corp <- corpus(docs)

# 2. Parse with udpipe (the model is downloaded once)
model <- udpipe_download_model(language = "spanish-gsd")
ud_model <- udpipe_load_model(model$file_model)
parsed <- udpipe_annotate(ud_model, x = as.character(corp), doc_id = docnames(corp))

# 3. Count the features: one row per document
features <- biber(parsed)
```

`utf8::utf8_normalize()` converts curly apostrophes and quotes to straight
ones and composes accented letters consistently. It keeps accents and *ñ*.
Parsers handle normalized text more reliably, and `biber()` only sees the
parsed tokens, so this step belongs before parsing.

### Using spaCy instead of udpipe

spaCy needs Python with a Spanish model installed (for example
`es_core_news_sm`). Request morphology, which `biber()` needs:

```r
library(spacyr)
spacy_initialize(model = "es_core_news_sm")
parsed <- spacy_parse(corp, pos = TRUE, tag = TRUE, dependency = TRUE,
                      entity = FALSE, additional_attributes = "morph")
features <- biber(parsed)
spacy_finalize()
```

Both parsers work with the same detectors, but their accuracy differs on
individual constructions.

### Trying it with sample data

```r
library(pseudobibeR.es)
biber(udpipe_samples)
```

## Understanding the output

`biber()` returns a data frame with one row per document: a `doc_id` column
followed by the feature columns.

- **Rates per 1,000 words.** By default (`normalize = TRUE`) counts are
  divided by the number of words in the document and multiplied by 1,000, so
  documents of different lengths can be compared. Words exclude punctuation,
  symbols and numbers written in digits. Use `normalize = FALSE` for raw
  counts.
- **Two measures are not counts.** f_43 is the type/token ratio (lexical
  diversity) and f_44 is the mean word length in letters; neither is
  normalized. The `measure` argument selects the type/token ratio: `"MATTR"`
  (default), `"TTR"`, `"CTTR"`, `"MSTTR"` or `"none"` (which omits f_43).
  MATTR needs at least 200 words per document; if any document is shorter,
  plain TTR is used for every document and a message is shown.
- **Column names.** Columns use Spanish-specific names, such as
  `f_09_impersonal` or `f_15_nominal_infinitives`. Use
  `feature_names = "shared"` for the names shared with the English and French
  packages (see [Comparing languages](#comparing-languages)).

## Checking results

`audit_features()` shows every occurrence behind a count, with its context:

```r
audit_features(parsed, feature = "f_18_agent_passives")
audit_features(parsed, category = "F", sample_n = 10, seed = 1)  # passives
```

It is a quick way to check a surprising value or to see what a feature
captures in your own texts.

## Linguistic features

The 67 features are grouped into the 16 categories used by Biber. Bold type in
the examples marks what is counted.

| Category | Features | Description |
|---|---|---|
| A. Tense and aspect | f_01–f_03 | Past, perfect, present |
| B. Place and time adverbials | f_04–f_05 | *aquí*, *ayer*… |
| C. Pronouns and pro-verbs | f_06–f_12 | Personal, demonstrative and indefinite pronouns; impersonal constructions; *hacerlo* |
| D. Questions | f_13 | Questions with an interrogative word |
| E. Nominal forms | f_14–f_16 | Nominalizations, nominal infinitives, other nouns |
| F. Passives | f_17–f_18 | Passives without and with an agent |
| G. Stative forms | f_19–f_20 | *ser/estar*, existential *haber* |
| H. Subordination | f_21–f_38 | Complement, relative, participial and adverbial clauses |
| I. Prepositions, adjectives and adverbs | f_39–f_42 | |
| J. Lexical specificity | f_43–f_44 | Type/token ratio, word length |
| K. Lexical classes | f_45–f_51 | Connectors, hedges, intensifiers, discourse markers… |
| L. Modals | f_52–f_54 | Possibility, necessity, prediction |
| M. Specialized verb classes | f_55–f_58 | Communication, thinking, persuasion, *parecer* |
| N. Reduced forms | f_59–f_63 | Colloquial reductions and interrupted structures |
| O. Coordination | f_64–f_65 | Phrasal and clausal coordination |
| P. Negation | f_66–f_67 | Negative words and *no* |

### Feature list

| Feature | Description | Descripción | Examples |
|---|---|---|---|
| **A. Tense and aspect** | | | |
| `f_01_past_tense` | Past tense (preterite and imperfect) | Pasado (pretérito perfecto simple e imperfecto) | *María **llegó** tarde.* *Antes **vivíamos** aquí.* |
| `f_02_perfect_aspect` | Perfect aspect: *haber* + participle | Aspecto perfecto: *haber* + participio | ***He** terminado el informe.* |
| `f_03_present_tense` | Present tense (indicative and subjunctive) | Presente (indicativo y subjuntivo) | *El profesor **explica** la lección.* |
| **B. Place and time adverbials** | | | |
| `f_04_place_adverbials` | Place adverbials | Adverbiales de lugar | *aquí, lejos, arriba* |
| `f_05_time_adverbials` | Time adverbials | Adverbiales de tiempo | *ayer, hoy, pronto* |
| **C. Pronouns and pro-verbs** | | | |
| `f_06_first_person_pronouns` | First-person pronouns | Pronombres de primera persona | *yo, nosotros, me* |
| `f_07_second_person_pronouns` | Second-person pronouns | Pronombres de segunda persona | *tú, usted, te* |
| `f_08_third_person_pronouns` | Third-person personal pronouns | Pronombres personales de tercera persona | *él, ella, lo, les* |
| `f_09_impersonal` | Impersonal constructions (counterpart of English *it*): weather verbs and impersonal predicates | Construcciones impersonales: verbos meteorológicos y predicados impersonales | ***Llueve.*** *Es **importante** que vengas.* |
| `f_10_demonstrative_pronoun` | Demonstratives used as pronouns | Demostrativos con función pronominal | ***Eso** no es verdad.* |
| `f_11_indefinite_pronouns` | Indefinite pronouns | Pronombres indefinidos | ***Alguien** dejó **algo**.* |
| `f_12_proverb_hacer` | *hacer* standing for an action already mentioned (counterpart of pro-verb *do*) | *hacer* que retoma una acción ya mencionada | *Luis también lo **hizo**.* |
| **D. Questions** | | | |
| `f_13_wh_question` | Questions with an interrogative word | Preguntas con palabra interrogativa | *¿**Qué** pasó?* *¿**Dónde** vives?* |
| **E. Nominal forms** | | | |
| `f_14_nominalizations` | Nouns derived with suffixes such as *-ción, -miento, -dad* | Nominalizaciones (*-ción, -miento, -dad*…) | *la **organización** del trabajo* |
| `f_15_nominal_infinitives` | Infinitives used as nouns (counterpart of English gerunds) | Infinitivos con función nominal | ***Fumar** perjudica la salud.* *El **saber** es valioso.* |
| `f_16_other_nouns` | Other nouns | Otros sustantivos | *la **mesa**, **Madrid*** |
| **F. Passives** | | | |
| `f_17_agentless_passives` | Passives without an agent, including the *se* passive | Pasivas sin agente, incluida la pasiva refleja | *La propuesta **fue aprobada**.* ***Se publicaron** los datos.* |
| `f_18_agent_passives` | Passives with an agent introduced by *por* | Pasivas con agente (*por*) | *La novela fue escrita **por** García Márquez.* |
| **G. Stative forms** | | | |
| `f_19_copula_main_verb` | *ser* and *estar* as main verbs | *ser* y *estar* como verbos principales | *El libro **es** interesante.* *Ana **está** en Lima.* |
| `f_20_existential` | Existential *haber* (*hay*) | *haber* existencial | ***Hay** tres errores en el documento.* |
| **H. Subordination** | | | |
| `f_21_que_verb_comp` | *que*-clauses after a verb | Completivas con *que* tras un verbo | *Creo **que** el proyecto tendrá éxito.* |
| `f_22_que_adj_comp` | *que*-clauses after an adjective | Completivas con *que* tras un adjetivo | *Es importante **que** todos participen.* |
| `f_23_wh_clause` | Indirect questions and free relatives (*lo que*) | Interrogativas indirectas y relativas libres | *No sé **quién** llamó.* *Sé **lo que** hiciste.* |
| `f_24_infinitives` | Infinitives used as verbs | Infinitivos con función verbal | *Quiere **terminar** el trabajo.* |
| `f_25_gerund_clause` | Adverbial clauses headed by a gerund | Cláusulas adverbiales de gerundio | ***Caminando** por el parque, encontré a mi vecino.* |
| `f_26_past_participle` | Adverbial clauses headed by a past participle | Cláusulas adverbiales de participio | ***Terminada** la reunión, todos se fueron.* |
| `f_27_past_participle_reduced_relative` | Past participle modifying a noun (reduced relative clause) | Participio que modifica a un sustantivo (relativa reducida) | *El artículo **publicado** ayer es interesante.* |
| `f_28_gerund_reduced_relative` | Always 0: a gerund modifying a noun is not standard Spanish | Siempre 0: el gerundio especificativo no es normativo | (*una caja conteniendo libros*) |
| `f_29_que_subj` | Relative clauses with *que*, as subject | Relativas con *que* en función de sujeto | *El estudiante **que** llegó tarde…* |
| `f_30_que_obj` | Relative clauses with *que*, as object | Relativas con *que* en función de objeto | *El libro **que** María escribió…* |
| `f_31_quien_cual_subj` | Relative clauses with *quien* or *el cual*, as subject | Relativas con *quien* o *el cual* en función de sujeto | *Los candidatos, **quienes** aprobaron, recibieron una beca.* |
| `f_32_quien_cual_obj` | Relative clauses with *quien* or *el cual*, as object | Relativas con *quien* o *el cual* en función de objeto | *El informe, **el cual** María revisó, fue publicado.* |
| `f_33_pied_piping` | Relative clauses introduced by a preposition | Relativas con preposición antepuesta | *La persona **con quien** hablé es mi tía.* |
| `f_34_sentence_relatives` | Relative clauses that comment on a whole clause | Relativas oracionales | *Llegó tarde, **lo cual** me molestó.* |
| `f_35_causal` | Causal clauses with *porque* | Subordinadas causales con *porque* | *No vino **porque** estaba enfermo.* |
| `f_36_concessive` | Concessive clauses with *aunque* | Subordinadas concesivas con *aunque* | ***Aunque** llovía, salimos.* |
| `f_37_conditional` | Conditional clauses with *si*, *a menos que*, *salvo que* | Subordinadas condicionales | ***Si** llueve, nos quedamos en casa.* |
| `f_38_other_adv_sub` | Other adverbial subordinators | Otros subordinadores adverbiales | ***cuando**, **mientras**, **para que*** |
| **I. Prepositions, adjectives and adverbs** | | | |
| `f_39_prepositions` | Prepositions | Preposiciones | *El libro está **sobre** la mesa **de** la biblioteca.* |
| `f_40_adj_attr` | Adjectives modifying a noun | Adjetivos atributivos | *una solución **nueva*** |
| `f_41_adj_pred` | Adjectives after a linking verb | Adjetivos predicativos | *El resultado es **positivo**.* |
| `f_42_adverbs` | Other adverbs (not counted in f_04, f_05, f_46–f_48) | Otros adverbios | *Habla **rápidamente**.* |
| **J. Lexical specificity** | | | |
| `f_43_type_token` | Type/token ratio (lexical diversity) | Diversidad léxica | — |
| `f_44_mean_word_length` | Mean word length in letters | Longitud media de las palabras | — |
| **K. Lexical classes** | | | |
| `f_45_conjuncts` | Connectors between sentences | Conectores | ***Sin embargo**, continuaron.* |
| `f_46_downtoners` | Downtoners (weaken a statement) | Atenuadores | *El cambio es **apenas** perceptible.* |
| `f_47_hedges` | Hedges (mark uncertainty) | Marcadores de duda o imprecisión | ***Quizás** dependa de otros factores.* |
| `f_48_amplifiers` | Amplifiers (strengthen a statement) | Intensificadores | *una diferencia **muy** significativa* |
| `f_49_emphatics` | Emphatics | Enfáticos | ***De hecho**, los resultados lo confirman.* |
| `f_50_discourse_particles` | Discourse markers at the start of a clause | Marcadores discursivos al inicio de cláusula | ***Bueno**, **pues** vamos.* |
| `f_51_demonstratives` | Demonstratives before a noun | Determinantes demostrativos | ***este** libro* |
| **L. Modals** | | | |
| `f_52_modal_possibility` | Possibility (*poder*) | Posibilidad (*poder*) | ***Podemos** mejorar los resultados.* |
| `f_53_modal_necessity` | Necessity and obligation | Necesidad y obligación | ***Debemos** revisar los datos.* ***Hay que** validarlo.* |
| `f_54_modal_predictive` | Prediction: future, conditional, *ir a* | Predicción: futuro, condicional, *ir a* | *El equipo **presentará** los resultados.* ***Vamos a** analizarlos.* |
| **M. Specialized verb classes** | | | |
| `f_55_verb_public` | Verbs of communication | Verbos de comunicación | ***afirmar**, **anunciar**, **declarar*** |
| `f_56_verb_private` | Verbs of thinking and feeling | Verbos de pensamiento y percepción | ***creer**, **pensar*** |
| `f_57_verb_suasive` | Verbs of persuasion and request | Verbos de persuasión y petición | ***recomendar**, **pedir*** |
| `f_58_verb_seem` | *parecer* and copular *resultar* | *parecer* y *resultar* copulativo | ***Parece** que no son concluyentes.* |
| **N. Reduced forms** | | | |
| `f_59_reduced_forms` | Colloquial reduced spellings | Formas reducidas coloquiales | ***pa'** (para), **toy** (estoy)* |
| `f_60_reduced_complement` | Reduced complement in place of a *que*-clause, after verbs like *creer* and *decir* | Completiva reducida en lugar de *que* | *Creo **haber entendido**.* *Afirma **haber visto** al ladrón.* |
| `f_61_resumptive_relative` | Relative clause with a repeated pronoun (colloquial alternative to a preposition + relative) | Relativa con pronombre reasuntivo | *el chico que hablé **de él*** |
| `f_62_split_infinitive` | Adverb between a preposition and an infinitive | Adverbio entre preposición e infinitivo | *Trato de **realmente** comprender.* |
| `f_63_split_auxiliary` | Adverb between an auxiliary or modal and its verb | Adverbio entre auxiliar y verbo | *Puede **fácilmente** hacerlo.* |
| **O. Coordination** | | | |
| `f_64_phrasal_coordination` | Coordination of words or phrases | Coordinación de sintagmas | *los estudiantes **y** los profesores* |
| `f_65_clausal_coordination` | Coordination of clauses | Coordinación de cláusulas | *Ella está contenta **y** él está triste.* |
| **P. Negation** | | | |
| `f_66_neg_synthetic` | Negative words | Palabras negativas | ***Nadie** sabe **nada**.* |
| `f_67_neg_analytic` | Negation with *no* | Negación con *no* | ***No** llegó.* |

For the exact rules behind each count, including exclusions and known parser
limitations, see the [technical reference](FEATURES_TECHNICAL.md).

## Comparing languages

The feature numbers (`f_01` … `f_67`) refer to the same feature in English,
Spanish and French. Only the column names differ, where a Spanish or French
name describes the counted construction better. To combine or compare results
across languages, use the shared names:

```r
biber(parsed, feature_names = "shared")

# or for the whole session, in all three packages:
options(pseudobibeR.feature_names = "shared")
```

The [cross-linguistic feature table](https://github.com/browndw/pseudobibeR/blob/main/CROSS_LINGUISTIC_FEATURES.md)
shows how each feature is realized in the three languages.

## Further documentation

- [FEATURES_TECHNICAL.md](FEATURES_TECHNICAL.md): exact counting rules for
  each feature.
- [NEWS.md](NEWS.md): changes between versions.
- [CONTRIBUTING.md](CONTRIBUTING.md): editing word lists, rebuilding package
  data and running tests.

## Citation

Please cite Biber's original work and this package:

> Biber, D. (1985). Investigating macroscopic textual variation through
> multifeature/multidimensional analyses. *Linguistics*, 23(2), 337–360.
> <https://doi.org/10.1515/ling.1985.23.2.337>

> Biber, D. (1988). *Variation across speech and writing*. Cambridge
> University Press. <https://doi.org/10.1017/CBO9780511621024>

> Brown, D. W., & Cordovez, M. (2026). *pseudobibeR.es: Aggregate counts of
> Spanish linguistic features*. R package. <https://github.com/browndw/pseudobibeR.es>

## License

MIT © the package authors.
