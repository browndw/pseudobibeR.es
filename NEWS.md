# pseudobibeR.es 0.1.0 (development)

## Package interface

- Output columns use language-specific display names from
  `data-raw/feature_names.yaml` (D-54), e.g. `f_15_nominal_infinitives` for
  the shared identifier `f_15_gerunds`. Each name keeps its `f_NN_` number,
  which identifies the same feature in all three packages. Use
  `feature_names = "shared"` in `biber()`/`audit_features()`, or
  `options(pseudobibeR.feature_names = "shared")`, for the shared identifiers;
  `audit_features(feature = )` accepts either name.
- Added the common `biber()` and `audit_features()` S3 interfaces for UDPipe
  and spaCy annotations.
- Removed the Excel export API and archived Shiny applications outside the
  package project. `biber_es()`, `biber_es_batch()`, and `biber_es_traced()` are
  no longer public APIs. Use `biber()` for annotated corpora and
  `audit_features()` for feature occurrences.
- Removed the internal batch wrapper and its CSV ingestion, parsing, metadata,
  progress, and failure-recovery helpers. The legacy implementation is archived
  outside the package. `biber()` continues to accept multi-document annotations;
  parsing and corpus management remain the caller's responsibility.
- `measure = "none"` now omits lexical diversity, rather than returning an
  undocumented lemma-based TTR.
- Spanish datasets and reproducible builders replace inherited French samples.
- Removed the temporary French reference checkout from the Spanish project.
  Package builds exclude parser models and development outputs. Integration
  tests do not download models.

## Corrections that change results

- f_44 is the mean length of purely alphabetic words, unrounded, as in
  English and French (D-51). Numerals (`2024`, `3,5`), hyphenated and
  apostrophe forms no longer count, and values are no longer rounded to three
  decimals. A document without alphabetic words gets `NA`.
- f_63 no longer counts the analytic negator no as the only word between an
  auxiliary or modal and its complement (`puede no venir`), as English does
  not count "not" (D-49). Negative adverbs such as nunca still count, like
  English "never".
- f_60 also counts reduced complements of the public verbs decir, afirmar and
  asegurar (`El testigo afirma haber visto al ladrón`), as Biber's
  that-deletion counts public as well as private verbs (D-46). Suasive
  `decir de` + infinitive and que-clauses are not counted.
- f_15 also counts an infinitive heading a subject placed before its verb
  (`Fumar perjudica`, `Leer es un placer`), the Spanish analogue of an English
  gerund subject (D-43). Postposed subjects (`Es importante descansar`) remain
  f_24, as do objects and prepositional infinitives; gerunds and participles
  are not nominal in Spanish and are not counted.
- f_23 now also counts *lo que* free relatives that are a verb's argument
  (`Sé lo que hiciste`, `Me gusta lo que haces`), once per clause, as in
  Biber's definition and the French *ce que/ce qui* counterpart. Their *que* is
  no longer counted in f_21, and their *lo* is not an f_34 antecedent.
  Sentence relatives after punctuation and free relatives governed by a copula
  or preposition are excluded.
- The output now contains `doc_id` and the feature columns only, as in the
  English and French packages: `n_tokens` and `n_lex_tokens` were removed.
- The normalization denominator now excludes digit-final numerals (`42`), as
  in English; spelled-out numerals still count. It is the same word-token set
  as the lexical-diversity stream.
- `biber()` and `audit_features()` accept UDPipe and spacyr annotations only;
  the plain data-frame and default methods were removed, as in English.

- f_21 counts que complement clauses headed by a copular predicate
  (`Creo que es una buena idea`, `Dijo que estaba cansado`), which were missed
  because the clause head had to be a verb.
- f_03 excludes conditional and imperative forms even when a model marks them
  `Tense=Pres` (no change with Spanish-GSD).
- Generated accent twins are now checked word by word within phrases, so a
  phrase twin is not added when one of its words becomes a contrastive form
  (no change to the current packaged data).
- f_03 now counts finite present-tense verbs and auxiliaries in every mood,
  including the present subjunctive, in parallel with f_01 and English
  VBZ/VBP. Previously only the present indicative counted.
- f_20 also counts perfect (`ha habido`) and infinitival (`puede haber`)
  existential haber. Accent-insensitive matching now uses stringi
  transliteration instead of platform-dependent `iconv()`.
- Packaged word lists are read without YAML 1.1 boolean coercion. Previously
  `no` in `neg_analytic_adverbs` was stored as `false` (no count changed,
  because `negation_particles` already supplied `no`).
- The data builder adds unaccented twins of accented inventory entries
  (`aquí` -> `aqui`), except for contrastive grammatical words listed in
  `accent_contrastive_forms` (`él`/`el`, `más`/`mas`, `qué`/`que`...) and
  inflected forms where the accent is morphological. `ñ` is preserved.
- Removed unused word lists (`verb_matchlist`, the subordinator inventories,
  `coordinating_conjunctions`, `prepositions_simple`, `gerund_stoplist` and
  `adverb_mente_stoplist`); `coordinating_conjunctions` had been stored as
  deparsed strings. The f_11 dictionary key is now `f_11_indefinite_pronouns`,
  matching its output column.
- Corrected the public f_42 example: under the residual definition `Habla muy
  rápidamente y siempre llega puntualmente.` has two adverbs, not four.

- Activated f_61 for bounded resumptive prepositional relatives: nominal que
  relatives with finite predicate/subject evidence and linked third-person PP
  under initial hablar-de/depender-de/confiar-en frames. Ordinary subject/object
  relatives, known agreement conflicts and bare objects are excluded. One
  relative clause contributes its first matching PP span; detected markers
  leave f_29/f_30. Pied-piping and other relative bins are unchanged. Exact
  hablé/confié lookup is local to f_61; source annotations, denominators and
  lexical metrics are preserved. f_28 is the sole compatibility zero; the
  French f_61 revision remains a separate follow-up.
- Activated f_62 for adverb-interrupted preposition-infinitive units. Require
  a linked verbal infinitive and at least one non-negative adverb; negation-only
  cases are excluded, while negative ADV/PART can accompany genuine adverbs.
  Consecutive adverbs are supported without an arbitrary token cap. Subjects,
  punctuation breaks and nominal infinitives are excluded. Full matched-span
  evidence/audits, bounded mejor/siquiera lookup and compound/copular links
  preserve supplied annotations. Other features, denominators and metrics are
  unchanged; f_24/f_63 overlap is intentional.
- Activated f_60 as bounded functional complement reduction, not only literal
  que deletion. Initial creer/pensar/esperar complements qualify when infinitival
  or unmarked finite, including auxiliary/copular projection and clear
  coordination. Overt introducers, questions and nominal infinitives are
  excluded; this is not every infinitive. Clause-head evidence and audits
  support counts, while f_24 overlap is intentional. Source annotations,
  other features, denominators and metrics are unchanged. Additional predicates
  were outside this initial complement implementation.
- Activated f_59 for recognized colloquial contractions/written reductions:
  pa forms, onde/nde/onde's/ontas and reduced estar spellings. Surface lookup
  supports straight/curly apostrophes and native token splits without retagging
  or expanding words. Each longest expression counts once with full-span
  evidence/audits. Standard al/del, nominal determiner contexts and short
  nominal acronyms are excluded; this is not a generic apostrophe detector.
  Source annotations, other features, denominators and lexical metrics remain
  unchanged. f_60/f_62 scope decisions and genuine f_61 stranding were separate
  from this contraction implementation.
- Activated f_31/f_32 for noun-headed quien/cual relatives in subject/object
  roles; f_29/f_30 now contain only que relatives. Counts and audits use a
  shared role partition with exclusive pied-piping f_33 precedence.
  Bounded checks repair conflicting nsubj assignments and singular-relative/
  singular-antecedent disagreement with first/second-person plural predicates,
  protecting coordinated subjects. Relative morphology supports narrow native
  finite appos-attachment recovery. Roleless que is no longer automatically
  counted as a subject: recovery requires positive intransitive/locative
  evidence, or an external subject for an object reading. Ambiguous gaps remain
  unclassified. Source annotations, denominators, lexical metrics and feature
  identifiers are unchanged.
- Activated `f_15_gerunds` for clear noun-like nominal infinitives. Shared
  occurrence selection combines verb/infinitive identity with nominal structure,
  excluding internal verbal arguments/modifiers rather than counting endings,
  articles, or all clausal subjects. Each qualifying word moves out of
  f_14/f_16/f_24, preventing duplicate counts and audits. Known verb identities
  support NOUN/PROPN annotations and the native `comer` -> `comero` lemma error
  without retagging. Nominal subjects recovered from csubj do not license
  impersonal f_09. Verbal infinitival clauses, supplied token populations and
  lexical metrics remain unchanged.
- Activated `f_12_proverb_do` for anaphoric lexical hacer with action-proform
  lo (including enclitics) or lo mismo. Counts and audits identify the verb once,
  excluding auxiliary/copular, causative, identifiable reflexive and overt
  nominal-object contexts. Native split-gerund and compound-enclitic lemmas
  receive narrow feature-local support. Bare lo remains a structural candidate
  without semantic antecedent resolution; other proforms are deferred.
  All other features, token populations and lexical metrics remain unchanged.
- Activated `f_09_pronoun_it` as a Spanish impersonal/expletive predicate
  measure rather than a compatibility zero. Weather constructions and
  selected impersonal clausal constructions generate one audited occurrence
  per real predicate word. A closed root-weather-form rule handles known native
  parser mistaggings without altering annotations. No null subjects or empty
  nodes are invented; existential haber remains in f_20. Personal pro-drop,
  generic complements, ambiguous se, and neuter lo are excluded from this
  baseline. Other feature counts, token populations, and lexical metrics
  remain unchanged.
- Synthetic negation retains the approved negative lexical inventory and
  per-word concord counts. Ordinary nominal uses with a preceding dependent
  article, including mis-tagged `la nada` and `un nadie`, no longer count.
  A guarded fallback recognizes verbal-subject `nadie` mis-tagged PROPN by
  native spaCy. Original token annotations and occurrence order are preserved;
  all other features, analytic audits, denominators, and lexical metrics are
  unchanged. Idiom disambiguation and new classes such as `sin` remain deferred.
- Total adverbs (`f_42_adverbs`) now follows Biber's exclusion definition:
  ADV occurrences counted as place/time adverbials, downtoners, hedges, or
  amplifiers are excluded. The union of actual detections and full phrase spans
  determines exclusions; feature totals are not subtracted. Conjuncts,
  emphatics, discourse particles, and negation are not additional exclusions.
  Specialized counts, normalization denominators, and lexical metrics are
  unchanged; f_42 audit rows now describe only its retained occurrences.
- Lexical diversity now uses lower-case surface-plus-tag types, matching the
  original English type definition rather than untagged surfaces. Fine-grained
  tags are retained; missing or `_` tags use UPOS. Punctuation, symbols, and
  spaces are excluded by UD POS, and English-style numeric matches are removed
  before selecting the corpus measure or calculating diversity. Spelled-out
  numerals remain words; dictionary phrases are not compounded for this metric.
  Filtered-empty documents receive an explicit `NA` metric and identifying warning.
  Word-count normalization, mean word length, and feature occurrence counts
  continue to use their existing token populations.
- Lexical diversity uses one measure for the entire supplied corpus. Default
  MATTR falls back to TTR for every document if any document has fewer than
  200 tokens, with one message. Explicit TTR, CTTR, and MSTTR choices are retained.
  Calculation uses a single corpus-wide `quanteda.textstats` call, including
  its shared MSTTR segment adjustment and warning for short documents.
- Extraction and auditing now explicitly retrieve this package's Spanish
  dictionaries and word lists. Namespace-only calls work without attaching
  the package, and other packages' datasets cannot change Spanish counts or
  audit evidence through attachment order.
- Native Spanish spaCy annotations now normalize `ROOT` before structural
  detection, fixing missed existential `haber` and direct questions incorrectly
  included among indirect questions. SpaCy passives also accept `ser` tagged
  `aux` with a verbal participle and nominal `por`-agents tagged `obj`;
  these passive exceptions do not change UDPipe conventions.
- Reflexive `darse cuenta` accepts the nominal `cuenta` dependency `compound`
  as well as `obj`. Native spaCy integration, portable frozen annotations,
  explicit morphology setup, and parser-tokenization limits are documented.
- Modal recognition follows infinitival complements through flat and nested
  auxiliary chains, including passive/progressive complements. `tener que`,
  `haber de/que`, and `ir a` require their linked particles. `puede que`
  also accepts an explicitly subjunctive complement-clause annotation.
- Split auxiliaries use the immediate nonfinite complement, avoiding duplicate
  splits across longer chains. Lexical complements such as `quiero estudiar`
  are not modal splits; evidence is anchored on the auxiliary/modal itself.
- Specialized verb detection recognizes annotated reflexive `darse cuenta`
  and adjectival `verse`, anchoring each on the verb. Appearance uses of `ver`
  do not also count as private verbs. General lexical sense disambiguation
  remains outside these rules.
- Coordination permits mixed common/proper nouns and verbal/copular
  predicates. Explicit nominal or clausal subjects distinguish clausal
  coordination. Initial `y/e` uses actual sentence position, not token ID 1,
  and requires a verbal predicate or a finite copula rather than a fragment.
- Analytic negation requires ADV/PART and accepts `advmod` or spaCy-style `neg`;
  valid nominal, pronominal, numeral, and function-word scopes now count alongside
  verbal/adjectival/adverbial scope. Counts and audits identify the `no` marker,
  never a consumed noun phrase. Contextual guards exclude `no obstante`,
  identifiable reply prefixes, nominalized `no`, and quoted standalone `no`
  while retaining `la no violencia`, `un no fumador`, parenthetical negation,
  and quoted negative clauses. General semantic disambiguation, including
  expletive `no` and non-negative `no más`, remains unresolved. Existing f_66
  inventory, negative-concord overlap, f_42 mapping, noun counts, denominators,
  and lexical metrics are unchanged.
- Hyphenated UD nouns and attributive adjectives are no longer discarded.
  Attributive and predicative roles propagate through adjective coordination
  chains. Explicit predicates are excluded from attributive fallback, and
  adjectives in noncomplement roles are not treated as copular attributes.
- Prepositional locutions use the packaged `prepositions_compound` inventory
  instead of a shorter detector-local list. Former detector entries are
  preserved there; existing additional entries such as `a excepcion de` now
  produce a single phrase occurrence with full audit evidence.
- Reflexive passives include compound tenses with a third-person finite
  auxiliary, while explicit reflexive/impersonal dependency labels are excluded.
  Passive agents require a `por` case phrase attached as an oblique to the
  passive predicate. Modal `haber` with a dependent infinitive is not existential.
- Verbal infinitives include bare roots and auxiliaries. Postnominal reduced
  participles no longer also count as adverbial participles.
- Verb complements require a complement-clause relation, not merely a
  subordinate `que`. Pied-piping requires a nominal relative clause and takes
  precedence over subject/object relatives; nonrelative `quien/cual` is excluded.
- Conditional `si` in an adverbial clause is retained even after a communication
  verb. `a menos que` and `salvo que` have one sentence-local phrase occurrence,
  with all components excluded from complement and other-subordinator counts.
- Perfect aspect counts auxiliary `haber` compounds once, including perfect
  passive, progressive, and copular chains. Progressive `estar` and modal
  `haber de` + infinitive are not perfects; passive voice no longer suppresses
  a genuine perfect. Indefinite pronouns exclude `det:*` determiner relations.
- Frequencies are now normalized per 1,000 UD words, excluding punctuation,
  symbols, spaces, and multiword-token containers. Previously the denominator
  included punctuation and depended on phrase compounding.
- Adjective fallback is document-local.
- Dictionary and grammatical detections no longer double-count specialized
  verbs. Counts and auditing share detection-level evidence, including
  dictionary phrases and prepositional locutions.
- Passive agent phrases are associated with their clause through dependencies,
  not a fixed positional window that could cross sentence boundaries. The
  Spanish-GSD agent-versus-cause ambiguity remains a parser limitation.

These are explicit Spanish corrections; they do not imply that English or
French companion implementations already use identical measurement semantics.
