# pseudobibeR.es 0.1.0 (development)

## Package interface

- Added the common `biber()` and `audit_features()` S3 interfaces for UDPipe,
  spaCy, and annotated data frames.
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
