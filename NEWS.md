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
- Package builds exclude parser models, the temporary French reference, and
  development outputs. Integration tests do not download models.

## Corrections that change results

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
  nominal and interjection uses of `no` are excluded. Existing negative-concord
  overlap between negative words and `no` is unchanged.
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
- Lexical-diversity fallback and adjective fallback are document-local.
- Dictionary and grammatical detections no longer double-count specialized
  verbs. Counts and auditing share detection-level evidence, including
  dictionary phrases and prepositional locutions.
- Passive agent phrases are associated with their clause through dependencies,
  not a fixed positional window that could cross sentence boundaries. The
  Spanish-GSD agent-versus-cause ambiguity remains a parser limitation.

These are explicit Spanish corrections; they do not imply that English or
French companion implementations already use identical measurement semantics.
