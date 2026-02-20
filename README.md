# semgram.nl: Extracting Semantic Motifs from Dutch Textual Data

`semgram.nl` is a Dutch language adaptation of the [`semgram`](https://github.com/omstuhler/semgram) R package by Oscar Stuhler for extracting semantic motifs around entities in textual data. Like the original, `semgram.nl` uses an entity-centered semantic grammar that distinguishes six classes of motifs: **actions** of an entity, **treatments** of an entity, **agents** acting upon an entity, **patients** acted upon by an entity, **characterizations** of an entity, and **possessions** of an entity. This adaptation of the orginal package was created with the help of Claude Opus 4.6.

This package adapts the comprehensive set of extraction rules from the original `semgram` to work with **Dutch syntax**, **Universal Dependencies labels** (as output by spaCy's Dutch models), and **Dutch lemmas**.

## Why a Dutch Adaptation?

The original `semgram` was designed for English and relies on ClearNLP-style dependency labels used by spaCy's English models. Dutch spaCy models (trained on UD Alpino/LassySmall treebanks) use **Universal Dependencies** labels, which differ significantly. Additionally, Dutch has several syntactic features that require adapted extraction rules.

### Key Differences Between English and Dutch Dependency Parsing

| Feature | English (ClearNLP) | Dutch (Universal Dependencies) |
|---|---|---|
| Direct object | `dobj` | `obj` |
| Indirect object | `dative` | `iobj` |
| Passive subject | `nsubjpass` | `nsubj:pass` |
| Passive auxiliary | `auxpass` | `aux:pass` |
| Agent in passive | `agent` → `pobj` (of "by") | `obl:agent` (direct) |
| Preposition object | `pobj` (child of `prep`) | `obl` / `nmod` (with `case` child) |
| Possessive modifier | `poss` | `nmod:poss` |
| Copular verb | Verb is head (with `attr`/`acomp`) | Predicate is head (with `cop` child) |
| Compound particles | `compound` / `prt` | `compound:prt` |
| "by" agent preposition | `by` (lemma) | `door` (lemma) |
| "have" verb | `have` (lemma) | `hebben` (lemma) |
| "of" preposition | `of` (lemma) | `van` (lemma) |
| Infinitive marker | `to` | `te` |
| Inherent reflexive | — | `expl:pv` |

### Dutch-Specific Syntactic Features Handled

1. **V2 Word Order**: Dutch is a V2 language — the finite verb comes second in main clauses, but final in subordinate clauses. spaCy's UD models handle this correctly in the parse tree, so the dependency-based rules work regardless of surface word order.

2. **Separable Verbs** (*scheidbare werkwoorden*): Dutch has many separable prefix verbs (e.g., "opbellen" → "belde op"). In spaCy's UD output, the particle is marked with `compound:prt`. The lemmatizer correctly maps these to the full infinitive.

3. **Copular Constructions**: In UD Dutch, copular verbs ("zijn", "worden", "lijken", "blijken", "schijnen") are **not** the head of the clause. Instead, the predicate (adjective or noun) is the head, and the copula is attached via `cop`. This is the most significant structural difference from the English ClearNLP style, where the verb is always the head.

4. **Door-Passives**: Dutch uses "door" (equivalent to English "by") to introduce agents in passive sentences. In UD, this is marked with the `obl:agent` relation directly, rather than requiring traversal through `agent` → `pobj`.

5. **Van-Possessives**: The "van" (of) construction for possession ("de auto van Emil") uses `nmod` with a `case` child for "van".

## Installation

```r
# Install from GitHub
devtools::install_github("your-username/semgram.nl")
```

### Prerequisites

`semgram.nl` requires:
- [`rsyntax`](https://CRAN.R-project.org/package=rsyntax) for querying dependency trees
- [`data.table`](https://CRAN.R-project.org/package=data.table) for efficient data handling
- [`spacyr`](https://CRAN.R-project.org/package=spacyr) (optional, for parsing Dutch text)

To parse Dutch text, you need spaCy with a Dutch model:

```r
# Install spacyr and initialize
library(spacyr)
spacy_install()  # or use an existing Python/spaCy installation
spacy_download_langmodel("nl_core_news_lg")  # recommended: use the large model
spacy_initialize(model = "nl_core_news_lg")
```

## Example

### Step 1: Parse Dutch text

```r
library(spacyr)
spacy_initialize(model = "nl_core_news_lg")

text = "Emil achtervolgde de dief."
tokens_df = spacy_parse(text, dependency = TRUE)
tokens_df

#>   doc_id sentence_id token_id         token       lemma   pos head_token_id dep_rel
#> 1  text1           1        1          Emil        Emil PROPN             2   nsubj
#> 2  text1           1        2 achtervolgde achtervolgen  VERB             2    ROOT
#> 3  text1           1        3            de          de   DET             4     det
#> 4  text1           1        4          dief        dief  NOUN             2     obj
#> 5  text1           1        5             .           . PUNCT             2   punct
```

### Step 2: Extract motifs

```r
library(semgram.nl)

extract_motifs(tokens = tokens_df, entities = c("Emil"), markup = TRUE)

#> $actions
#>   doc_id     ann_id Entity        action      markup
#>    text1 text1.1.1   Emil  achtervolgen  a_achtervolgen
#>
#> $treatments
#>   [empty]
#>
#> $characterizations
#>   [empty]
#>
#> $possessions
#>   [empty]
#>
#> $agent_treatments
#>   [empty]
#>
#> $action_patients
#>   doc_id     ann_id Entity        action Patient          markup
#>    text1 text1.1.2   Emil  achtervolgen    dief  aP_achtervolgen_dief
```

### More Examples

```r
# Passive construction with 'door'
text2 = "Emil werd gebeld door Jan."
tokens2 = spacy_parse(text2, dependency = TRUE)
extract_motifs(tokens = tokens2, entities = c("Emil"), markup = TRUE)
# → treatment: t_bellen, agent_treatment: At_Jan_bellen

# Characterization with copula
text3 = "Emil is vriendelijk en eerlijk."
tokens3 = spacy_parse(text3, dependency = TRUE)
extract_motifs(tokens = tokens3, entities = c("Emil"), markup = TRUE)
# → characterizations: be_vriendelijk, be_eerlijk

# Possession with 'hebben'
text4 = "Emil heeft vrienden en vijanden."
tokens4 = spacy_parse(text4, dependency = TRUE)
extract_motifs(tokens = tokens4, entities = c("Emil"), markup = TRUE)
# → possessions: H_vriend, H_vijand

# Possession with 'van'
text5 = "De wielen van de auto waren oud."
tokens5 = spacy_parse(text5, dependency = TRUE)
extract_motifs(tokens = tokens5, entities = c("auto"), markup = TRUE)
# → possessions: H_wiel
```

## Motif Classes

| Class | Code | Description | Dutch Example |
|---|---|---|---|
| Actions | `a` | Entity does something | "Emil **belt**." → `a_bellen` |
| Treatments | `t` | Something is done to entity | "Jan **belt** Emil." → `t_bellen` |
| Characterizations | `be` | Characteristics of entity | "Emil is **vriendelijk**." → `be_vriendelijk` |
| Possessions | `H` | Things entity possesses | "Emils **huis**" → `H_huis` |
| Agent-Treatments | `At` | Who does what to entity | "**Jan** belt Emil." → `At_Jan_bellen` |
| Action-Patients | `aP` | Entity does what to whom | "Emil belt **Jan**." → `aP_bellen_Jan` |

## Function Parameters

The main function `extract_motifs()` accepts the same parameters as the original `semgram`, with Dutch-specific defaults:

- `tokens`: Parsed tokens data.frame (UD-style dependencies from Dutch spaCy)
- `entities`: Entity/entities to extract motifs for (default: `"*"` = all)
- `motif_classes`: Which motif classes to extract (default: all six)
- `extract`: Use `"lemma"` (default) or `"token"` for motif representation
- `markup`: Add collapsed markup strings (default: `FALSE`)
- `fast`: Skip more specific rules for speed (default: `FALSE`)
- `be_entity`: Extract reverse copular characterizations (default: `TRUE`)
- `get_aux_verbs`: Treat auxiliary verbs as actions (default: `FALSE`)
- `pron_as_ap`: Allow pronouns as agents/patients (default: `FALSE`)
- `use_appos`: Treat appositional modifiers as entity equivalents (default: `TRUE`)
- `lowercase`: Lowercase all tokens (default: `FALSE`)
- `verbose`: Print progress messages (default: `FALSE`)

## Adaptation Details

### Rules Mapping

Each extraction rule from the original `semgram` has been adapted:

| Rule Category | # Rules | Key Dutch Adaptations |
|---|---|---|
| Actions (`a_1`–`a_7`) | 7 | `obl:agent` for door-passives; UD label mappings |
| Treatments (`t_1`–`t_7`) | 7 | `obj`/`iobj` instead of `dobj`/`dative`; `nsubj:pass` |
| Characterizations (`be_1`–`be_8`) | 8 | Copula as `cop` child (not head); UD predicate-headed structure |
| Possessions (`H_1`–`H_6`) | 6 | `nmod:poss`; `van`+`nmod`+`case`; `hebben` lemma |
| Agent-Treatments (`At_1`–`At_5`) | 5 | `obl:agent` for door-agents; UD object labels |
| Action-Patients (`aP_1`–`aP_5`) | 5 | UD passive/object labels; `obl:agent` for entity |

### Recommended spaCy Models

For best results with Dutch text, use:
- `nl_core_news_lg` (best accuracy, 560MB)
- `nl_core_news_md` (good balance, 40MB)
- `nl_core_news_sm` (fastest, 12MB)

All models are trained on UD Dutch Alpino and LassySmall corpora and produce UD-style dependency labels.

## Attribution

This package is adapted from [semgram](https://github.com/omstuhler/semgram) by Oscar Stuhler. If you use `semgram.nl` in your research, please cite both the original package and this adaptation:

> Stuhler, Oscar (2022). "Who does What to Whom? Making Text Parsers Work for Sociological Inquiry." *Sociological Methods & Research*. [doi: 10.1177/00491241221099551](https://journals.sagepub.com/doi/full/10.1177/00491241221099551).

## License

GPL-3 (same as the original `semgram`)
