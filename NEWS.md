# semgram.nl 0.2.0

## Dutch-specific augmentations

* **Separable verb reconstruction** (`merge_separable_verbs = TRUE`): Merges `compound:prt` particles back into verb lemmas before extraction. "bellen" + "op" → "opbellen". This prevents semantic fragmentation of Dutch separable verbs, which is the single most important Dutch-specific issue for accurate motif extraction.
* **Reflexive pronoun handling** (`reflexive_as_patient = FALSE`): New option to treat true reflexive objects ("zichzelf", "mezelf", etc.) as valid patients. Only applies to tokens with `obj`/`iobj` relations, not inherent reflexives (`expl:pv` like "zich").
* **Appositional modifier handling** (`use_appos = TRUE`): Restored original semgram's `appos_child` column mechanism. Appos-linked tokens are marked sentence-locally via a column flag, rather than expanding the global entity list. Rules use `OR(token = entities, appos_child = "appos_child")` for matching.
* **Known limitations documented**: R-pronouns, stative/eventive passive distinction, coreference resolution.

# semgram.nl 0.1.0

## Initial release

* Dutch language adaptation of semgram 0.1.1
* All extraction rules adapted for Universal Dependencies labels (UD Alpino/LassySmall)
* Key dependency label mappings:
  - `dobj` → `obj` (direct object)
  - `dative` → `iobj` (indirect object) 
  - `nsubjpass` → `nsubj:pass` (passive nominal subject)
  - `agent`/`pobj` → `obl:agent` (agent in passive with "door")
  - `poss` → `nmod:poss` (possessive modifier)
* Copular constructions adapted for UD predicate-headed structure (adjective/noun as head, copula as `cop` child)
* Dutch lemmas: "hebben" (have), "van" (of), "door" (by), "te" (to)
* Support for Dutch spaCy models: nl_core_news_sm, nl_core_news_md, nl_core_news_lg
