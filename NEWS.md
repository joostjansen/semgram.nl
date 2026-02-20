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
