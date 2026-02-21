#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
NULL

###############################################################################################
##### Rule be_1: Entity as subject of copular verb with adjectival predicate
##### Example: "Emil is vriendelijk." (be_vriendelijk)
##### Dutch adaptation: In UD Dutch, copulas (zijn, worden, lijken, etc.) are
#####   marked with 'cop' relation. The adjective is the root, not the copula.
#####   So the entity is nsubj of the adjective, and the adjective has a cop child.

be_1 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(pos = c("ADJ"),
                label = "characterization", fill = F,
                children(relation = "cop"),
                children(OR(token = entities, appos_child = "appos_child"),
                         relation = "nsubj",
                         label = "Entity", fill = F)
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), characterization = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

###############################################################################################
##### Rule be_2: Entity as subject of copular verb with nominal predicate
##### Example: "Emil is de winnaar." (be_winnaar)
##### Dutch adaptation: Same as be_1 but with nominal predicate (NOUN/PROPN)

be_2 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(pos = c("NOUN", "PROPN"),
                label = "characterization", fill = F,
                children(relation = "cop"),
                children(OR(token = entities, appos_child = "appos_child"),
                         relation = "nsubj",
                         label = "Entity", fill = F)
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), characterization = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

###############################################################################################
##### Rule be_3: Entity as conjunct of subject with copular predicate
##### Example: "Jan en Emil zijn vriendelijk." (be_vriendelijk)

be_3 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"),
                relation = "conj",
                label = "Entity", fill = F,
                parents(pos = c("NOUN", "PROPN", "PRON"),
                        relation = "nsubj",
                        parents(pos = c("ADJ", "NOUN", "PROPN"),
                                label = "characterization", fill = F,
                                children(relation = "cop")))
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), characterization = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

###############################################################################################
##### Rule be_4: Direct adjectival modifier of entity
##### Example: "Jan kocht een goedkope, nieuwe Emil." (be_goedkoop, be_nieuw)
##### Dutch adaptation: 'amod' relation is the same in UD Dutch

be_4 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(pos = "ADJ",
                relation = "amod",
                label = "characterization", fill = F,
                parents(OR(token = entities, appos_child = "appos_child"),
                        label = "Entity", fill = F)
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), characterization = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

###############################################################################################
##### Rule be_5: Entity as appositional modifier (characterization of head)
##### Example: "Mijn broer Emil won." (be_broer)
##### Dutch adaptation: 'appos' relation is the same in UD Dutch

be_5 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"),
                relation = "appos",
                label = "Entity", fill = F,
                parents(pos = c("NOUN", "PROPN"),
                        label = "characterization", fill = F)
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), characterization = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

###############################################################################################
##### Rule be_6: Conjoined copular predicate
##### Example: "Emil is vriendelijk en eerlijk." (be_vriendelijk, be_eerlijk)

be_6 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(pos = c("ADJ", "NOUN", "PROPN"),
                relation = "conj",
                label = "characterization", fill = F,
                parents(pos = c("ADJ", "NOUN", "PROPN"),
                        children(relation = "cop"),
                        children(OR(token = entities, appos_child = "appos_child"),
                                 relation = "nsubj",
                                 label = "Entity", fill = F))
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), characterization = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

###############################################################################################
##### Rule be_7: Nominal subject of copular verb where entity is the predicate
##### (be_entity = TRUE): "De winnaar was Emil." (be_winnaar)

be_7 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"),
                relation = "nsubj",
                label = "Entity", fill = F,
                parents(pos = c("NOUN", "PROPN"),
                        label = "characterization", fill = F,
                        children(relation = "cop"))
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), characterization = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

###############################################################################################
##### Rule be_8: xcomp copular chain
##### Example: "Emil hoopt president te blijven." (be_president)

be_8 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(pos = c("ADJ", "NOUN", "PROPN"),
                relation = "xcomp",
                label = "characterization", fill = F,
                parents(pos = verb_pos,
                        children(OR(token = entities, appos_child = "appos_child"),
                                 relation = "nsubj",
                                 label = "Entity", fill = F))
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), characterization = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}
