#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
NULL

###############################################################################################
##### Rule t_1: Entity as direct object (obj) of a verb
##### Example: "Jan belt Emil." (belt -> bellen)
##### Dutch adaptation: 'dobj' -> 'obj' in UD Dutch

t_1 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(pos = verb_pos,
                label = "treatment", fill = F,
                children(token = entities,
                         relation = c("obj", "iobj"),
                         label = "Entity", fill = F)
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), treatment = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

###############################################################################################
##### Rule t_2: Entity as conjunct of direct object
##### Example: "Jan belt Peter en Emil." (belt -> bellen)

t_2 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(token = entities,
                relation = "conj",
                label = "Entity", fill = F,
                parents(pos = agent_patient_pos,
                        relation = c("obj", "iobj"),
                        parents(pos = verb_pos,
                                label = "treatment", fill = F))
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), treatment = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

###############################################################################################
##### Rule t_3: Entity as passive subject (nsubj:pass)
##### Example: "Emil werd gebeld." (gebeld -> bellen)
##### Dutch adaptation: 'nsubjpass' -> 'nsubj:pass' in UD Dutch

t_3 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(pos = verb_pos,
                label = "treatment", fill = F,
                children(token = entities,
                         relation = "nsubj:pass",
                         label = "Entity", fill = F)
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), treatment = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

###############################################################################################
##### Rule t_4: Entity as conjunct of passive subject
##### Example: "Peter en Emil werden gebeld." (gebeld -> bellen)

t_4 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(token = entities,
                relation = "conj",
                label = "Entity", fill = F,
                parents(pos = c("NOUN", "PROPN", "PRON"),
                        relation = "nsubj:pass",
                        parents(pos = verb_pos,
                                label = "treatment", fill = F))
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), treatment = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

###############################################################################################
##### Rule t_5: Entity as object of xcomp verb chain
##### Example: "Jan wil Emil vragen." (vragen)

t_5 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(pos = verb_pos, relation = "xcomp",
                label = "treatment", fill = F,
                children(token = entities,
                         relation = c("obj", "iobj"),
                         label = "Entity", fill = F)
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), treatment = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

###############################################################################################
##### Rule t_6: Entity as object of conjoined verb
##### Example: "Jan kwam en vroeg Emil." (vroeg -> vragen)

t_6 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(pos = verb_pos, relation = "conj",
                label = "treatment", fill = F,
                children(token = entities,
                         relation = c("obj", "iobj"),
                         label = "Entity", fill = F)
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), treatment = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

###############################################################################################
##### Rule t_7: Entity as conjunct of object of conjoined verb
##### Example: "Jan kwam en vroeg Peter en Emil." (vroeg -> vragen)

t_7 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(token = entities,
                relation = "conj",
                label = "Entity", fill = F,
                parents(pos = agent_patient_pos,
                        relation = c("obj", "iobj"),
                        parents(pos = verb_pos, relation = "conj",
                                label = "treatment", fill = F))
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), treatment = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}
