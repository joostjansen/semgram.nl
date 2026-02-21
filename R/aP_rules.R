#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
NULL

###############################################################################################
##### Rule aP_1: Patient as object of entity's action verb
##### Example: "Emil vraagt Jan." (aP_vragen_Jan)
##### Dutch adaptation: 'dobj' -> 'obj', 'dative' -> 'iobj'

aP_1 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(pos = verb_pos,
                label = "action", fill = F,
                children(OR(token = entities, appos_child = "appos_child"),
                         relation = "nsubj",
                         label = "Entity", fill = F),
                children(pos = agent_patient_pos,
                         relation = c("obj", "iobj"),
                         label = "Patient", fill = F,
                         children(pos = agent_patient_pos,
                                  relation = c("conj", "appos"),
                                  label = "Patient", req = F, depth = 3, fill = F))
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character(), Patient = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

###############################################################################################
##### Rule aP_2: Patient as object, entity as conj of subject

aP_2 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"),
                relation = "conj",
                label = "Entity", fill = F,
                parents(pos = c("NOUN", "PROPN", "PRON"),
                        relation = "nsubj",
                        parents(pos = verb_pos,
                                label = "action", fill = F,
                                children(pos = agent_patient_pos,
                                         relation = c("obj", "iobj"),
                                         label = "Patient", fill = F,
                                         children(pos = agent_patient_pos,
                                                  relation = c("conj", "appos"),
                                                  label = "Patient", req = F, depth = 3, fill = F))))
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character(), Patient = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

###############################################################################################
##### Rule aP_3: Patient as passive subject, entity in obl:agent (door-phrase)
##### Example: "Jan wordt gevraagd door Emil." (aP_vragen_Jan)
##### Dutch adaptation: obl:agent for the entity, nsubj:pass for the patient

aP_3 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(pos = verb_pos,
                label = "action", fill = F,
                children(OR(token = entities, appos_child = "appos_child"),
                         relation = "obl:agent",
                         label = "Entity", fill = F),
                children(pos = c("NOUN", "PROPN", "PRON"),
                         relation = "nsubj:pass",
                         label = "Patient", fill = F,
                         children(pos = c("NOUN", "PROPN", "PRON"),
                                  relation = c("conj", "appos"),
                                  label = "Patient", req = F, depth = 3, fill = F))
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character(), Patient = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

###############################################################################################
##### Rule aP_4: Patient as object of xcomp verb chain with entity as subject

aP_4 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(pos = verb_pos,
                children(OR(token = entities, appos_child = "appos_child"),
                         relation = "nsubj",
                         label = "Entity", fill = F),
                children(pos = verb_pos, relation = "xcomp",
                         label = "action", fill = F,
                         not_children(relation = "nsubj", depth = 1),
                         children(pos = agent_patient_pos,
                                  relation = c("obj", "iobj"),
                                  label = "Patient", fill = F,
                                  children(pos = agent_patient_pos,
                                           relation = c("conj", "appos"),
                                           label = "Patient", req = F, depth = 3, fill = F)))
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character(), Patient = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

###############################################################################################
##### Rule aP_5: Patient as object of conjoined verb, entity as subject

aP_5 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(pos = verb_pos, relation = "conj",
                label = "action", fill = F,
                not_children(relation = "nsubj", depth = 1),
                children(pos = agent_patient_pos,
                         relation = c("obj", "iobj"),
                         label = "Patient", fill = F,
                         children(pos = agent_patient_pos,
                                  relation = c("conj", "appos"),
                                  label = "Patient", req = F, depth = 3, fill = F)),
                parents(pos = verb_pos,
                        children(OR(token = entities, appos_child = "appos_child"),
                                 relation = "nsubj",
                                 label = "Entity", fill = F))
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character(), Patient = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}
