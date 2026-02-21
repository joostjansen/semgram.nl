#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
NULL

###############################################################################################
##### Rule At_1: Agent as subject of treatment verb (entity as obj)
##### Example: "Jan belt Emil." (At_Jan_bellen)
##### Dutch adaptation: 'dobj' -> 'obj', 'dative' -> 'iobj'

At_1 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(pos = verb_pos,
                label = "treatment", fill = F,
                children(OR(token = entities, appos_child = "appos_child"),
                         relation = c("obj", "iobj"),
                         label = "Entity", fill = F),
                children(pos = c("NOUN", "PROPN", "PRON"),
                         relation = "nsubj",
                         label = "Agent", fill = F,
                         children(pos = c("NOUN", "PROPN", "PRON"),
                                  relation = c("conj", "appos"),
                                  label = "Agent", req = F, depth = 3, fill = F))
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), Agent = character(), treatment = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

###############################################################################################
##### Rule At_2: Agent as subject of treatment verb (entity as conj of obj)

At_2 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"),
                relation = "conj",
                label = "Entity", fill = F,
                parents(pos = agent_patient_pos,
                        relation = c("obj", "iobj"),
                        parents(pos = verb_pos,
                                label = "treatment", fill = F,
                                children(pos = c("NOUN", "PROPN", "PRON"),
                                         relation = "nsubj",
                                         label = "Agent", fill = F,
                                         children(pos = c("NOUN", "PROPN", "PRON"),
                                                  relation = c("conj", "appos"),
                                                  label = "Agent", req = F, depth = 3, fill = F))))
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), Agent = character(), treatment = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

###############################################################################################
##### Rule At_3: Agent in passive 'door' phrase (entity as nsubj:pass)
##### Example: "Emil wordt gevraagd door Jan." (At_Jan_vragen)
##### Dutch adaptation: obl:agent for the agent in door-phrase

At_3 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(pos = verb_pos,
                label = "treatment", fill = F,
                children(OR(token = entities, appos_child = "appos_child"),
                         relation = "nsubj:pass",
                         label = "Entity", fill = F),
                children(pos = c("NOUN", "PROPN", "PRON"),
                         relation = "obl:agent",
                         label = "Agent", fill = F,
                         children(pos = c("NOUN", "PROPN", "PRON"),
                                  relation = c("conj", "appos"),
                                  label = "Agent", req = F, depth = 3, fill = F))
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), Agent = character(), treatment = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

###############################################################################################
##### Rule At_4: Agent as subject of xcomp chain treating entity

At_4 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(pos = verb_pos,
                children(pos = c("NOUN", "PROPN", "PRON"),
                         relation = "nsubj",
                         label = "Agent", fill = F,
                         children(pos = c("NOUN", "PROPN", "PRON"),
                                  relation = c("conj", "appos"),
                                  label = "Agent", req = F, depth = 3, fill = F)),
                children(pos = verb_pos, relation = "xcomp",
                         label = "treatment", fill = F,
                         not_children(relation = "nsubj", depth = 1),
                         children(OR(token = entities, appos_child = "appos_child"),
                                  relation = c("obj", "iobj"),
                                  label = "Entity", fill = F))
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), Agent = character(), treatment = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

###############################################################################################
##### Rule At_5: Agent as subject of conjoined treatment verb

At_5 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(pos = verb_pos, relation = "conj",
                label = "treatment", fill = F,
                not_children(relation = "nsubj", depth = 1),
                children(OR(token = entities, appos_child = "appos_child"),
                         relation = c("obj", "iobj"),
                         label = "Entity", fill = F),
                parents(pos = verb_pos,
                        children(pos = c("NOUN", "PROPN", "PRON"),
                                 relation = "nsubj",
                                 label = "Agent", fill = F,
                                 children(pos = c("NOUN", "PROPN", "PRON"),
                                          relation = c("conj", "appos"),
                                          label = "Agent", req = F, depth = 3, fill = F)))
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), Agent = character(), treatment = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}
