#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
NULL

###############################################################################################
##### Rule H_1: Entity as possessive modifier (nmod:poss)
##### Example: "Emils vrouw, vrienden en ouders waren geschokt." (H_vrouw, H_vriend, H_ouder)
##### Dutch adaptation: 'poss' -> 'nmod:poss' in UD Dutch

H_1 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(pos = agent_patient_pos,
                label = "Possession", fill = F,
                children(OR(token = entities, appos_child = "appos_child"),
                         relation = "nmod:poss",
                         label = "Entity", fill = F),
                children(pos = agent_patient_pos, relation = c("conj", "appos"),
                         label = "Possession", req = F, depth = 3, fill = F)
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), Possession = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

###############################################################################################
##### Rule H_2: Entity as object of "van" (of) construction
##### Example: "De remmen en wielen van de Emil waren oud." (H_rem, H_wiel)
##### Dutch adaptation: In UD Dutch, "van" is a case marker.
#####   The pattern is: noun --nmod--> entity, with "van" as case child of entity

H_2 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(pos = agent_patient_pos,
                label = "Possession", fill = F,
                children(OR(token = entities, appos_child = "appos_child"),
                         relation = "nmod",
                         label = "Entity", fill = F,
                         children(pos = "ADP", lemma = "van", relation = "case")),
                children(pos = agent_patient_pos, relation = c("conj", "appos"),
                         label = "Possession", req = F, depth = 3, fill = F)
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), Possession = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

###############################################################################################
##### Rule H_3: Entity as subject of "hebben" (have) verb
##### Example: "Emil heeft vrienden en vijanden." (H_vriend, H_vijand)
##### Dutch adaptation: lemma "have" -> "hebben"

H_3 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(pos = c("VERB", "AUX"), lemma = "hebben",
                children(OR(token = entities, appos_child = "appos_child"),
                         relation = "nsubj",
                         label = "Entity", fill = F),
                children(pos = agent_patient_pos,
                         relation = c("obj", "iobj"),
                         label = "Possession", fill = F,
                         children(pos = agent_patient_pos,
                                  relation = c("conj", "appos"),
                                  label = "Possession", req = F, depth = 3, fill = F))
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), Possession = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

###############################################################################################
##### Rule H_4: Entity as conjunct of subject of "hebben"
##### Example: "Jan en Emil hebben een huis." (H_huis)

H_4 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"),
                relation = "conj",
                label = "Entity", fill = F,
                parents(pos = c("NOUN", "PROPN", "PRON"),
                        relation = "nsubj",
                        parents(pos = c("VERB", "AUX"), lemma = "hebben",
                                children(pos = agent_patient_pos,
                                         relation = c("obj", "iobj"),
                                         label = "Possession", fill = F,
                                         children(pos = agent_patient_pos,
                                                  relation = c("conj", "appos"),
                                                  label = "Possession", req = F, depth = 3, fill = F))))
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), Possession = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

###############################################################################################
##### Rule H_5: Entity as subject with "hebben" in xcomp chain
##### Example: "Emil schijnt een auto te hebben." (H_auto)

H_5 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(pos = c("VERB", "AUX"),
                children(OR(token = entities, appos_child = "appos_child"),
                         relation = "nsubj",
                         label = "Entity", fill = F),
                children(pos = c("VERB", "AUX"), relation = "xcomp",
                         lemma = "hebben",
                         not_children(relation = "nsubj", depth = 1),
                         children(pos = agent_patient_pos,
                                  relation = c("obj", "iobj"),
                                  label = "Possession", fill = F,
                                  children(pos = agent_patient_pos,
                                           relation = c("conj", "appos"),
                                           label = "Possession", req = F, depth = 3, fill = F)))
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), Possession = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}

###############################################################################################
##### Rule H_6: Entity as conjunct of subject with "hebben" in conjoined verb
##### Example: "Jan en Emil kwamen en hadden een taart." (H_taart)

H_6 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"),
                relation = "conj",
                label = "Entity", fill = F,
                parents(pos = c("NOUN", "PROPN", "PRON"),
                        relation = "nsubj",
                        parents(pos = c("VERB", "AUX"),
                                children(pos = c("VERB", "AUX"),
                                         relation = "conj",
                                         lemma = "hebben",
                                         not_children(relation = "nsubj", depth = 1),
                                         children(pos = agent_patient_pos,
                                                  relation = c("obj", "iobj"),
                                                  label = "Possession", fill = F,
                                                  children(pos = agent_patient_pos,
                                                           relation = c("conj", "appos"),
                                                           label = "Possession", req = F, depth = 3, fill = F)))))
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), Possession = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}
