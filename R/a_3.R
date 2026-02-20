#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
NULL

###############################################################################################
##### Rule: Passive subject with 'door' (Dutch equivalent of English 'by')
##### Example: "Jan werd gevraagd door Emil." (gevraagd -> vragen)
##### Dutch adaptation: In UD Dutch, the agent in a passive is marked with
#####   obl:agent relation, and the preposition 'door' has relation 'case'.
#####   The entity is the object of the obl:agent phrase.

a_3 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  # UD Dutch style: entity --obl:agent--> verb, with 'door' as case child
  rule = tquery(token = entities,
                relation = "obl:agent",
                label = "Entity", fill = F,
                parents(pos = verb_pos,
                        label = "action", fill = F)
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}
