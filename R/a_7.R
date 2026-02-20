#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
NULL

###############################################################################################
##### Rule: Entity as conjunct of subject with xcomp chain
##### Example: "Jan en Emil willen vragen." (vragen)

a_7 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(OR(token = entities, appos_child = "appos_child"),
                relation = "conj",
                label = "Entity", fill = F,
                parents(pos = c("NOUN", "PROPN", "PRON"),
                        relation = "nsubj",
                        parents(pos = verb_pos,
                                children(pos = verb_pos, relation = "xcomp",
                                         not_children(relation = "nsubj", depth = 1),
                                         label = "action", fill = F)))
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}
