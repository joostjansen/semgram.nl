#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
NULL

###############################################################################################
##### Rule: Entity as nominal subject (nsubj) of a verb
##### Example: "Emil achtervolgde de dief." (achtervolgde -> achtervolgen)
##### Dutch adaptation: Uses 'nsubj' (same as English, UD standard)

a_1 = function(tokens, entities, verb_pos, agent_patient_pos, extract){
  rule = tquery(pos = verb_pos,
                label = "action", fill = F,
                not_children(relation = "nsubj", pos = verb_pos),
                children(OR(token = entities, appos_child = "appos_child"),
                         relation = "nsubj",
                         label = "Entity", fill = F)
  )

  tokens = annotate_tqueries(tokens, "query", rule, overwrite = T, copy = F)
  if(all(is.na(tokens$query))){
    casted = data.table(doc_id = character(), ann_id = factor(), Entity = character(), action = character())
  } else {
    casted = cast_text(tokens, 'query', text_col = extract)
  }
  return(casted)
}
