#' Extract semantic motifs from parsed Dutch text object
#'
#' This function extracts semantic motifs from Dutch text. The input is a data.frame
#' representing a parsed text such as those returned by \code{spacyr::spacy_parse()}
#' using a Dutch language model (e.g., nl_core_news_sm, nl_core_news_md, nl_core_news_lg).
#' The output is a list of data.frames containing semantic motifs such as actions or
#' characterizations of textual entities.
#'
#' This is a Dutch adaptation of the \code{semgram} package by Stuhler (2022).
#' All extraction rules have been adapted to work with Universal Dependencies
#' labels as used by spaCy's Dutch models (trained on UD Alpino/LassySmall).
#'
#' @param tokens A tokens data.frame with predicted dependencies as generated,
#'   for instance, by \code{spacyr::spacy_parse()} with a Dutch language model.
#'   Dependencies should be in Universal Dependencies style, which is the tag set
#'   used by Dutch language models in spaCy.
#' @param entities Specifies the core entities around which to extract motifs.
#'   This can be a single character string or a vector of character strings.
#'   By default, multi-token strings such as "Harry Potter" will be parsed and
#'   considered. Note that this parameter is case-sensitive. It defaults to "*"
#'   in which case any token is treated as a potential entity.
#' @param motif_classes A character vector specifying which motif classes should
#'   be considered. This can include "t" for treatments, "a" for actions, "be"
#'   for characterizations, "H" for possessions, as well as "At" and "aP" for
#'   agent-treatment and action-patient motifs respectively.
#' @param custom_cols Generally, the columns in the tokens object should be labeled
#'   as follows: "doc_id", "sentence_id", "token_id", "token", "lemma", "pos",
#'   "head_token_id", "dep_rel". If different, provide the matching column names.
#' @param fast If set to true, some of the more specific extraction rules are not
#'   applied. Defaults to FALSE.
#' @param parse_multi_token_entities Should multi-token entities be considered?
#'   Defaults to TRUE.
#' @param extract Defines whether extracted motifs are represented in "lemma" or
#'   "token" form. Defaults to "lemma".
#' @param markup If TRUE, motifs will also be provided as collapsed markup tokens.
#'   Defaults to FALSE.
#' @param add_sentence If TRUE, the sentence for each motif is added. Defaults to FALSE.
#' @param be_entity Should things that are linked to an entity via "being" be
#'   considered as characterization motifs? Defaults to TRUE.
#' @param get_aux_verbs Should auxiliary verbs be considered actions? Defaults to FALSE.
#' @param aux_verb_markup Should auxiliary verbs with "te" be marked up so that
#'   "gaan" in "gaan eten" becomes "gaan-te"? Defaults to TRUE. Note: uses Dutch
#'   infinitive marker "te" instead of English "to".
#' @param pron_as_ap Should pronouns be considered agents and patients? Defaults to FALSE.
#' @param use_appos Should things linked to an entity via an appositional modifier
#'   be considered as equivalent to the entity? Defaults to TRUE.
#' @param lowercase Should all tokens and lemmas be lowercased? Defaults to FALSE.
#' @param verbose Should progress be reported during execution? Defaults to FALSE.
#'
#' @return A list with six dataframes, one for each motif class.
#'
#' @references
#' Stuhler, O. (2022) "Who Does What To Whom? Making Text Parsers Work for
#' Sociological Inquiry." Sociological Methods and Research.
#' <doi: 10.1177/00491241221099551>.
#'
#' @examples
#' # Given data.frame with parsed Dutch sentence
#' tokens_df = data.frame(
#'   doc_id = rep("text1", 5),
#'   sentence_id = rep(1, 5),
#'   token_id = 1:5,
#'   token = c("Emil", "achtervolgde", "de", "dief", "."),
#'   lemma = c("Emil", "achtervolgen", "de", "dief", "."),
#'   pos = c("PROPN", "VERB", "DET", "NOUN", "PUNCT"),
#'   head_token_id = c(2, 2, 4, 2, 2),
#'   dep_rel = c("nsubj", "ROOT", "det", "obj", "punct")
#' )
#'
#' # Extract motifs around specific entities
#' extract_motifs(tokens = tokens_df, entities = c("Emil"))
#'
#' @export
#' @importFrom data.table data.table rbindlist
#' @importFrom rsyntax annotate_tqueries cast_text children NOT OR not_children parents tquery
extract_motifs = function(tokens,
                          entities = "*",
                          motif_classes = c("t", "a", "be", "H", "At", "aP"),
                          custom_cols,
                          fast = F,
                          parse_multi_token_entities = T,
                          extract = "lemma",
                          markup = F,
                          add_sentence = F,
                          be_entity = T,
                          get_aux_verbs = F,
                          aux_verb_markup = T,
                          pron_as_ap = F,
                          use_appos = T,
                          lowercase = F,
                          verbose = F){

  # --------------------------------------------------------------------------
  # Validate input
  # --------------------------------------------------------------------------

  if (!is.data.frame(tokens)) stop("tokens must be a data.frame")

  # Rename columns if custom_cols provided
  expected_cols = c("doc_id", "sentence_id", "token_id", "token", "lemma",
                    "pos", "head_token_id", "dep_rel")
  if (!missing(custom_cols)){
    if (length(custom_cols) != 8) stop("custom_cols must have 8 elements")
    for (i in seq_along(custom_cols)){
      if (custom_cols[i] %in% colnames(tokens)){
        colnames(tokens)[colnames(tokens) == custom_cols[i]] = expected_cols[i]
      }
    }
  }

  for (col in expected_cols){
    if (!col %in% colnames(tokens)) stop(paste0("Column '", col, "' not found in tokens"))
  }

  # Convert to data.table
  tokens = data.table::as.data.table(tokens)

  # Lowercase if requested
  if (lowercase){
    tokens$token = tolower(tokens$token)
    tokens$lemma = tolower(tokens$lemma)
  }

  # --------------------------------------------------------------------------
  # Define POS sets
  # --------------------------------------------------------------------------

  # Verb POS tags
  if (get_aux_verbs){
    verb_pos = c("VERB", "AUX")
  } else {
    verb_pos = "VERB"
  }

  # Agent/Patient POS tags
  if (pron_as_ap){
    agent_patient_pos = c("NOUN", "PROPN", "PRON")
  } else {
    agent_patient_pos = c("NOUN", "PROPN")
  }

  # --------------------------------------------------------------------------
  # Handle multi-token entities
  # --------------------------------------------------------------------------

  if (parse_multi_token_entities && any(grepl(" ", entities))){
    multi_ents = entities[grepl(" ", entities)]
    single_ents = entities[!grepl(" ", entities)]

    # For multi-token entities, we use the last token as anchor
    # and add compound/flat relations
    for (me in multi_ents){
      me_tokens = strsplit(me, " ")[[1]]
      single_ents = c(single_ents, me_tokens)
    }
    entities = unique(single_ents)
  }

  # Handle wildcard entity
  if (length(entities) == 1 && entities == "*"){
    entities = unique(tokens$token)
  }

  # --------------------------------------------------------------------------
  # Handle appositional modifiers
  # --------------------------------------------------------------------------
  # If use_appos = TRUE, we expand the entity list to include tokens that are
  # linked to known entities via appositional modifier relations ('appos').
  # E.g., in "Mijn broer Emil won", if entity = "Emil", we also consider
  # "broer" as equivalent to the entity for extraction purposes.

  if (use_appos){
    appos_tokens = tokens[tokens$dep_rel == "appos", ]
    for (ent in entities) {
      # Find heads of appos relations where the entity is the appos modifier
      appos_heads = appos_tokens[appos_tokens$token == ent, "head_token_id", drop = FALSE]
      if (nrow(appos_heads) > 0) {
        for (j in 1:nrow(appos_heads)) {
          head_id = appos_heads$head_token_id[j]
          # Find what token this head_id corresponds to (within same doc/sentence)
          head_row = appos_tokens[appos_tokens$token == ent, ]
          if (nrow(head_row) > 0) {
            d_id = head_row$doc_id[j]
            s_id = head_row$sentence_id[j]
            head_tok = tokens[tokens$doc_id == d_id &
                              tokens$sentence_id == s_id &
                              tokens$token_id == head_id, "token"]
            if (nrow(head_tok) > 0) {
              entities = unique(c(entities, head_tok$token))
            }
          }
        }
      }
    }
  }

  # --------------------------------------------------------------------------
  # Apply extraction rules
  # --------------------------------------------------------------------------

  # Initialize output
  actions_list = list()
  treatments_list = list()
  characterizations_list = list()
  possessions_list = list()
  agent_treatments_list = list()
  action_patients_list = list()

  # --- ACTIONS ---
  if ("a" %in% motif_classes){
    if (verbose) cat("Extracting action motifs...\n")

    actions_list[[1]] = a_1(tokens, entities, verb_pos, agent_patient_pos, extract)
    actions_list[[2]] = a_2(tokens, entities, verb_pos, agent_patient_pos, extract)
    actions_list[[3]] = a_3(tokens, entities, verb_pos, agent_patient_pos, extract)
    actions_list[[4]] = a_4(tokens, entities, verb_pos, agent_patient_pos, extract)

    if (!fast){
      actions_list[[5]] = a_5(tokens, entities, verb_pos, agent_patient_pos, extract)
      actions_list[[6]] = a_6(tokens, entities, verb_pos, agent_patient_pos, extract)
      actions_list[[7]] = a_7(tokens, entities, verb_pos, agent_patient_pos, extract)
    }
  }

  # --- TREATMENTS ---
  if ("t" %in% motif_classes){
    if (verbose) cat("Extracting treatment motifs...\n")

    treatments_list[[1]] = t_1(tokens, entities, verb_pos, agent_patient_pos, extract)
    treatments_list[[2]] = t_2(tokens, entities, verb_pos, agent_patient_pos, extract)
    treatments_list[[3]] = t_3(tokens, entities, verb_pos, agent_patient_pos, extract)
    treatments_list[[4]] = t_4(tokens, entities, verb_pos, agent_patient_pos, extract)

    if (!fast){
      treatments_list[[5]] = t_5(tokens, entities, verb_pos, agent_patient_pos, extract)
      treatments_list[[6]] = t_6(tokens, entities, verb_pos, agent_patient_pos, extract)
      treatments_list[[7]] = t_7(tokens, entities, verb_pos, agent_patient_pos, extract)
    }
  }

  # --- CHARACTERIZATIONS ---
  if ("be" %in% motif_classes){
    if (verbose) cat("Extracting characterization motifs...\n")

    characterizations_list[[1]] = be_1(tokens, entities, verb_pos, agent_patient_pos, extract)
    characterizations_list[[2]] = be_2(tokens, entities, verb_pos, agent_patient_pos, extract)
    characterizations_list[[3]] = be_3(tokens, entities, verb_pos, agent_patient_pos, extract)
    characterizations_list[[4]] = be_4(tokens, entities, verb_pos, agent_patient_pos, extract)
    characterizations_list[[5]] = be_5(tokens, entities, verb_pos, agent_patient_pos, extract)

    if (!fast){
      characterizations_list[[6]] = be_6(tokens, entities, verb_pos, agent_patient_pos, extract)
      if (be_entity){
        characterizations_list[[7]] = be_7(tokens, entities, verb_pos, agent_patient_pos, extract)
      }
      characterizations_list[[8]] = be_8(tokens, entities, verb_pos, agent_patient_pos, extract)
    }
  }

  # --- POSSESSIONS ---
  if ("H" %in% motif_classes){
    if (verbose) cat("Extracting possession motifs...\n")

    possessions_list[[1]] = H_1(tokens, entities, verb_pos, agent_patient_pos, extract)
    possessions_list[[2]] = H_2(tokens, entities, verb_pos, agent_patient_pos, extract)
    possessions_list[[3]] = H_3(tokens, entities, verb_pos, agent_patient_pos, extract)

    if (!fast){
      possessions_list[[4]] = H_4(tokens, entities, verb_pos, agent_patient_pos, extract)
      possessions_list[[5]] = H_5(tokens, entities, verb_pos, agent_patient_pos, extract)
      possessions_list[[6]] = H_6(tokens, entities, verb_pos, agent_patient_pos, extract)
    }
  }

  # --- AGENT-TREATMENTS ---
  if ("At" %in% motif_classes){
    if (verbose) cat("Extracting agent-treatment motifs...\n")

    agent_treatments_list[[1]] = At_1(tokens, entities, verb_pos, agent_patient_pos, extract)
    agent_treatments_list[[2]] = At_2(tokens, entities, verb_pos, agent_patient_pos, extract)
    agent_treatments_list[[3]] = At_3(tokens, entities, verb_pos, agent_patient_pos, extract)

    if (!fast){
      agent_treatments_list[[4]] = At_4(tokens, entities, verb_pos, agent_patient_pos, extract)
      agent_treatments_list[[5]] = At_5(tokens, entities, verb_pos, agent_patient_pos, extract)
    }
  }

  # --- ACTION-PATIENTS ---
  if ("aP" %in% motif_classes){
    if (verbose) cat("Extracting action-patient motifs...\n")

    action_patients_list[[1]] = aP_1(tokens, entities, verb_pos, agent_patient_pos, extract)
    action_patients_list[[2]] = aP_2(tokens, entities, verb_pos, agent_patient_pos, extract)
    action_patients_list[[3]] = aP_3(tokens, entities, verb_pos, agent_patient_pos, extract)

    if (!fast){
      action_patients_list[[4]] = aP_4(tokens, entities, verb_pos, agent_patient_pos, extract)
      action_patients_list[[5]] = aP_5(tokens, entities, verb_pos, agent_patient_pos, extract)
    }
  }

  # --------------------------------------------------------------------------
  # Combine results
  # --------------------------------------------------------------------------

  combine_motifs = function(motif_list, motif_type){
    if (length(motif_list) == 0) return(data.table())
    combined = rbindlist(motif_list, fill = T)
    if (nrow(combined) == 0) return(data.table())
    # Remove duplicates
    combined = unique(combined)
    return(combined)
  }

  actions = combine_motifs(actions_list, "action")
  treatments = combine_motifs(treatments_list, "treatment")
  characterizations = combine_motifs(characterizations_list, "characterization")
  possessions = combine_motifs(possessions_list, "possession")
  agent_treatments = combine_motifs(agent_treatments_list, "agent_treatment")
  action_patients = combine_motifs(action_patients_list, "action_patient")

  # --------------------------------------------------------------------------
  # Add markup if requested
  # --------------------------------------------------------------------------

  if (markup){
    if (nrow(actions) > 0){
      actions$markup = paste0("a_", actions$action)
    }
    if (nrow(treatments) > 0){
      treatments$markup = paste0("t_", treatments$treatment)
    }
    if (nrow(characterizations) > 0){
      characterizations$markup = paste0("be_", characterizations$characterization)
    }
    if (nrow(possessions) > 0){
      possessions$markup = paste0("H_", possessions$Possession)
    }
    if (nrow(agent_treatments) > 0){
      agent_treatments$markup = paste0("At_", agent_treatments$Agent, "_", agent_treatments$treatment)
    }
    if (nrow(action_patients) > 0){
      action_patients$markup = paste0("aP_", action_patients$action, "_", action_patients$Patient)
    }
  }

  # --------------------------------------------------------------------------
  # Add sentence if requested
  # --------------------------------------------------------------------------

  if (add_sentence){
    add_sent = function(motif_df){
      if (nrow(motif_df) == 0) return(motif_df)
      if (!"ann_id" %in% names(motif_df)) return(motif_df)

      motif_df$sentence = NA_character_
      for (i in 1:nrow(motif_df)){
        ann = motif_df$ann_id[i]
        parts = strsplit(as.character(ann), "\\.")[[1]]
        if (length(parts) >= 2){
          d_id = parts[1]
          s_id = as.integer(parts[2])
          sent_tokens = tokens[tokens$doc_id == d_id & tokens$sentence_id == s_id,]
          motif_df$sentence[i] = paste(sent_tokens$token, collapse = " ")
        }
      }
      return(motif_df)
    }

    actions = add_sent(actions)
    treatments = add_sent(treatments)
    characterizations = add_sent(characterizations)
    possessions = add_sent(possessions)
    agent_treatments = add_sent(agent_treatments)
    action_patients = add_sent(action_patients)
  }

  # --------------------------------------------------------------------------
  # Return results
  # --------------------------------------------------------------------------

  result = list(
    actions = actions,
    treatments = treatments,
    characterizations = characterizations,
    possessions = possessions,
    agent_treatments = agent_treatments,
    action_patients = action_patients
  )

  return(result)
}
