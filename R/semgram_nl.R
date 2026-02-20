#' semgram.nl: Extracting Semantic Motifs from Dutch Textual Data
#'
#' 'semgram.nl' extracts semantic motifs from Dutch textual data. It is a
#' Dutch language adaptation of the 'semgram' package by Oscar Stuhler.
#' It builds on an entity-centered semantic grammar that distinguishes six
#' classes of motifs: actions of an entity, treatments of an entity, agents
#' acting upon an entity, patients acted upon by an entity, characterizations
#' of an entity, and possessions of an entity.
#'
#' This package adapts all extraction rules from the original English-language
#' 'semgram' to work with Dutch dependency labels (Universal Dependencies
#' style as used by spaCy's Dutch models trained on UD Alpino/LassySmall),
#' Dutch-specific syntactic constructions, and Dutch lemmas.
#'
#' @docType package
#' @name semgram.nl
NULL
