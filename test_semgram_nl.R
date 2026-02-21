###############################################################################
# test_semgram_nl.R
# Test script for semgram.nl: Dutch semantic motif extraction
#
# This script tests the semgram.nl package on 5 recent NOS.nl articles about
# migration, extracting motifs around migration-related entities.
#
# PREREQUISITES:
#   1. Install semgram.nl:
#      devtools::install_github("joostjansen/semgram.nl")
#
#   2. Install dependencies:
#      install.packages(c("rsyntax", "data.table", "spacyr"))
#
#   3. Set up spaCy with Dutch model (one-time):
#      library(spacyr)
#      spacy_install()
#      spacy_download_langmodel("nl_core_news_lg")
#
# USAGE:
#   source("test_semgram_nl.R")
###############################################################################

library(spacyr)
library(semgram.nl)
library(data.table)

# --------------------------------------------------------------------------
# Initialize spaCy with Dutch model
# --------------------------------------------------------------------------

cat("=== Initializing spaCy with Dutch model ===\n")
spacy_initialize(model = "nl_core_news_lg")

# --------------------------------------------------------------------------
# Define the 5 NOS.nl articles (article text extracted from NOS.nl)
# --------------------------------------------------------------------------

articles <- list(

  # Article 1: EU illegale immigratie gedaald (Jan 2026)
  list(
    id = "nos_2598384",
    title = "EU: illegale immigratie in 2025 met ruim een kwart gedaald",
    date = "2026-01-16",
    text = paste(
      "In 2025 werden bijna 178.000 mensen onderschept die de EU trachtten",
      "binnen te komen zonder papieren, zo'n 26 procent minder dan een jaar eerder.",
      "Het is ook minder dan de helft van het totale aantal in 2023, en het",
      "laagste aantal sinds 2021. In absolute aantallen was de daling het sterkst",
      "aan de grens tussen Turkije en de EU. Daar werden afgelopen jaar ruim",
      "51.000 mensen onderschept. Zij kwamen vooral uit Afghanistan, Sudan en",
      "Egypte. Relatief gezien was de daling het sterkst in de regio West-Afrika.",
      "Daar daalde het aantal bootvluchtelingen met 63 procent. De EU beschuldigt",
      "Belarus en Rusland al jaren ervan migranten daar illegaal de grens over te",
      "zetten, in een poging de EU te frustreren. Een duidelijke reden voor de",
      "daling geeft Frontex niet, al legt het agentschap wel de nadruk op het",
      "belang van migratiedeals met landen net buiten de EU."
    )
  ),

  # Article 2: Buurten veranderen door migratie (Oct 2025)
  list(
    id = "nos_2586818",
    title = "Buurten veranderen door migratie, en dat leidt tot zorgen bij bewoners en politiek",
    date = "2025-10-17",
    text = paste(
      "Het is opnieuw een van de belangrijkste verkiezingsthema's: migratie.",
      "Een omvangrijke en ingewikkelde kwestie met als grootste gevolg dat de",
      "bevolking toeneemt en diverser wordt. Elk jaar komen er meer migranten bij",
      "terwijl de autochtone bevolking juist krimpt. Het Sociaal en Cultureel",
      "Planbureau constateerde in een onderzoek dat de politieke partijen veel",
      "aandacht hebben voor het beperken van migratie, maar minder voor de vraag",
      "hoe we met elkaar omgaan in een diverse samenleving. Mensen maken zich daar",
      "wel veel zorgen over. Dilan Yesilgoz van de VVD noemt het geen goed idee als",
      "mensen erg geclusterd bij elkaar leven in wijken. Jimmy Dijk van de SP is",
      "voorstander van een actief spreidingsbeleid. Lijsttrekker Henri Bontenbal",
      "bepleit hard in te zetten op het leren van Nederlands."
    )
  ),

  # Article 3: Aantal mensen dat migreert opnieuw gedaald (Jul 2025)
  list(
    id = "nos_2573640",
    title = "Aantal mensen dat naar Nederland migreert opnieuw gedaald",
    date = "2025-07-04",
    text = paste(
      "Het aantal mensen dat naar Nederland migreert is in 2024 opnieuw gedaald.",
      "Dat blijkt uit de vijfde editie van de Staat van Migratie waarin het",
      "demissionaire kabinet de ontwikkeling van migratie in kaart heeft laten",
      "brengen. Vorig jaar kwamen er volgens het rapport 314.000 mensen naar",
      "Nederland, 6 procent minder dan in 2023. Deze migranten komen vooral",
      "hierheen om te werken, te studeren of voor hun gezin. Het aandeel dat asiel",
      "aanvraagt blijft relatief klein. In de afgelopen tien jaar was ongeveer",
      "11 procent van de migranten asielzoeker. In 2024 werden wel 15 procent meer",
      "asielvergunningen verleend dan een jaar eerder. Gisteren stemde de Tweede",
      "Kamer in met strengere asielwetten."
    )
  ),

  # Article 4: Migranten staan nodeloos aan de kant (Jun 2025)
  list(
    id = "nos_2572524",
    title = "Ongeveer een vijfde van alle migranten staat nodeloos aan de kant",
    date = "2025-06-26",
    text = paste(
      "Als migranten even vaak een baan zouden hebben als Nederlanders zonder",
      "migratieachtergrond, dan zouden zo'n 331.000 meer migranten werken dan nu",
      "het geval is. Dat concludeert de Adviesraad Migratie in een vandaag",
      "verschenen rapport over het onbenut arbeidspotentieel onder migranten.",
      "Het komt neer op ongeveer een vijfde van alle migranten. Het verschil in",
      "arbeidsdeelname met in Nederland geboren mensen is het grootst onder",
      "migranten van 45 jaar en ouder en vrouwen. Vooral bij Turkse, Marokkaanse",
      "en Syrische migranten is het onbenut arbeidspotentieel hoog. Om te zorgen",
      "dat meer van hen aan het werk komen, is vooral een goede opleiding en meer",
      "kennis van de Nederlandse taal belangrijk."
    )
  ),

  # Article 5: Nederlanders genuanceerder over migratie (Apr 2025)
  list(
    id = "nos_2561886",
    title = "Nederlanders genuanceerder over migratie dan debat doet vermoeden",
    date = "2025-04-01",
    text = paste(
      "De meeste Nederlanders denken genuanceerd over de komst van asielzoekers,",
      "hoewel debatten in de Tweede Kamer en berichten in de media een andere",
      "indruk wekken. Dat staat in een onderzoeksrapport over migratie van het",
      "Sociaal en Cultureel Planbureau. Volgens het onderzoek is er een grote",
      "variatie in opvattingen. Een derde van de bevolking vindt dat Nederland de",
      "grenzen moet sluiten voor asielzoekers, terwijl een op de tien juist meer",
      "asielzoekers wil opnemen. Maar tegelijkertijd is een grote groep van mening",
      "dat Nederland de plicht heeft om echte vluchtelingen op te vangen.",
      "Ondertussen zijn Nederlanders de voorbije twintig jaar eerder positiever",
      "dan negatiever gaan denken over toelating van migranten. SCP-onderzoeker",
      "Dagevos hoopt dat het rapport tot meer aandacht leidt voor de grote",
      "middengroep in het migratiedebat."
    )
  )
)

# --------------------------------------------------------------------------
# Define migration-related entity search terms
# We use a regex-style approach: find all tokens containing "migr"
# This catches: migrant, migranten, migratie, immigratie, immigranten, etc.
# --------------------------------------------------------------------------

cat("\n=== Parsing articles with spaCy ===\n")

all_tokens <- data.table()
for (art in articles) {
  cat(sprintf("  Parsing: %s\n", art$title))
  tok <- spacy_parse(art$text, dependency = TRUE)
  tok$doc_id <- art$id  # Use article ID instead of default doc names
  all_tokens <- rbindlist(list(all_tokens, as.data.table(tok)), fill = TRUE)
}

cat(sprintf("\nTotal tokens parsed: %d\n", nrow(all_tokens)))
cat(sprintf("Total sentences: %d\n", length(unique(paste(all_tokens$doc_id, all_tokens$sentence_id)))))

# --------------------------------------------------------------------------
# Find all migration-related entities (tokens containing "migr")
# --------------------------------------------------------------------------

migr_tokens <- unique(all_tokens[grepl("migr", token, ignore.case = TRUE), token])
migr_lemmas <- unique(all_tokens[grepl("migr", lemma, ignore.case = TRUE), lemma])

cat("\n=== Migration-related tokens found ===\n")
cat("Tokens: ", paste(migr_tokens, collapse = ", "), "\n")
cat("Lemmas: ", paste(migr_lemmas, collapse = ", "), "\n")

# --------------------------------------------------------------------------
# Extract motifs for all migration-related entities
# --------------------------------------------------------------------------

cat("\n=== Extracting semantic motifs ===\n")

# Use all tokens matching 'migr' pattern as entities
entities <- migr_tokens

cat(sprintf("Searching for motifs around %d entities: %s\n",
            length(entities), paste(entities, collapse = ", ")))

results <- extract_motifs(
  tokens = all_tokens,
  entities = entities,
  motif_classes = c("t", "a", "be", "H", "At", "aP"),
  markup = TRUE,
  add_sentence = TRUE,
  merge_separable_verbs = TRUE,   # NEW in v0.2.0: reconstruct separable verbs
  reflexive_as_patient = FALSE,   # NEW in v0.2.0: set TRUE to capture "zichzelf" etc.
  use_appos = TRUE,               # Sentence-scoped appos_child matching (restored in v0.2.0)
  lowercase = FALSE,
  verbose = TRUE
)

# --------------------------------------------------------------------------
# Optional: Compare with vs without separable verb merging
# --------------------------------------------------------------------------

cat("\n=== Separable verb impact comparison ===\n")

results_no_merge <- extract_motifs(
  tokens = all_tokens,
  entities = entities,
  motif_classes = c("a"),
  markup = TRUE,
  merge_separable_verbs = FALSE,
  verbose = FALSE
)

cat(sprintf("  Actions WITH separable verb merging:    %d\n", nrow(results$actions)))
cat(sprintf("  Actions WITHOUT separable verb merging: %d\n", nrow(results_no_merge$actions)))

if (nrow(results$actions) > 0 && nrow(results_no_merge$actions) > 0) {
  merged_verbs <- setdiff(results$actions$action, results_no_merge$actions$action)
  if (length(merged_verbs) > 0) {
    cat("  Verbs that changed due to merging:\n")
    for (v in merged_verbs) cat(sprintf("    %s\n", v))
  }
}

# --------------------------------------------------------------------------
# Display results
# --------------------------------------------------------------------------

cat("\n")
cat("================================================================\n")
cat("  RESULTS: Semantic motifs around migration entities\n")
cat("  Entity tokens: ", paste(entities, collapse = ", "), "\n")
cat("================================================================\n\n")

# Actions: What do migrants/migration DO?
cat("--- ACTIONS (what migration entities do) ---\n")
if (nrow(results$actions) > 0) {
  cat(sprintf("  Found %d action motifs:\n", nrow(results$actions)))
  for (i in 1:nrow(results$actions)) {
    cat(sprintf("    [%s] %s -> %s\n",
                results$actions$doc_id[i],
                results$actions$Entity[i],
                results$actions$markup[i]))
    if ("sentence" %in% names(results$actions)) {
      cat(sprintf("      Sentence: %s\n", results$actions$sentence[i]))
    }
  }
} else {
  cat("  (none found)\n")
}

# Treatments: What is done TO migrants/migration?
cat("\n--- TREATMENTS (what is done to migration entities) ---\n")
if (nrow(results$treatments) > 0) {
  cat(sprintf("  Found %d treatment motifs:\n", nrow(results$treatments)))
  for (i in 1:nrow(results$treatments)) {
    cat(sprintf("    [%s] %s -> %s\n",
                results$treatments$doc_id[i],
                results$treatments$Entity[i],
                results$treatments$markup[i]))
    if ("sentence" %in% names(results$treatments)) {
      cat(sprintf("      Sentence: %s\n", results$treatments$sentence[i]))
    }
  }
} else {
  cat("  (none found)\n")
}

# Characterizations: How are migrants/migration described?
cat("\n--- CHARACTERIZATIONS (how migration entities are described) ---\n")
if (nrow(results$characterizations) > 0) {
  cat(sprintf("  Found %d characterization motifs:\n", nrow(results$characterizations)))
  for (i in 1:nrow(results$characterizations)) {
    cat(sprintf("    [%s] %s -> %s\n",
                results$characterizations$doc_id[i],
                results$characterizations$Entity[i],
                results$characterizations$markup[i]))
    if ("sentence" %in% names(results$characterizations)) {
      cat(sprintf("      Sentence: %s\n", results$characterizations$sentence[i]))
    }
  }
} else {
  cat("  (none found)\n")
}

# Possessions: What do migrants/migration HAVE?
cat("\n--- POSSESSIONS (what migration entities possess) ---\n")
if (nrow(results$possessions) > 0) {
  cat(sprintf("  Found %d possession motifs:\n", nrow(results$possessions)))
  for (i in 1:nrow(results$possessions)) {
    cat(sprintf("    [%s] %s -> %s\n",
                results$possessions$doc_id[i],
                results$possessions$Entity[i],
                results$possessions$markup[i]))
  }
} else {
  cat("  (none found)\n")
}

# Agent-Treatments: WHO does what to migrants/migration?
cat("\n--- AGENT-TREATMENTS (who acts upon migration entities) ---\n")
if (nrow(results$agent_treatments) > 0) {
  cat(sprintf("  Found %d agent-treatment motifs:\n", nrow(results$agent_treatments)))
  for (i in 1:nrow(results$agent_treatments)) {
    cat(sprintf("    [%s] %s -> %s\n",
                results$agent_treatments$doc_id[i],
                results$agent_treatments$Entity[i],
                results$agent_treatments$markup[i]))
    if ("sentence" %in% names(results$agent_treatments)) {
      cat(sprintf("      Sentence: %s\n", results$agent_treatments$sentence[i]))
    }
  }
} else {
  cat("  (none found)\n")
}

# Action-Patients: What do migrants/migration do TO WHOM?
cat("\n--- ACTION-PATIENTS (whom migration entities act upon) ---\n")
if (nrow(results$action_patients) > 0) {
  cat(sprintf("  Found %d action-patient motifs:\n", nrow(results$action_patients)))
  for (i in 1:nrow(results$action_patients)) {
    cat(sprintf("    [%s] %s -> %s\n",
                results$action_patients$doc_id[i],
                results$action_patients$Entity[i],
                results$action_patients$markup[i]))
    if ("sentence" %in% names(results$action_patients)) {
      cat(sprintf("      Sentence: %s\n", results$action_patients$sentence[i]))
    }
  }
} else {
  cat("  (none found)\n")
}

# --------------------------------------------------------------------------
# Summary statistics
# --------------------------------------------------------------------------

cat("\n================================================================\n")
cat("  SUMMARY\n")
cat("================================================================\n")
cat(sprintf("  Articles analyzed:      %d\n", length(articles)))
cat(sprintf("  Total tokens:           %d\n", nrow(all_tokens)))
cat(sprintf("  Entity tokens:          %s\n", paste(entities, collapse = ", ")))
cat(sprintf("  Actions found:          %d\n", nrow(results$actions)))
cat(sprintf("  Treatments found:       %d\n", nrow(results$treatments)))
cat(sprintf("  Characterizations:      %d\n", nrow(results$characterizations)))
cat(sprintf("  Possessions found:      %d\n", nrow(results$possessions)))
cat(sprintf("  Agent-treatments:       %d\n", nrow(results$agent_treatments)))
cat(sprintf("  Action-patients:        %d\n", nrow(results$action_patients)))

total_motifs <- nrow(results$actions) + nrow(results$treatments) +
  nrow(results$characterizations) + nrow(results$possessions) +
  nrow(results$agent_treatments) + nrow(results$action_patients)

cat(sprintf("  --------------------------------\n"))
cat(sprintf("  TOTAL MOTIFS:           %d\n", total_motifs))
cat("================================================================\n")

# --------------------------------------------------------------------------
# Clean up
# --------------------------------------------------------------------------
cat("\nDone! Finalizing spaCy...\n")
spacy_finalize()
