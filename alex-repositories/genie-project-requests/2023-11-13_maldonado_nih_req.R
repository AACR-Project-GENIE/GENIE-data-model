# Description:  Starter code for Alberto Maldonado's project, which seeks to test the frequency of clincially defined groups with a particular gene alteration.
# Author:  Alex Paynter

# We will use the synapse R client 'synapser' to download the data.
# Useful resources on synapser (install, use, etc) include:
# - https://github.com/Sage-Bionetworks/synapser
# - https://r-docs.synapse.org/
# If you don't want to use synapser this project could absolutely be 
# completed by downloading the data and loading it into memory from 
# a file. 

library(synapser)
library(dplyr)
library(magrittr)
library(readr)
library(gtsummary)
library(janitor)

# Defining a convenience function for loading synapse txt entities into memory.
get_synapse_entity_txt <- function(
  synapse_id, 
  version = NA,
  skip = 0,
  cols_to_lower = T
) {
  if (is.na(version)) {
    entity <- synGet(synapse_id, followLink = T)
  } else {
    entity <- synGet(synapse_id, followLink = T, version = version)
  }
  data <- read_tsv(
    file = entity$path,
    show_col_types = F,
    progress = F,
    skip = skip,
    comment = "#"
  )
  if (cols_to_lower) {
    data <- rename_all(data, tolower)
  }
  return(data)
}

synLogin()

# The appropriate synapse entity IDs can be found by looking at the relevant 
#   GENIE project in Synapse (web browser).  AACR or Sage can help with this.
dft_maf <- get_synapse_entity_txt('syn9734426') # data_mutations_extended.txt
dft_clin <- get_synapse_entity_txt('syn9734421') # data_clinical.txt

# reduce these datasets down to only the columns needed for the project.
# expand as needed.
dft_maf %<>%
  select(
    sample_id = tumor_sample_barcode, # renaming to match clinical data.
    hugo_symbol,
    hgvsp_short # protein encoding seemed preferred.
  )

dft_clin %<>%
  select(
    sample_id,
    patient_id,
    primary_race,
    ethnicity,
    oncotree_code,
    oncotree_primary_node
  )

# There was a custom definition of race used in the slides with four levels.
# This is my guess of how that's encoded:
lev_race <- c(
  "NHW", # non-hispanic white
  "NHB", # non-hispanic black
  "Hisp", # hispanic
  "Asian" # asian - presumably non-hispanic also.
)
dft_clin %<>%
  mutate(
    race_custom = case_when(
      # If this information is missing we can't do much in the given encoding:
      ethnicity %in% c("Not Collected", "Unknown") ~ NA_character_,
      primary_race %in% c("Not Applicable", "Not Collected", 
                          "Other", "Unknown") ~ NA_character_,
      # When it is known here is my guess:
      ethnicity %in% "Spanish/Hispanic" ~ lev_race[3],
      primary_race %in% "White" ~ lev_race[1],
      primary_race %in% "Black" ~ lev_race[2],
      # I did not include pacific islander here - update as needed.
      primary_race %in% "Asian" ~ lev_race[4],
      T ~ NA_character_
    ),
    race_custom = factor(race_custom, levels = lev_race)
  )

# Important note:  Under this scheme roughly 30% of the samples/people are left out.
dft_clin %>% janitor::tabyl(race_custom)

# remove extra vars:
dft_clin %<>% select(-c(primary_race, ethnicity))

# ad = analysis dataset.
# we will add the clinical data to each row in the (reduced) MAF file.
dft_ad <- left_join(
  dft_maf,
  dft_clin,
  by = "sample_id"
) 

# The goal was to filter down to cases of pancreatic ductal adenocarcinoma.
# Because I am not a pancreatic cancer subtype expert and PDAC is not a valid 
# oncotree code, I took a guess here.  Please review the oncotree codes 
# associated with pancreatic cancer (https://oncotree.mskcc.org/#/home) and 
# update as needed (you can specify more than one).
dft_ad %<>%
  filter(
    oncotree_code %in% c("PAAD") # PAAD = Pancreatic Adenocarcinoma
  ) %>%
  select(-c(oncotree_code, oncotree_primary_node))

# dft_ad is now a dataframe with one row per mutation.  That is, if one person
#  had two samples with 3 and 4 alterations each, that person would have 7 rows.
# The request was to count the number of alterations in each person.  So step 
# one is limit the dataframe to one row per {patient, alteration type}.
dft_ad %<>%
  group_by(patient_id, hugo_symbol, hgvsp_short) %>%
  slice(1) %>% # limit to one row per group
  ungroup(.) %>%
  select(-sample_id) # sample_id is meaningless now - we just selected the first positive.

# Count the number of patients with each alteration type, split by race groups:
dft_alt_counts <- dft_ad %>%
  group_by(hugo_symbol, hgvsp_short, race_custom) %>%
  summarize(
    n = n(),
    .groups = "drop"
  )

# Let's look at KRAS to see if it lines up roughly with what you saw:
dft_alt_counts %>%
  filter(hugo_symbol %in% 'KRAS') %>%
  filter(hgvsp_short %in% c('p.G12D', 'p.G12V', 'p.G12R', 'p.G12C')) %>%
  arrange(race_custom, hgvsp_short)

# This looks approximately right - much more G12D and G12V than G12R and G12C
#  within each race group.

# The final step to reproducing the results is finding the correct denominator.
# There are two ways to do this:
#   1. Use the data_gene_panel_*.txt files, found in the same location as the
#      clinical and data_mutations_extended files.  These express whether an
#      entire gene was covered in the panel or not.  This is a fairly coarse 
#      concept of a denominator and possibly not appropriate for your project.
#   2. Use the genie_combined.bed file, which can give you denominators for
#      specific genomic coordinates each assay covers.  This is harder to work
#      with (I personally don't - talk to your local bioinformaticist) but 
#      possibly more accurate.


  






