# Description:  Get a list of participants from the BPC cohorts.
# Some pasted code from 
# https://github.com/Sage-Bionetworks/genie-project-requests/blob/main/2023-04-05_jennifer_alex_drugs_in_bpc.R
# Author: Alex Paynter

library(synapser)
library(readxl)
library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)
library(magrittr)
library(glue)
library(here)

synLogin()

get_synapse_entity_data_in_csv <- function(synapse_id, 
                                           version = NA) {
  if (is.na(version)) {
    entity <- synGet(synapse_id)
  } else {
    entity <- synGet(synapse_id, version = version)
  }
  
  data <- read_csv(entity$path,
                   show_col_types = F,
                   progress = F)
  return(data)
}

# a interation dataframe where each row specifies a cohort and synid for the folder.
# Here we're taking the latest CONSORTIUM release because some people get 
#   pulled out for the public release.
dft_folders <- tibble::tribble(
  ~cohort, ~synid,
  "BLADDER", "syn28495599", # 1.1 consortium
  "BrCa", "syn39802381", # 1.2 consortium
  "CRC", "syn39802279", # 2.0 public
  "NSCLC", "syn27245047", # 2.0 public
  
  # For the public release files I'm pulling the consortium releases too.  
  # We can filter duplicates later on.
  "CRC", "syn53463635", # 1.3 consortium 
  "NSCLC", "syn53463552", # 2.2 consortium.
  "NSCLC", 'syn22418966', # 1.1 consortium - just because I don't understand the numbering.
  
  "PANC", "syn50612197", # 1.2 consortium
  "Prostate", "syn50612196", # 1.2 consortium
  # cohorts that had no available releases at the time:
  #   CRC2, ESOPHAGO, MELANOMA, NSCLC2, OVARIAN, RENAL
)

pull_one <- function(synid_fold) {
  df_clin_children <- synGetChildren(synid_fold) %>%
    as.list %>%
    purrr::map_dfr(.x = .,
                   .f = as_tibble)
  
  
  syn_pt <- df_clin_children %>%
    filter(name %in% "patient_level_dataset.csv") %>%
    pull(id)
  
  dft_pt <- get_synapse_entity_data_in_csv(syn_pt)
  
  
  # A few messages just to get the rough idea:
  message(glue("Cohort: {first(dft_pt$cohort)}"))
  message(glue("Patient level dataset read in with {nrow(dft_pt)} rows."))
  
  # Minimal info needed for this request so far:
  dft_pt %<>% select(cohort, record_id)
  
  return(dft_pt)
}

# If you want to pull one:
# pull_one(pull(slice(dft_folders, 3), synid))

# To pull them all:
dft_bpc_participants <- dft_folders %>%
  mutate(
    dat = purrr::pmap(
      .f = pull_one,
      .l = list(synid_fold = synid)
    )
  ) %>%
  pull(dat) %>%
  dplyr::bind_rows(.)

dft_bpc_participants %<>%
  distinct(.) %>%
  arrange(cohort, record_id)

readr::write_rds(
  x = dft_bpc_participants,
  file = here("data", "bpc_participants.rds")
)
