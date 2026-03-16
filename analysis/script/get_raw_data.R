# Description: Grabs the raw data from Synapse and stores it in the data-raw folder.
#  We are using the genieBPC function in the hopes that it will support
#  PAT access in the future.
# Author: Alex Paynter

# Before running this file you will need to login to synapse with
#   genieBPC::set_synapse_credentials()
# This login will expire every 24h, and should be replaced with a PAT
#   access ASAP.

library(cli)
library(synapser)
library(here)
library(genieBPC)
library(fs)
library(purrr)
library(here)
library(tibble)
library(stringr)
library(dplyr)

dir.create(here("data-raw"), showWarnings = F)
dir.create(here("data"), showWarnings = F)
# Not immediately needed but setting up now:
dir.create(here('data', 'survival', 'prepared_data'), showWarnings = F)
dir.create(here('data', 'survival', 'fit_outputs'), showWarnings = F)

# Rdata version:
data_list <- genieBPC::pull_data_synapse(cohort = "BrCa",
                                         version = "v1.2-consortium")
# Save the Rdata version:
readr::write_rds(x = data_list,
                 file = here('data-raw', 'data_list.rds'))


# CSV versions.
# These may not match the processing done by MSK exactly (for example I think
#   they filter down to index cancer regimens only).
synLogin()
synid_clin_data <- 'syn39802381'
synid_gen_data <- 'syn32299078'
vec_gen_files <- c(
  'data_CNA.txt', 
  'data_fusions.txt', 
  'data_mutations_extended.txt',
  'data_gene_matrix.txt'
  #'data_gene_panel_DFCI-ONCOPANEL-1.txt'
  # Also includes all gene panels using the logic below.
)
dft_dat_list <- synGetChildren(synid_clin_data) %>%
  as.list %>%
  purrr::map_dfr(.x = .,
                 .f = as_tibble)
dft_gen_list <- synGetChildren(synid_gen_data) %>%
  as.list %>%
  purrr::map_dfr(.x = .,
                 .f = as_tibble) %>%
  filter(name %in% vec_gen_files | str_detect(name, "^data_gene_panel_.*"))
dft_dat_list <- bind_rows(dft_dat_list, dft_gen_list)

syn_store_in_dataraw <- function(sid) {
  synGet(
    entity = sid, 
    downloadLocation = here("data-raw"),
    ifcollision = 'overwrite.local' # replaces local copy if it exists.
  )
}
purrr::walk(.x = dft_dat_list$id, 
            .f = syn_store_in_dataraw)





# Update: Also want to pull a sheet shared by Brooke M. @ MSK:
synLogin()
classified_meds_synid <- "syn51405609"
synGet(entity = classified_meds_synid,
       downloadLocation = here("data"))

# And one file created by Protiva and Brooke - I saved a perm
