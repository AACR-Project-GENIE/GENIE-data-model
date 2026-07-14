library(tidyverse)
library(magrittr)
library(synapser)
library(here)

synLogin()


syn_store_help <- function(
    sid, 
    loc = here('data-raw'), 
    v = NULL
) {
  
  # not sure how to do this with synGet, so we'll do a conditional for the version.
  if (is.null(v)) {
    synGet(
      entity = sid, 
      downloadLocation = loc,
      ifcollision = 'overwrite.local'
    ) 
  } else {
    synGet(
      entity = sid, 
      downloadLocation = loc,
      ifcollision = 'overwrite.local',
      version = v
    ) 
  }
}

main_genie_folder <- 'syn64770196' # 18.2 consortium.
df_main_children <- synGetChildren(main_genie_folder) %>%
  as.list %>%
  purrr::map_dfr(.x = .,
                 .f = as_tibble)

# Don't think we need the panel files here.
df_main_children %<>%
  filter(name %in% c(
    "data_mutations_extended.txt",
    "data_CNA.txt",
    "data_fusions.txt",
    'data_sv.txt', 
    'genomic_information.txt',
    'data_clinical_patient.txt',
    'data_clinical_sample.txt'
  ))

purrr::walk(
  .x = df_main_children$id, 
  .f = \(z) {
    syn_store_help(
      z, 
      loc = here("data-raw", 'main_genie')
    )}
)
