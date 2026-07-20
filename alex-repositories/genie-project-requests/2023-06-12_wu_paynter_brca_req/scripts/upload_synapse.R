output_synid <- "syn52161183" 

library(synapser)
library(magrittr)
library(here)

synLogin()
synapser::File(here("output", "wu-brca-main-genie-request.html"),
               parent = output_synid) %>%
  synStore()

synapser::File(here("output", "brca_known_impact.csv"),
               parent = output_synid) %>%
  synStore()

synapser::File(here("output", "brca_unknown_impact.csv"),
               parent = output_synid) %>%
  synStore()

synapser::File(here("output", "brca_known_impact_bpc.csv"),
               parent = output_synid) %>%
  synStore()

synapser::File(here("output", "brca_unknown_impact_bpc.csv"),
               parent = output_synid) %>%
  synStore()
