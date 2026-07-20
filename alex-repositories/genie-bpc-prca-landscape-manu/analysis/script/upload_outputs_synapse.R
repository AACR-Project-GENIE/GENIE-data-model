output_prca_synid <- "syn51393619" #2023-04-17-PrCa-landscape-paper-outputs

library(synapser)
library(magrittr)
library(here)

synLogin()
synapser::File(here("analysis", "report", "genie-bpc-prca-manu.html"),
               parent = output_prca_synid) %>%
  synStore()
