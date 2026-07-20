output_synid <- "syn52392353" #2023-04-17-BrCa-landscape-paper-outputs

library(synapser)
library(here)
library(magrittr)

synLogin()
synapser::File(here("analysis", "report", "genie-bpc-off-label.html"),
               parent = output_synid) %>%
  synStore()


synapser::File(here("output", "regimen_counts_by_cohort.csv"),
               parent = output_synid) %>%
  synStore()
