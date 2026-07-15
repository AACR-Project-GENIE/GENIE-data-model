# Not currently working, I don't know why.  Getting a 403 Client Error.

private_synid <- "syn52266449" # not shared, can't be made public (IID)

library(synapser)
library(magrittr)
library(here)

synLogin()
synapser::File(
  path = here("data", "genomic", "cna_long_selected_onco.txt"),
  parent = private_synid) %>%
  synStore()

synapser::File(
  path = here("data", "genomic", "mut_onco.txt"),
  parent = private_synid) %>%
  synStore()

synapser::File(
  here("data", "genomic", "fus_onco.txt"),
  parent = private_synid) %>%
  synStore()
