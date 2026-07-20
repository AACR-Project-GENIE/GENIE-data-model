output_synid <- "syn51470310"

library(synapser)
library(magrittr)
library(here)

synLogin()
synapser::File(here("output.html"),
               parent = output_synid) %>%
  synStore()
