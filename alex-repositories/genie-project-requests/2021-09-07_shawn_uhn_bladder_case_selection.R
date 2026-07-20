# Description: check number of cases selected for SDV and IRR
# Author: Haley Hunter-Zinck
# Date: September 7, 2021

# setup ---------------------------------

library(synapser)
synLogin()

# synapse
synid_file_uhn_bladder <- "syn22311993"

# functions -------------------------------

get_synapse_entity_data_in_csv <- function(synapse_id, sep = ",", na.strings = c("NA")) {
  
  data <- read.csv(synGet(synapse_id)$path, stringsAsFactors = F, 
                   na.strings = na.strings, sep = sep)
  return(data)
}


# read -------------------------------------

data <- get_synapse_entity_data_in_csv(synid_file_uhn_bladder)

n_pilot <- 10
n_sdv <- length(which(data[,"sdv"] == "UHN" & is.na(data[,"remove"])))
n_sdv_all <- length(which(data[,"sdv"] == "UHN"))
n_irr_all <- length(which(data[,"irr"] == "UHN"))
n_irr <- length(which(data[,"irr"] == "UHN" & is.na(data[,"remove"])))

# output -----------------------------------

print(glue("Number SDV (pilot included, deceased included): {n_sdv_all}"))
print(glue("Number SDV (pilot included, deceased removed): {n_sdv}"))
print(glue("Number SDV (pilot excluded, deceased removed): {n_sdv - n_pilot}"))
print(glue("Number IRR (deceased included): {n_irr_all}"))
print(glue("Number IRR (deceased removed): {n_irr}"))

      