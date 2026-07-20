# Description: Check for non-numeric values in the "naaccr_diagnostic_conf_cd" variable in the DFCI PANC upload. 
# Author: Haley Hunter-Zinck
# Date: 2021-10-05

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_panc_dfci_header <- "syn25544639"
synid_panc_dfci_data <- "syn25544638"

# functions ----------------------------

get_synapse_entity_data_in_csv <- function(synapse_id, 
                                           version = NA,
                                           sep = ",", 
                                           na.strings = c("NA"), 
                                           header = T) {
  
  if (is.na(version)) {
    entity <- synGet(synapse_id)
  } else {
    entity <- synGet(synapse_id, version = version)
  }
  
  data <- read.csv(entity$path, stringsAsFactors = F, 
                   na.strings = na.strings, sep = sep, check.names = F,
                   header = header)
  return(data)
}

is_double <- function(x) {
  
  if (is.na(x)) {
    return(NA)
  }
  
  res <- tryCatch(as.double(x),
             error = function(cond) {return(NA)},
             warning = function(cond) {return(NA)})
  
  if (!is.na(res)) {
    return(T)
  }
  
  return(F)
}

# read ----------------------------

data <- get_synapse_entity_data_in_csv(synid_panc_dfci_data, na.strings = "")
header <- get_synapse_entity_data_in_csv(synid_panc_dfci_header)
colnames(data) <- header


# main ----------------------------

idx <- which(!apply(matrix(data$naaccr_diagnostic_conf_cd, ncol = 1), 1, is_double))
res <- data[idx, c("record_id", "redcap_repeat_instrument", "redcap_repeat_instance", "naaccr_diagnostic_conf_cd")]
print(res)

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
