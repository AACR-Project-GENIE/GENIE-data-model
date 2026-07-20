# Description: 
# Author: Haley Hunter-Zinck
# Date: 

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_file_msk_panc <- "syn25541828"

# parameters
column_names <- c("rt_start_time", "rt_stop_time")

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

# read ----------------------------

data <- get_synapse_entity_data_in_csv(synid_file_msk_panc, 
                                       version = 3,
                                       na.strings = c(""))


# main ----------------------------

res <- data %>% 
  filter(!is.na(rt_start_time)) %>%
  select(all_of(column_names)) %>%
  head()
print(res)

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
