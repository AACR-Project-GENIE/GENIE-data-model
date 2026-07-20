# Description: Determine if DFCI query responses are in the right place.  
# Author: Haley Hunter-Zinck
# Date: 2021-10-05

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_file_dfci_panc_queries <- "syn26292458"

# functions ----------------------------

get_synapse_entity_data_in_xlsx <- function(synapse_id, 
                                            version = NA,
                                            sheet = 1,
                                            check.names = F) {
  library(openxlsx)
  
  if (is.na(version)) {
    entity <- synGet(synapse_id)
  } else {
    entity <- synGet(synapse_id, version = version)
  }
  
  data <- read.xlsx(entity$path, check.names = check.names, sheet = sheet)
  
  return(data)
}

# read ----------------------------

data_excel <- get_synapse_entity_data_in_xlsx(synid_file_dfci_panc_queries, 
                                              version = 1)

# look at query sign off ----------------------------

print(head(data_excel))
print(table(data_excel$QA.manager.signoff, useNA = "always"))

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
