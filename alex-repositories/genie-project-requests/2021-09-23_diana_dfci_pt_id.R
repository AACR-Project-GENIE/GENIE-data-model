# Description: Determine if DFCI patient ID was retracted in the most recent GENIE release
# Author: Haley Hunter-Zinck
# Date: 


# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_file_retracted <- "syn26134869"

# parameters
record_id <- "GENIE-DFCI-093951"

# read ----------------------------

df_retracted <- read.csv(synGet(synid_file_retracted)$path, stringsAsFactors = F)

# main ----------------------------

print(df_retracted %>% 
  filter(PATIENT_ID == record_id))

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
