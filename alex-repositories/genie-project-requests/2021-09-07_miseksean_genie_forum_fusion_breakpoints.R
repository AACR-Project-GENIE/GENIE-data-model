# Description: Look for any data on actual fusion breakpoints in submitted GENIE files.  
# Author: Haley Hunter-Zinck
# Date: September 7, 2021
# Request:
#   source: GENIE forum
#   url: https://www.synapse.org/#!Synapse:syn7222066/discussion/threadId=8220
#   author: https://www.synapse.org/#!Profile:3344028 (@Miseksean)
# Note: Tom said GENIE is working on a revised file format for structural variants but this
#   will only be available in 6 - 12 months at best.  So we did not supply the MSK breakpoint
#   information to the user.  

#center	n_fusion	comments	synapse
#MSK	21153	yes	https://www.synapse.org/#!Synapse:syn7224465
#DFCI	12879	no	https://www.synapse.org/#!Synapse:syn19031190
#UCSF	1339	no	https://www.synapse.org/#!Synapse:syn12224153
#VICC	1033	no	https://www.synapse.org/#!Synapse:syn7214277
#CHOP	546	no	https://www.synapse.org/#!Synapse:syn22242717
#UHN	164	no	https://www.synapse.org/#!Synapse:syn20397180

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(synapser)
synLogin()

# synapse
synid_file_fusion_msk <- "syn7224465"

# functions ----------------------------

get_synapse_entity_data_in_csv <- function(synapse_id, sep = ",", na.strings = c("NA")) {
  
  data <- read.csv(synGet(synapse_id)$path, stringsAsFactors = F, 
                   na.strings = na.strings, sep = sep)
  return(data)
}

trim <- function(str) {
  front <- gsub(pattern = "^[[:space:]]+", replacement = "", x = str)
  back <- gsub(pattern = "[[:space:]]+$", replacement = "", x = front)
  
  return(back)
}

get_element_from_list <- function(x, idx) {
  return(unlist(lapply(lapply(x, head, n = idx), tail, n = 1)))
}

# read ----------------------------

data <- get_synapse_entity_data_in_csv(synid_file_fusion_msk, sep = "\t")
comments <- data[,"Comments"]

# main ----------------------------

prefix <- get_element_from_list(strsplit(comments, split = "[Rr]earrangement[ ]*:"), idx = 2)

suffix <- get_element_from_list(strsplit(prefix, split = "Note"), idx = 1)
rearr <- trim(suffix)

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
