# Description: Do the data dictionaries have branching logic?
# Author: Haley Hunter-Zinck
# Date: 2021-12-01

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
#synid_file_ddphi <- "syn26469276" # BLADDER 3.7.6
synid_file_ddphi <- "syn24181708" # BrCa 3.4.8

# functions ----------------------------

#' Download and load data stored in csv or other delimited format on Synapse
#' into an R data frame.
#' 
#' @param synapse_id Synapse ID
#' @version Version of the Synapse entity to download.  NA will load current
#' version
#' @param set Delimiter for file
#' @param na.strings Vector of strings to be read in as NA values
#' @param header TRUE if the file contains a header row; FALSE otherwise.
#' @param check_names TRUE if column names should be modified for compatibility 
#' with R upon reading; FALSE otherwise.
#' @return data frame
get_synapse_entity_data_in_csv <- function(synapse_id, 
                                           version = NA,
                                           sep = ",", 
                                           na.strings = c("NA"), 
                                           header = T,
                                           check_names = F) {
  
  if (is.na(version)) {
    entity <- synGet(synapse_id)
  } else {
    entity <- synGet(synapse_id, version = version)
  }
  
  data <- read.csv(entity$path, stringsAsFactors = F, 
                   na.strings = na.strings, sep = sep, check.names = check_names,
                   header = header)
  return(data)
}

# read ----------------------------

ddphi <- get_synapse_entity_data_in_csv(synid_file_ddphi, na.strings = "")


# main ----------------------------

# control
ddphi %>% 
  filter(!is.na(`Branching Logic (Show field only if...)`) & grepl(pattern = 'vstat', x = `Branching Logic (Show field only if...)`)) %>%
  select(`Branching Logic (Show field only if...)`)

# look for cohort variable
ddphi %>% 
  filter(!is.na(`Branching Logic (Show field only if...)`) & grepl(pattern = 'cohort', x = `Branching Logic (Show field only if...)`)) %>%
  select(`Branching Logic (Show field only if...)`)

# look for rt_start_time variable in branching logic
ddphi %>% 
  filter(!is.na(`Branching Logic (Show field only if...)`) & grepl(pattern = 'rt', x = `Branching Logic (Show field only if...)`)) %>%
  select(`Branching Logic (Show field only if...)`)
ddphi %>% 
  filter(!is.na(`Branching Logic (Show field only if...)`) & grepl(pattern = 'rt_start_time', x = `Branching Logic (Show field only if...)`)) %>%
  select(`Branching Logic (Show field only if...)`)

# get rt_start_time variable specific branching logic
ddphi %>% 
  filter(`Variable / Field Name` == 'rt_start_time') %>%
  select(`Branching Logic (Show field only if...)`, `Required Field?`)


# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
