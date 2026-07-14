# Description: Why index cancer diagnoses aren't being loaded into the timeline diagnosis file.
# Author: Haley Hunter-Zinck
# Date: 2022-04-27

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)

# synapse
synid_file_dx <- "syn22296816"

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
#' @param comment.char character designating comment lines to ignore
#' @return data frame
get_synapse_entity_data_in_csv <- function(synapse_id, 
                                           version = NA,
                                           sep = ",", 
                                           na.strings = c("NA"), 
                                           header = T,
                                           check_names = F,
                                           comment.char = "#",
                                           colClasses = "character") {
  
  if (is.na(version)) {
    entity <- synGet(synapse_id)
  } else {
    entity <- synGet(synapse_id, version = version)
  }
  
  data <- read.csv(entity$path, stringsAsFactors = F, 
                   na.strings = na.strings, sep = sep, check.names = check_names,
                   header = header, comment.char = comment.char, colClasses = colClasses)
  return(data)
}

# synapse login --------------------

synLogin()

# read ----------------------------

der_dx <- get_synapse_entity_data_in_csv(synid_file_dx)


# main ----------------------------

n_prostate_dx <- der_dx %>% 
  filter(cohort == "Prostate") %>%
  count()

n_prostate_dx_start <- der_dx %>% 
  filter(cohort == "Prostate" & first_index_ca_days != "") %>%
  count()

n_prostate_dx_alt_start <- der_dx %>% 
  filter(cohort == "Prostate" & dob_ca_dx_days != "") %>%
  count()

der_dx %>% 
  filter(cohort == "Prostate" & first_index_ca_days != "") %>%
  select(ca_type, ca_histology)

der_dx %>% 
  filter(cohort == "Prostate" & first_index_ca_days != "" & redcap_ca_seq == "1") %>%
  select(ca_type, ca_histology)

der_dx %>% 
  filter(cohort == "Prostate" & grepl(pattern = "Prostate", x = ca_type)) %>%
  select(first_index_ca_days, ca_type)

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
