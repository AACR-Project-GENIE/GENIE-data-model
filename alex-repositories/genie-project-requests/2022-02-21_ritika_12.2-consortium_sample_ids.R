# Description: Check sample IDs in the 12.2-consoritum release files.
# Author: Haley Hunter-Zinck
# Date: 2022-02-21

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_file_sample <- "syn9734573"
synid_version_sample <- "197"
synid_file_prov <- "syn26986940"
synid_version_prov <- "1"

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
                                           comment.char = "#") {
  
  if (is.na(version)) {
    entity <- synGet(synapse_id)
  } else {
    entity <- synGet(synapse_id, version = version)
  }
  
  data <- read.csv(entity$path, stringsAsFactors = F, 
                   na.strings = na.strings, sep = sep, check.names = check_names,
                   header = header, comment.char = comment.char)
  return(data)
}

# read ----------------------------

data <- get_synapse_entity_data_in_csv(synid_file_sample, version = synid_version_sample, comment.char = "#", sep = "\t")
prov <- get_synapse_entity_data_in_csv(synid_file_prov, version = synid_version_prov, comment.char = "#", sep = "\t")

# main ----------------------------

samp_long <- data %>% 
  filter(grepl(pattern = "PROV", x = SAMPLE_ID)) %>%
  mutate(n_chr = nchar(SAMPLE_ID)) %>%
  filter(n_chr > 30) %>%
  select(SAMPLE_ID, n_chr)

samp_short <- data %>% 
  filter(grepl(pattern = "PROV", x = SAMPLE_ID)) %>%
  mutate(n_chr = nchar(SAMPLE_ID)) %>%
  filter(n_chr <= 30) %>%
  select(SAMPLE_ID, n_chr)

id_rel <- unlist(data %>% 
  filter(grepl(pattern = "PROV", x = SAMPLE_ID)) %>%
  select(SAMPLE_ID) %>% 
  distinct())

id_upl <- unlist(prov %>% 
                   select(SAMPLE_ID) %>% 
                   distinct())

diff_rel <- setdiff(id_rel, id_upl)
diff_upl <- setdiff(id_upl, id_rel)

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
