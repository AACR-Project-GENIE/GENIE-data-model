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
synid_file_dd <- "syn25468849"

# parmaeters
drug_name <- "Irinotecan"
oth_drug_names <- c("Fluorouracil", "Irinotecan Hydrochloride", "Leucovorin Calcium")

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

dd <- get_synapse_entity_data_in_csv(synid_file_dd)

# specific drug ----------------------------

drugs_str <- unlist(dd %>% 
  filter(`Variable / Field Name` == "drugs_drug_1") %>%
  select(`Choices, Calculations, OR Slider Labels`))

drugs <- strsplit(x = strsplit(x = drugs_str, split = "\\|")[[1]], split = ", ")

res <- unlist(lapply(drugs, 
       function(x, pattern) {return(grepl(pattern = pattern, x = x[2]))}, 
       pattern = drug_name))

print(drugs[which(res)])

# find codes for drugs ----------------------

for (oth_drug in oth_drug_names) {
  res <- unlist(lapply(drugs, 
                       function(x, pattern) {return(grepl(pattern = pattern, x = x[2]))}, 
                       pattern = oth_drug))
  
  print(glue("{oth_drug}: {drugs[which(res)][[1]][1]}"))
}

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
