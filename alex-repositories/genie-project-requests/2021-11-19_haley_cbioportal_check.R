# Description: check for value in mutation file.
# Author: Haley Hunter-Zinck
# Date: 2021-11-19
# command line for cbioportal files: 
# python -m geniesp PANC ../cbioportal 1.1-consortium --staging
# for file in $(ls *meta*) 
# {
#   echo $file
#   sed 's/panc/prostate/' $file > tmp.txt
#   mv tmp.txt $file
# }
# 
# for file in $(ls meta*) 
# {
#   echo $file
#   synapse store --parentid syn26471041 $file
# }
# python -m geniesp Prostate ../cbioportal 1.1-consortium --staging
# python ../cbioportal/core/src/main/scripts/importer/validateData.py -s PANC -n 
# python ../cbioportal/core/src/main/scripts/importer/validateData.py -s Prostate -n 


# setup ------------------------

library(synapser)
synLogin()

# synapse 
synid_file_mut <- "syn22228700"

# functions----------------

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

# main -------------------------------

data <- get_synapse_entity_data_in_csv(synid_file_mut, sep = "\t")

# check
col_names <- c('t_depth', 't_ref_count', 't_alt_count','n_depth', 'n_ref_count', 'n_alt_count')
count <- c()
for (col in col_names) {
  count[col] <- length(which(data[[col]] == "."))
}

print(count)

