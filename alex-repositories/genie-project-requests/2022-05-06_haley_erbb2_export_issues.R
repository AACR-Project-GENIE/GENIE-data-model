# Description: Examind issues with ERBB2 upload data.
# Author: Haley Hunter-Zinck
# Date: 2022-05-06

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)

# synapse
synid_file_data <- "syn29875992"
synid_file_dd <- "syn29880018"
synid_table_map <- "syn8363731"

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

#' Read contents of an Excel Spreadsheet stored on Synapse.
#' 
#' @param synapse_id Synapse ID of the spreadsheet
#' @param version Version of the file
#' @param sheet Number of the sheet of the spreadsheet
#' @param check.names Whether R should modify names if non-conforming
#' @return Matrix of data
#' @example 
#' get_synapse_entity_data_in_xlsx(synapse_id = "syn123345", sheet = 2)
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

add_genie_prefix <- function(x) {
  if (!(is.na(x) || grepl(pattern = "^GENIE-", x = x))) {
    return(glue("GENIE-{x}"))
  }
  return(x)
}

# synapse login --------------------

synLogin()

# read ----------------------------

data <- get_synapse_entity_data_in_xlsx(synid_file_data)
dd <- get_synapse_entity_data_in_csv(synid_file_dd)

query <- glue("SELECT * FROM {synid_table_map}")
mapping <- as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))

# main ----------------------------

# numberca_dx non-integer
print(data %>%
  filter(numberca_dx == "LMS") %>%
  select(record_id_patient_id, numberca_dx))

# sample not available
for (sample_no in 1:10) {
  var <- glue("sample_id_{sample_no}")
  df_sid_na <- data %>%
    filter(!!as.symbol(var) == "Not available") %>%
    select(record_id_patient_id, !!as.symbol(var))
  
  if(nrow(df_sid_na)) {
    print(df_sid_na)
  }
}

# sample ID duplicated
mat <- c()
for (sample_no in 1:10) {
  var <- glue("sample_id_{sample_no}")
  df_id <- data %>%
    filter(!is.na(!!as.symbol(var))) %>%
    rename(sample_id = !!as.symbol(var)) %>%
    select(record_id_patient_id, sample_id)
  
  mat <- rbind(mat, cbind(rep(var, nrow(df_id)), as.matrix(df_id)))
}
colnames(mat)[1] <- "variable"
df_dup1 <- mat[which(duplicated(mat[,"sample_id"])), ]
df_dup2 <- mat[which(duplicated(mat[,"sample_id"], fromLast = T)), ]
print(rbind(df_dup1, df_dup2))

pref_sample_ids <- as.character(unlist(sapply(mat[,"sample_id"], add_genie_prefix)))
print(pref_sample_ids[which(duplicated(pref_sample_ids))])

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
