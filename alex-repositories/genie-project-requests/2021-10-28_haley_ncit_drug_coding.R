# Description: Prototype mapping regimens to NCIT drug codes
# Author: Haley Hunter-Zinck
# Date: 2021-10-28

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# parameters
my_cohort <- "Prostate"

# synapse
synid_table_prissmm <- "syn22684834"
synid_file_drug <- "syn22296818"
synid_file_grs <- "syn24184523"

# functions ----------------------------

#' Get Synapse IDs of the most recent PRISSMM documentation files
#' for a cohort.
#'
#' @param synid_table_prissmm Synapse ID of the table containing Synapse IDs
#' of all PRISSM documentation
#' @param cohort cohort of interest
#' @file_name name of the type of documentation file of interest
#' @return Synapse ID of the most recent PRISSMM documentation corresponding
#' to the cohort and type of document.
#' @example
#' get_bpc_synid_prissmm("syn12345", "my_cohort", "Import Template")
get_bpc_synid_prissmm <- function(synid_table_prissmm, cohort,
                                  file_name = c("Import Template", "Data Dictionary non-PHI")) {
  
  query <- glue("SELECT id FROM {synid_table_prissmm} \
                WHERE cohort = '{cohort}' \
                ORDER BY name DESC LIMIT 1")
  synid_folder_prissmm <- as.character(unlist(as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))))
  
  synid_prissmm_children <- as.list(synGetChildren(synid_folder_prissmm))
  synid_prissmm_children_ids <- setNames(unlist(lapply(synid_prissmm_children,
                                                       function(x) {return(x$id)})),
                                         unlist(lapply(synid_prissmm_children,
                                                       function(x) {return(x$name)})))
  
  return(as.character(synid_prissmm_children_ids[file_name]))
}

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

trim <- function(str) {
  front <- gsub(pattern = "^[[:space:]]+", replacement = "", x = str)
  back <- gsub(pattern = "[[:space:]]+$", replacement = "", x = front)
  
  return(back)
}

shorten_drug_names <- function(full_drug_name) {
  return(trim(unlist(lapply(strsplit(x = full_drug_name, split = "(", fixed = T), head, n = 1))))
}

get_drug_map <- function(primary, secondary = NULL) {
  
  
  choice_code <- c()
  choice_label <- c()
  dictionaries <- list()
  
  if (is.null(secondary)) {
    dictionaries <- list(primary)
  } else {
    dictionaries <- list(primary, secondary)
  }
  
  var_names <- c(paste0("drugs_drug_", c(1:5)), paste0("drugs_drug_oth", c(1:5)))
  for (var_name in var_names) {
    
    for (obj in dictionaries) {
      choice_str <- unlist(obj %>% 
                             filter(`Variable / Field Name` == "drugs_drug_1") %>%
                             select(`Choices, Calculations, OR Slider Labels`))
      choice_str <- gsub(pattern = "\"", replacement = "", x = choice_str)
      
      choice_code <- c(choice_code, 
                       trim(unlist(lapply(strsplit(strsplit(choice_str, split = "\\|")[[1]], split = ", "), head, n = 1))))
      
      choice_value <- trim(unlist(lapply(strsplit(strsplit(choice_str, split = "\\|")[[1]], split = ", "), function(x) {return(x[2])})))
      choice_label <- c(choice_label, shorten_drug_names(choice_value))
    }
  }
  
  map <- setNames(choice_code, choice_label)

  idx <- which(duplicated(map))
  if (length(idx)) {
    return(map[-idx])
  }
  return(map)
}

get_regimen_abbrev <- function(regimen, map) {
  drugs <- trim(unlist(strsplit(regimen, split = ",")))
  codes <- map[drugs]
  abbrev <- paste0(codes, collapse = "_")
  
  return(abbrev)
}

# read ----------------------------

synid_file_dd <- get_bpc_synid_prissmm(synid_table_prissmm, cohort = my_cohort,
                                        file_name = "Data Dictionary non-PHI")
dd <- get_synapse_entity_data_in_csv(synid_file_dd)

grs <- get_synapse_entity_data_in_csv(synid_file_grs)
colnames(grs) <- c("Variable / Field Name", "Choices, Calculations, OR Slider Labels")

data <- get_synapse_entity_data_in_csv(synid_file_drug)

# main ----------------------------

map <- get_drug_map(primary = grs, secondary = dd)
u_regimen <- unlist(data %>% 
  filter(cohort == my_cohort) %>%
  filter(redcap_ca_index == "Yes") %>%
  filter(!grepl(pattern = "Investigational Drug", x = regimen_drugs)) %>%
  filter(!grepl(pattern = "Other", x = regimen_drugs)) %>%
  select(regimen_drugs) %>%
  distinct())

regimen_abbrev <- c()
for (i in 1:length(u_regimen)) {
  regimen_abbrev[i] <- get_regimen_abbrev(u_regimen[i], map = map)
}

# validation --------------------

idx <- grep(pattern = "NA", x = regimen_abbrev)
if (length(idx)) {
  print("ISSUE: unmapped drug names found")
  print(cbind(regimen_abbrev[idx], u_regimen[idx]))
} else {
  print("PASS: no unmapped drug names!")
}

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
