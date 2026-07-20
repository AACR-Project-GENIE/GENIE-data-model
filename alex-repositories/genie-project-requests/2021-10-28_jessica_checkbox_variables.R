# Description: Determine number of choices per checkbox variable.  
# Author: Haley Hunter-Zinck
# Date: 2021-10-28

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_file_dd <- "syn26344720"
synid_folder_output <- "syn26162727"

# functions ----------------------------

trim <- function(str) {
  front <- gsub(pattern = "^[[:space:]]+", replacement = "", x = str)
  back <- gsub(pattern = "[[:space:]]+$", replacement = "", x = front)
  
  return(back)
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

get_num_choices_per_checkbox <- function(dd) {
  
  u_check <- unlist(dd %>%
                      filter(`Field Type` == "checkbox") %>%
                      select(`Variable / Field Name`))
  
  res <- matrix(NA, 
                nrow = length(u_check), 
                ncol = 2, 
                dimnames = list(c(), c("checkbox_variable", "num_choices")))
  
  for (i in 1:length(u_check)) {
    
    var_name <- u_check[i]
    choice_str <- unlist(dd %>% 
                           filter(`Variable / Field Name` == var_name) %>%
                           select(`Choices, Calculations, OR Slider Labels`))
    choice_code <- trim(unlist(lapply(strsplit(strsplit(choice_str, split = "\\|")[[1]], split = ", "), head, n = 1)))
    
    res[i, 1] <- var_name
    res[i, 2] <- length(choice_code)
  }
  
  return(res)
}

#' Store a file on Synapse with options to define provenance.
#' 
#' @param path Path to the file on the local machine.
#' @param parent_id Synapse ID of the folder or project to which to load the file.
#' @param file_name Name of the Synapse entity once loaded
#' @param prov_name Provenance short description title
#' @param prov_desc Provenance long description
#' @param prov_used Vector of Synapse IDs of data used to create the current
#' file to be loaded.
#' @param prov_exec String representing URL to script used to create the file.
#' @return TRUE if successful, otherwise return error.
save_to_synapse <- function(path, 
                            parent_id, 
                            file_name = NA, 
                            prov_name = NA, 
                            prov_desc = NA, 
                            prov_used = NA, 
                            prov_exec = NA) {
  
  if (is.na(file_name)) {
    file_name = path
  } 
  file <- File(path = path, parentId = parent_id, name = file_name)
  
  if (!is.na(prov_name) || !is.na(prov_desc) || !is.na(prov_used) || !is.na(prov_exec)) {
    act <- Activity(name = prov_name,
                    description = prov_desc,
                    used = prov_used,
                    executed = prov_exec)
    file <- synStore(file, activity = act)
  } else {
    file <- synStore(file)
  }
  
  return(T)
}

# read ----------------------------

dd <- get_synapse_entity_data_in_csv(synid_file_dd)

# main ----------------------------

res <- get_num_choices_per_checkbox(dd)
res <- res[order(as.double(res[,2]), decreasing = T), ]

# write --------------------------

file_local <- "2021-10-28_checkbox_variable_choice_count.csv"
write.csv(res, file = file_local, row.names = F)

save_to_synapse(path = file_local, 
                parent_id = synid_folder_output,
                prov_name = "checkbox choice counts", 
                prov_desc = "checkbox variable counts in the latest BLADDER BPC data dictionary", 
                prov_used = synid_file_dd, 
                prov_exec = "https://github.com/hhunterzinck/genie_requests/blob/main/2021-10-28_jessica_checkbox_variables.R") 
  
file.remove(file_local)

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
