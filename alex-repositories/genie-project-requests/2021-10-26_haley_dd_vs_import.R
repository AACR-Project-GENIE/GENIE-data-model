# Description: Figure out the difference between the import template and data dictionary variable lists.
# Author: Haley Hunter-Zinck
# Date: 2021-10-26

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_file_dd <- "syn25570252"
synid_file_import <- "syn25575660"

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

create_import_template_ordered <- function(dd) {
  
  template <- c()
  
  if (dd$`Variable / Field Name`[1] != "record_id") {
    return(NULL)
  }
  template <- c("record_id")
  
  # rcc columns
  template <- c(template, "redcap_repeat_instrument", "redcap_repeat_instance", "redcap_data_access_group")
  
  # iterate through variables
  form_prev <- dd$`Form Name`[2]
  for (i in 2:length(dd$`Variable / Field Name`)) {
    
    form_curr <- dd$`Form Name`[i]
    var_name <- dd$`Variable / Field Name`[i]
    
    # add complete column
    if (form_curr != form_prev) {
      template <- c(template, paste0(form_prev, "_complete"))
    }
    
    # if check box
    if (dd$`Field Type`[i] == "checkbox") {
      choice_str <- unlist(dd %>% 
                             filter(`Variable / Field Name` == var_name) %>%
                             select(`Choices, Calculations, OR Slider Labels`))
      choice_code <- trim(unlist(lapply(strsplit(strsplit(choice_str, split = "\\|")[[1]], split = ", "), head, n = 1)))
      template <- c(template, paste0(var_name, "___", choice_code))
    } else {
      template <- c(template, var_name)
    }
    
    form_prev <- form_curr
  }
  
  # add last complete columns
  form_curr <- tail(dd$`Form Name`, 1)
  template <- c(template, paste0(form_prev, "_complete"))
  
  return(template)
}

create_import_template <- function(dd) {
  
  # rcc columns
  template <- c("redcap_repeat_instrument", "redcap_repeat_instance", "redcap_data_access_group")
  
  # complete columns
  u_form <- unlist(dd %>% 
                      select(`Form Name`) %>%
                      distinct())
  template <- c(template, paste0(u_form, "_complete"))
  
  # non-checkbox variables
  u_noncheck <- unlist(dd %>%
                         filter(`Field Type` != "checkbox") %>%
                         select(`Variable / Field Name`))
  template <- c(template, u_noncheck)
    
  # expand all checkbox variables
  u_check <- unlist(dd %>%
                      filter(`Field Type` == "checkbox") %>%
                      select(`Variable / Field Name`))
  for (var_name in u_check) {
    choice_str <- unlist(dd %>% 
                           filter(`Variable / Field Name` == var_name) %>%
                           select(`Choices, Calculations, OR Slider Labels`))
    choice_code <- trim(unlist(lapply(strsplit(strsplit(choice_str, split = "\\|")[[1]], split = ", "), head, n = 1)))
    template <- c(template, paste0(var_name, "___", choice_code))
  }
  
  # sort according to data dictionary order
  
  
  return(template)
}

# read ----------------------------

dd <- get_synapse_entity_data_in_csv(synid_file_dd)
import <- unlist(get_synapse_entity_data_in_csv(synid_file_import, header = F))

# main ----------------------------

import_prime <- create_import_template(dd)
import_ordered <- create_import_template_ordered(dd)


setdiff(import, import_prime)
setdiff(import_prime, import)

# checks --------------------------

var_name <- "rt_qaminor"
dd %>% 
  filter(`Variable / Field Name` == var_name) %>%
  select(`Choices, Calculations, OR Slider Labels`)

identical(as.character(import[-which(is.element(import, setdiff(import, import_prime)))]), 
          as.character(import_ordered))

write(import_ordered, sep = ",", ncolumns = length(import_ordered), file = "test.csv")

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
