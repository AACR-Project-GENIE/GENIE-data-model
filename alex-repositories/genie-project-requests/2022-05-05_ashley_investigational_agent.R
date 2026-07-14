# Description: Check for investigational agent form QA check flagged issues.
# Author: Haley Hunter-Zinck
# Date: 2022-05-05
# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)

# synapse
synid_file_data1 <- "syn26160187"
synid_file_data2 <- "syn26160184"
synid_file_header1 <- "syn25981842"
synid_file_header2 <- "syn25981841"

# parameters
instrument <- "ca_directed_drugs"
instance <- "1"
id_pat <- "GENIE-DFCI-038811"

# functions ----------------------------

#' Return a file upload data corresponding to a cohort-site pair as an R object.
#'
#' @param cohort cohort of interest
#' @param site site of interest
#' @param obj upload file object from config
#' @return Contents of a cohort-side data upload file as an R data frame.
#' @example
#' get_bpc_data_upload(cohort, site, list(data1 = "syn12345", data2 = "syn6554332", 
#'                           header1 = "syn39857289375, header2 = "syn11111"),
#' get_bpc_data_upload(cohort, site, list(data1 = "syn12345"))
get_bpc_data_upload <- function(synid_file_data1, 
                                synid_file_data2 = NULL, 
                                synid_file_header1 = NULL, 
                                synid_file_header2 = NULL) {
  
  data <- c()
  data1 <- c()
  data2 <- c()
  
  # get data 1 (default, should always be specified)
  ent <- synGet(synid_file_data1)
  data1 <- read.csv(ent$path, check.names = F,
                    na.strings = c(""), 
                    stringsAsFactors = F,
                    colClasses = "character")
  
  # check for header1
  if (length(synid_file_header1)) {
    ent <- synGet(synid_file_header1)
    colnames(data1) <- as.character(read.csv(ent$path, check.names = F,
                                             na.strings = c(""),
                                             stringsAsFactors = F,
                                             colClasses = "character"))
  }
  
  # check for data 2
  if (length(synid_file_data2)) {
    ent <- synGet(synid_file_data2)
    data2 <- read.csv(ent$path, check.names = F,
                      na.strings = c(""), 
                      stringsAsFactors = F,
                      colClasses = "character")
  }
  
  # check for header2
  if (length(synid_file_header2)) {
    ent <- synGet(synid_file_header2)
    colnames(data2) <- as.character(read.csv(ent$path, check.names = F,
                                             na.strings = c(""),
                                             stringsAsFactors = F,
                                             colClasses = "character"))
  }
  
  if (length(synid_file_data2)) {
    data <- data1 %>% inner_join(data2, by = c("record_id", 
                                               "redcap_repeat_instrument", 
                                               "redcap_repeat_instance"))
  } else {
    data <- data1
  }
  
  return(data)
}


# synapse login --------------------

status <- synLogin()

# read ----------------------------

data <- get_bpc_data_upload(synid_file_data1, 
                            synid_file_data2, 
                            synid_file_header1, 
                            synid_file_header2)


# main ----------------------------

data %>% 
  filter(redcap_repeat_instrument == instrument & redcap_repeat_instance == instance & record_id == id_pat) %>% 
  select(drugs_drug_1, drugs_ct_yn)

data %>% select(drugs_ct_yn) %>% distinct()

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
