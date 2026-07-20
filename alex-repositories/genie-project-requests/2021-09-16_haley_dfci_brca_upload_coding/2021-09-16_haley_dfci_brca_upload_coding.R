# Description: Determine if DFCI coding for the ca_drugs___1 variables in the CA Directed Drugs table changed
#     in the most recent upload.
# Author: Haley Hunter-Zinck
# Date: 2021-09-16
# Request:
#   Source: Freedcamp comment discussion with Jessica Lavery after issue with running the derived variable code
#   Date of request: 2021-09-16
#   Message: For example, in the Aug data cut, the variables with the prefix drugs_ca on the 
#             ca_drugs data had values of 1, 2, 3, etc. and were missing if they were not checked. 
#             However, in the Sept tables, the values of each drugs_ca column has 0's instead of unknowns. 

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(synapser)
synLogin()
library(yaml)
library(dplyr)

# synapse
synid_table_ca_drugs <- "syn21446703"

# config
config <- read_yaml("config.yaml")

# constants
instrument <- "ca_directed_drugs"
cohort = "BrCa"
values <- c("0", "1", NA)
sites <- c("DFCI", "MSK", "VICC")

# functions ----------------------------

get_bpc_data_upload <- function(cohort, site, obj) {
  
  data <- c()
  data1 <- c()
  data2 <- c()
  
  # get data 1 (default, should always be specified)
  ent <- synGet(obj$data1)
  data1 <- read.csv(ent$path, 
                    check.names = F,
                    na.strings = c(""), 
                    stringsAsFactors = F)
  
  # check for header1
  if (length(obj$header1)) {
    ent <- synGet(obj$header1)
    colnames(data1) <- as.character(read.csv(ent$path, check.names = F,
                                             na.strings = c(""),
                                             stringsAsFactors = F))
  }
  
  # check for data 2
  if (length(obj$data2)) {
    ent <- synGet(obj$data2)
    data2 <- read.csv(ent$path, check.names = F,
                      na.strings = c(""), 
                      stringsAsFactors = F)
  }
  
  # check for header2
  if (length(obj$header2)) {
    ent <- synGet(obj$header2)
    colnames(data2) <- as.character(read.csv(ent$path, check.names = F,
                                             na.strings = c(""),
                                             stringsAsFactors = F))
  }
  
  if (length(obj$data2)) {
    data <- data1 %>% inner_join(data2, by = c("record_id", 
                                               "redcap_repeat_instrument", 
                                               "redcap_repeat_instance"))
  } else {
    data <- data1
  }
  
  return(data)
}

get_table <- function(vec, levels) {
  return(table(factor(vec, levels = levels), useNA = "always"))
}

# read ----------------------------

data_current <- list()
data_previous <- list()
for (site in sites) {
  data_current[[site]] <- get_bpc_data_upload(cohort = "BrCa", 
                                           site = "DFCI", 
                                           obj = config$upload_current$BrCa[[site]])
  
  if (site != "VICC") {
    data_previous[[site]] <- get_bpc_data_upload(cohort = "BrCa", 
                                                 site = "DFCI", 
                                                 obj = config$upload_previous$BrCa[[site]])
  }
}



# main ----------------------------

tab_drugs_ca <- c()
tab_drugs_dc_reason <- c()
p_tab_drugs_ca <- c()
p_tab_drugs_dc_reason <- c()
for (site in sites) {
  
  var <- data_current[[site]] %>% 
    filter(redcap_repeat_instrument == instrument) %>%
    select("drugs_ca___1", "drugs_dc_reason___1")
  
  tab_drugs_ca  <- rbind(tab_drugs_ca, c(cohort, site, get_table(unlist(var$drugs_ca___1), 
                                                                 levels = values)))
  tab_drugs_dc_reason <-  rbind(tab_drugs_dc_reason, c(cohort, site, get_table(unlist(var$drugs_dc_reason___1), 
                                                                        levels = values)))
  
  if (site != "VICC") {
    var <- data_previous[[site]] %>% 
      filter(redcap_repeat_instrument == instrument) %>%
      select("drugs_ca___1", "drugs_dc_reason___1")
    
    p_tab_drugs_ca  <- rbind(p_tab_drugs_ca, c(cohort, site, get_table(unlist(var$drugs_ca___1), 
                                                                   levels = values)))
    p_tab_drugs_dc_reason <-  rbind(p_tab_drugs_dc_reason, c(cohort, site, get_table(unlist(var$drugs_dc_reason___1), 
                                                                                 levels = values)))
    
  }
}

colnames(tab_drugs_ca)[1:2] <- c("cohort", "site")
colnames(tab_drugs_dc_reason)[1:2] <- c("cohort", "site")
colnames(p_tab_drugs_ca)[1:2] <- c("cohort", "site")
colnames(p_tab_drugs_dc_reason)[1:2] <- c("cohort", "site")

# summary -------------------------------

# current
print("Current:")
print(tab_drugs_ca)
print(tab_drugs_dc_reason)

# previous
print("Previous:")
print(p_tab_drugs_ca)
print(p_tab_drugs_dc_reason)


# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
