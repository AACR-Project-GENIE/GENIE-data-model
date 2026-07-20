# Description: Determine sites with unmasked drugs.
# Author: Haley Hunter-Zinck
# Date: 2021-09-20
# Request:
#     Source: verbal request from Katya during drug masking meeting
#     Date of request: 2021-09-20
#     Message: "several unmasked investigational agents were noted: for example, GDC0941  and MK2206."  
#             Determine sites and record keys to report to sites for masking

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_tables_drug <- c("syn21446703.35", "syn21446703.37")

# parameters
drug_names_check <- c("GDC0941", "MK2206")
drug_names_control <- c("Investigational Drug", "Carboplatin")


# main ----------------------------

for (synid_table_drug in synid_tables_drug) {
  
  drug_columns <- c(paste0("drugs_drug_", c(1:5)), paste0("drugs_drug_oth_", c(1:5)))
  drug_columns_str <- paste0("'", paste0(drug_columns, collapse = "','"), "'")
  query <- glue("SELECT cohort, record_id, redcap_repeat_instance, redcap_data_access_group, {drug_columns_str} FROM {synid_table_drug}")
  data <- as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))
  
  # real check
  res_check <- data %>%
    filter(grepl(pattern = drug_names_check[1], x = drugs_drug_1) | grepl(pattern = drug_names_check[2], x = drugs_drug_1) |
             grepl(pattern = drug_names_check[1], x = drugs_drug_2) | grepl(pattern = drug_names_check[2], x = drugs_drug_2) |
             grepl(pattern = drug_names_check[1], x = drugs_drug_3) | grepl(pattern = drug_names_check[2], x = drugs_drug_3) |
             grepl(pattern = drug_names_check[1], x = drugs_drug_4) | grepl(pattern = drug_names_check[2], x = drugs_drug_4) |
             grepl(pattern = drug_names_check[1], x = drugs_drug_5) | grepl(pattern = drug_names_check[2], x = drugs_drug_5) |
             grepl(pattern = drug_names_check[1], x = drugs_drug_oth_1) | grepl(pattern = drug_names_check[2], x = drugs_drug_oth_1) |
             grepl(pattern = drug_names_check[1], x = drugs_drug_oth_2) | grepl(pattern = drug_names_check[2], x = drugs_drug_oth_2) |
             grepl(pattern = drug_names_check[1], x = drugs_drug_oth_3) | grepl(pattern = drug_names_check[2], x = drugs_drug_oth_3) |
             grepl(pattern = drug_names_check[1], x = drugs_drug_oth_4) | grepl(pattern = drug_names_check[2], x = drugs_drug_oth_4) |
             grepl(pattern = drug_names_check[1], x = drugs_drug_oth_5) | grepl(pattern = drug_names_check[2], x = drugs_drug_oth_5))
  
  # control
  res_control <- data %>%
    filter(grepl(pattern = drug_names_control[1], x = drugs_drug_1) | grepl(pattern = drug_names_control[2], x = drugs_drug_1) |
             grepl(pattern = drug_names_control[1], x = drugs_drug_2) | grepl(pattern = drug_names_control[2], x = drugs_drug_2) |
             grepl(pattern = drug_names_control[1], x = drugs_drug_3) | grepl(pattern = drug_names_control[2], x = drugs_drug_3) |
             grepl(pattern = drug_names_control[1], x = drugs_drug_4) | grepl(pattern = drug_names_control[2], x = drugs_drug_4) |
             grepl(pattern = drug_names_control[1], x = drugs_drug_5) | grepl(pattern = drug_names_control[2], x = drugs_drug_5) |
             grepl(pattern = drug_names_control[1], x = drugs_drug_oth_1) | grepl(pattern = drug_names_control[2], x = drugs_drug_oth_1) |
             grepl(pattern = drug_names_control[1], x = drugs_drug_oth_2) | grepl(pattern = drug_names_control[2], x = drugs_drug_oth_2) |
             grepl(pattern = drug_names_control[1], x = drugs_drug_oth_3) | grepl(pattern = drug_names_control[2], x = drugs_drug_oth_3) |
             grepl(pattern = drug_names_control[1], x = drugs_drug_oth_4) | grepl(pattern = drug_names_control[2], x = drugs_drug_oth_4) |
             grepl(pattern = drug_names_control[1], x = drugs_drug_oth_5) | grepl(pattern = drug_names_control[2], x = drugs_drug_oth_5))
  
  
  
  print(glue("{synid_table_drug}: Number of rows in control: {nrow(res_control)}"))
  print(glue("{synid_table_drug}: Number of rows in check: {nrow(res_check)}"))
  
  if(nrow(res_check) > 0) {
    to_print <- res_check %>% 
      select(cohort, record_id, redcap_repeat_instance, redcap_data_access_group)
    print(to_print)
  }
  print("------")
}

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
