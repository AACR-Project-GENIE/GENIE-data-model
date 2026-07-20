# Description: Comapre the Data elements catalog and mapping to find discrepancies.
# Author: Haley Hunter-Zinck
# Date: 2022-04-06

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)

# synapse
synid_table_cata <- "syn21431364"
synid_table_cbio <- "syn25712693"

# parameters
cohorts = c("BrCa", "CRC", "NSCLC", "PANC", "Prostate")

# synapse login --------------------

synLogin()

# main ----------------------------

# compare variable names
for (cohort in cohorts) {
  # cbio mapping variables released
  query <- glue("SELECT code FROM {synid_table_cbio} WHERE {cohort} = 'true' AND cbio <> 'EVENT_TYPE' AND data_type <> 'portal_value'")
  var_cbio <- as.character(unlist(as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))))
  
  # catalog variables released
  query <- glue("SELECT variable FROM {synid_table_cata} WHERE {cohort}_sor <> 'private'")
  var_cata <- as.character(unlist(as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))))
  
  
  # print discrepancies
  print(glue("cohort: {cohort}"))
  missing_var <- setdiff(var_cbio, var_cata)
  print(grep(pattern = "_\\*$", x = missing_var, value = T, invert = T))
  print("--------------")
}

# compare queries
for (cohort in cohorts) {
  
  # catalog variables released
  query <- glue("SELECT variable FROM {synid_table_cata} WHERE {cohort}_sor IN ('project', 'consortium', 'public')")
  data_elements <- as.character(unlist(as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))))
  
  
  # cbio mapping variables df with catalog reference
  data_elements_str <- paste0(data_elements, collapse = "','")
  query_1 <- glue("SELECT * FROM {synid_table_cbio} WHERE (code IN ('{data_elements_str}') OR cbio = 'EVENT_TYPE' or sampleType in ('TIMELINE-TREATMENT', 'TIMELINE-TREATMENT-RT') and data_type <> 'heme') and {cohort} is true")
  df_cbio_1 <- as.data.frame(synTableQuery(query_1, includeRowIdAndRowVersion = F))
  
  # cbio mapping variables df without catalog reference
  query_2 <- glue("SELECT * FROM {synid_table_cbio} WHERE {cohort} is true AND sampleType <> 'TIMELINE-STATUS'")
  df_cbio_2 <- as.data.frame(synTableQuery(query_2, includeRowIdAndRowVersion = F))
  
  # print discrepancies
  missing <- setdiff(df_cbio_1$code, df_cbio_2$code)
  extra <- setdiff(df_cbio_2$code, df_cbio_1$code)
}

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
