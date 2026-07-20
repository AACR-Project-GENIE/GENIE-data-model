# Description: Examine time intervals for death and date of sequencing.
# Author: Haley Hunter-Zinck
# Date: 2021-11-19

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_table_sam <- "syn7517674"
synid_table_pat <- "syn7517669"

# functions ----------------------------

p_as_double <- function(x) {
  out <- tryCatch({
    as.double(x)
  }, error = function(cond){
    return(NA)
  }, warning = function(cond){
    return(NA)
  })
  
  return(out)
}

as_double <- function(x) {
  return(sapply(x, p_as_double))
}

# read ----------------------------

query <- glue("SELECT PATIENT_ID, INT_DOD, YEAR_DEATH FROM {synid_table_pat}")
data_pat <- as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))

query <- glue("SELECT PATIENT_ID, SAMPLE_ID, AGE_AT_SEQ_REPORT, SEQ_DATE FROM {synid_table_sam}")
data_sam <- as.data.frame(synTableQuery(query, includeRowIdAndRowVersion = F))

# main ----------------------------

# format
data_pat_mod <- data_pat %>% 
  mutate(INT_DOD_NA = as_double(INT_DOD)) %>%
  select(PATIENT_ID, INT_DOD_NA) %>%
  rename(INT_DOD = INT_DOD_NA)
data_sam_mod <- data_sam %>% 
  mutate(AGE_AT_SEQ_REPORT_NA = as_double(AGE_AT_SEQ_REPORT)) %>%
  select(PATIENT_ID, SAMPLE_ID, AGE_AT_SEQ_REPORT_NA) %>%
  rename(AGE_AT_SEQ_REPORT = AGE_AT_SEQ_REPORT_NA)

# res
data_res <- data_pat_mod %>%
  inner_join(data_sam_mod, by = "PATIENT_ID") %>%
  mutate(seq_before_death = is.na(INT_DOD) | is.na(AGE_AT_SEQ_REPORT) | INT_DOD > AGE_AT_SEQ_REPORT)

# check ---------------------------------

table(data_res$seq_before_death)
sum(table(data_res$seq_before_death)) == nrow(data_res)

na_pat <- data_pat %>% mutate(INT_DOD_NA = as_double(INT_DOD)) %>% filter(is.na(INT_DOD_NA)) %>% select(INT_DOD) %>% distinct()
na_sam <- data_sam %>% mutate(AGE_AT_SEQ_REPORT_NA = as_double(AGE_AT_SEQ_REPORT)) %>% filter(is.na(AGE_AT_SEQ_REPORT_NA)) %>% select(AGE_AT_SEQ_REPORT) %>% distinct()

data_pat %>% filter(is.element(INT_DOD, unlist(na_pat))) %>% select(INT_DOD, YEAR_DEATH) %>% distinct()
data_sam %>% filter(is.element(AGE_AT_SEQ_REPORT, unlist(na_sam))) %>% select(AGE_AT_SEQ_REPORT, SEQ_DATE) %>% distinct()

data_pat %>% filter(is.element(INT_DOD, unlist(na_pat))) %>% select(INT_DOD, YEAR_DEATH) %>% count()
data_sam %>% filter(is.element(AGE_AT_SEQ_REPORT, unlist(na_sam))) %>% select(AGE_AT_SEQ_REPORT, SEQ_DATE) %>% count()

data_pat %>% filter(is.element(INT_DOD, c(">32485"))) %>% select(INT_DOD, YEAR_DEATH) %>% count()
data_sam %>% filter(is.element(AGE_AT_SEQ_REPORT, c(">32485"))) %>% select(AGE_AT_SEQ_REPORT, SEQ_DATE) %>% count()

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
