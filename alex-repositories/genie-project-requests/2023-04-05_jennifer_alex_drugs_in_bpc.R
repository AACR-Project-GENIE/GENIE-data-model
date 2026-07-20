 # Description:  Request from Jennifer Hoppe, on behalf of Jeremy Warner and 
#    Shawn Sweeney.  Create a list of all drugs used in the BPC projects using 
#    the regimen datasets.  For each drug used, state the BPC cohort it was observed 
#    in and whether it was observed as a regimen for a non-index or index cancer.   
#    Use the latest release available, whether public or consortium, in each case.  
#    The desired output format is excel/csv.
# Author: Alex Paynter

output_location_synid <- "syn51317177" # in 'GENIE BioPharma Collaborative Internal' > requests

library(synapser)
library(readxl)
library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)
library(magrittr)
library(glue)

synLogin()

# slightly altered to get tibbles. 
get_synapse_entity_data_in_csv <- function(synapse_id, 
                                           version = NA) {
  if (is.na(version)) {
    entity <- synGet(synapse_id)
  } else {
    entity <- synGet(synapse_id, version = version)
  }
  
  data <- read_csv(entity$path,
                   show_col_types = F,
                   progress = F)
  return(data)
}

syn_upload_help <- function(obj, name) {
  readr::write_csv(obj,
                   file = name,
                   na = "")
  synapser::File(name, parent = output_location_synid) %>%
    synStore()
}


# takes a list of character vectors.  It returns the unique sorted set of non-NA
#   values, separated by commas.
list_helper <- function(l) {
  c(l) %>%
    unlist %>%
    discard(., .p = is.na) %>%
    unique %>%
    sort %>%
    paste(., collapse = ", ")
}

# The synapse ID here should be the folder containing csv data, such as
#   cancer_level_dataset_index.csv
merge_one_folder <- function(synid_fold) {
  df_clin_children <- synGetChildren(synid_fold) %>%
    as.list %>%
    purrr::map_dfr(.x = .,
                   .f = as_tibble)
  
  
  syn_ca_index <- df_clin_children %>%
    filter(name %in% "cancer_level_dataset_index.csv") %>%
    pull(id)
  syn_ca_non_index <- df_clin_children %>%
    filter(name %in% "cancer_level_dataset_non_index.csv") %>% 
    pull(id)
  syn_reg <- df_clin_children %>%
    filter(name %in% "regimen_cancer_level_dataset.csv") %>% 
    pull(id)
  
  
  dft_ca_index <- get_synapse_entity_data_in_csv(syn_ca_index)
  dft_ca_non_index <- get_synapse_entity_data_in_csv(syn_ca_non_index)
  dft_regimen <- get_synapse_entity_data_in_csv(syn_reg)
  
  # A few messages just to get the rough idea:
  message(glue("Cohort: {first(dft_ca_index$cohort)}"))
  message(glue("Index dataset read in with {nrow(dft_ca_index)} rows."))
  message(glue("Non-index dataset read in with {nrow(dft_ca_non_index)} rows."))
  message(glue("Regimen dataset read in with {nrow(dft_regimen)} rows."))
  
  
  ca_comb <- dplyr::bind_rows(
    (dft_ca_index %>% 
       select(record_id, ca_seq) %>%
       mutate(index_ca = T)),
    (dft_ca_non_index %>%
       select(record_id, ca_seq, ca_d_site) %>%
       mutate(index_ca = F))
  )
  
  dft_reg_aug <- dft_regimen %>%
    left_join(., ca_comb, by = c("record_id", "ca_seq"))
  
  # for now we just want a list of the drugs used:
  dft_reg_aug %<>% 
    select(cohort, record_id, ca_seq, index_ca, ca_d_site,
           institution,
           contains("drugs_drug")) %>%
    pivot_longer(cols = -c(cohort, record_id, ca_seq, index_ca, 
                           ca_d_site, institution),
                 names_to = "drug_num",
                 values_to = "drug_name") %>%
    mutate(
      drug_num = stringr::str_replace(drug_num,
                                      "drugs_drug_",
                                      ""),
      drug_num = readr::parse_number(drug_num)
    ) 
  
  dft_reg_aug %<>%
    filter(!is.na(drug_name)) %>%
    # first get a 0/1 for each person:
    group_by(cohort, institution, drug_name, index_ca, record_id) %>%
    summarize(
      obs = 1,
      non_index_ca = list(ca_d_site),
      ca_seq = list(ca_seq),
      .groups = "drop"
    )
    
  return(dft_reg_aug)
}

# a interation dataframe where each row specifies a cohort and synid for the folder.
dft_folders <- tibble::tribble(
  ~cohort, ~synid,
  "BLADDER", "syn28495599", # 1.1 consortium
  "BrCa", "syn39802381", # 1.2 consortium
  "CRC", "syn39802279", # 2.0 public
  "NSCLC", "syn27245047", # 2.0 public 
  "PANC", "syn50612197", # 1.2 consortium
  "Prostate", "syn50612196", # 1.2 consortium
  # cohorts that had no available releases at the time:
  #   CRC2, ESOPHAGO, MELANOMA, NSCLC2, OVARIAN, RENAL
)


# Pull and merge the data associated with each synid in dft_folders.
dft_cohort_comb <- dft_folders %>%
  mutate(
    dat = purrr::pmap(
      .f = merge_one_folder,
      .l = list(synid_fold = synid)
    )
  ) %>%
  pull(dat) %>%
  dplyr::bind_rows(.)

# Sanity check:  We expect one row per {cohort, record_id, drug_name, index_ca}
#   at this stage:
if(
  (count(
    dft_cohort_comb, cohort, record_id, drug_name, index_ca, 
    sort = T) %>%
   pull(n) %>% 
   max(.,na.rm = T) %>%
   is_greater_than(.,1))
) {
  cli::cli_abort("Problem with the pulling done by merge_one_folder.")
}




   
# Request from May 18, 2023: Output a list of all participants with non-index
#  cancer drug regimens who had missing ca_d_site for that cancer.
dft_ca_d_miss <- dft_cohort_comb %>% 
  mutate(nic_blank = purrr::map_lgl(
    .x = non_index_ca,
    .f = (function(x) {
      all(is.na(x))
    })
  )) %>%
  filter(!index_ca) %>%
  filter(nic_blank) 

dft_ca_d_miss %<>% 
  # multiple cancer sequences can be associated with each row.  Make them a
  #   comma separated vector:
  group_by(record_id, drug_name) %>%
  mutate(ca_seq = list_helper(ca_seq)) %>%
  ungroup(.) %>%
  select(cohort, record_id, drug_name, ca_seq) %>%
  arrange(cohort, record_id, drug_name, ca_seq) %>%
  # clean up the drug names:
  mutate(
    drug_name = str_replace(drug_name, pattern = "\\(.*", ""),
    drug_name = str_trim(drug_name)
  )
  

syn_upload_help(obj = dft_ca_d_miss,
                name = "missing_ca_d_nonindex.csv")







# Back to the main goal:  The list of drugs

# Summarize over all participants
dft_cohort_comb %<>%
  group_by(cohort, institution, drug_name, index_ca) %>%
  summarize(
    n = sum(obs), # equivalent to n().
    non_ind_ca = list_helper(non_index_ca),
    .groups = "drop"
  )

dft_cohort_comb %<>%
  mutate(
    region = case_when(
      institution %in% c("DFCI", "MSK", "VICC") ~ "US",
      institution %in% c("UHN") ~ "Canada",
      T ~ "error"
    )
  )
if (any(dft_cohort_comb$region %in% "error")) {
  cli::cli_abort("Error on region list - were new sites added?")
}

# make this a function, so we can run it once for Canada and once for USA.
process_output <- function(dat) {
  # summarize over institutions:
  dat %<>%
    group_by(drug_name, cohort, index_ca) %>%
    # turn this back into a list:
    mutate(non_ind_ca = str_split(non_ind_ca, pattern = ", ")) %>%
    summarize(
      n = sum(n, na.rm = T),
      non_ind_ca = list_helper(non_ind_ca),
      .groups = 'drop'
    )
  
  # Pull out the non-index cancer list, we'll put it back later:
  ca_lists <- dat %>%
    filter(!index_ca) %>%
    select(drug_name, cohort, non_ind_ca)
  dat %<>% select(-non_ind_ca)
  
  # Pivot the data into one row per {drug_name, cohort}, and take the counts
  #   down to binary data.
  dat %<>%
    mutate(index_ca = if_else(index_ca, "index_n", "nonindex_n")) %>%
    pivot_wider(
      names_from = "index_ca",
      values_from = "n") %>%
    mutate(
      obs_index = if_else(index_n >= 1, 1, 0, 0),
      obs_nonindex = if_else(nonindex_n >= 1, 1, 0, 0)
    ) 
  
  # order it in the way that seems most helpful:
  dat %<>%
    select(drug_name, cohort, obs_index, obs_nonindex) %>%
    arrange(drug_name, cohort)
  
  rtn <- left_join(
    dat,
    ca_lists,
    by = c("drug_name", "cohort")
  ) %>%
    # just for consistency:
    mutate(non_ind_ca = if_else(non_ind_ca == "", NA_character_, non_ind_ca))
  
  # Formatting cleanup:
  rtn %<>%
    mutate(
      drug_name = str_replace(drug_name, pattern = "\\(.*", ""),
      drug_name = str_trim(drug_name),
      obs_index = if_else(obs_index %in% 1, "Yes", "No"),
      obs_nonindex = if_else(obs_nonindex %in% 1, "Yes", "No")
    )
  
  return(rtn)
}


dft_us <- dft_cohort_comb %>% 
  filter(region %in% "US") %>%
  process_output(.)

dft_canada <- dft_cohort_comb %>% 
  filter(region %in% "Canada") %>%
  process_output(.)



syn_upload_help(obj = dft_us,
                name = "drugs_by_cohort_us.csv")
syn_upload_help(obj = dft_canada,
                name = "drugs_by_cohort_canada.csv")







