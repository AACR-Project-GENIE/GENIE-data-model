# Description: A sandbox for looking at and playing with the datasets.
#   This script will never be a part of the final analysis workflow.
# Author: Alex Paynter

library(cli) # prevents an error on version reliance.
library(readr)
library(vctrs)
library(rlang)
library(here)
library(dplyr)
library(tidyr)
library(magrittr)
library(janitor)
library(glue)
library(genieBPC)
library(ggplot2)

read_wrap <- function(p) {
  read_csv(file = here("data-raw", p), show_col_types = F)
}

dft_ca_radtx <- read_wrap("ca_radtx_dataset.csv")
dft_ca_ind <- read_wrap("cancer_level_dataset_index.csv")
dft_ca_nonind <- read_wrap("cancer_level_dataset_non_index.csv")
dft_ca_panel <- read_wrap("cancer_panel_test_level_dataset.csv")

dft_img <- read_wrap("imaging_level_dataset.csv")
dft_med_onc <- read_wrap("med_onc_note_level_dataset.csv")
dft_path <- read_wrap("pathology_report_level_dataset.csv")

dft_pt <- read_wrap("patient_level_dataset.csv")
dft_regimen <- read_wrap("regimen_cancer_level_dataset.csv")
dft_tm <- read_wrap("tm_level_dataset.csv")

glimpse(dft_ca_radtx)
glimpse(dft_ca_ind)
glimpse(dft_ca_panel)
glimpse(dft_img)
glimpse(dft_med_onc)
glimpse(dft_path)
glimpse(dft_regimen)
glimpse(dft_tm)


# 841 rows, 7 cols, 000183 is the first pt.


# Time variables:
# hybrid_death_int - interval in days from date of birth to date of death.
# dob_ca_dx_days - interval in days from date of birth to cancer dx.
#   - Called [ca_cadx_int] in the brca cohort data.
# tt_os_dx_days - interval from diagnosis to death in days.

  

# Start of the code needed to do dmets timing by site:
dft_ca_ind %>%
  select(record_id, 
         ca_seq,
         stage_dx_iv, # stage at diagnosis
         # ca_cadx_int,
         ca_dmets_yn,
         contains("ca_first_dmets")) %>%
  filter(stage_dx_iv %in% "Stage IV") 
# from here timing is easy - it's at diagnosis.

dft_ca_ind %>%
  select(record_id, 
         ca_seq,
         stage_dx_iv, # stage at diagnosis
         dmets_stage_i_iii, #y/n dmets after dx for early stage.
         matches("^dmets_[a-z]*"),
         matches("^dx_to_dmets.*yrs$")) %>%
  filter(!(stage_dx_iv %in% c("Stage IV"))) %>%
  select(record_id, ca_seq, matches("^dx_to_dmets.*yrs$"))




    

  
  
  
  dft_ca_ind %>%
    select(record_id, 
           ca_seq,
           stage_dx_iv, # stage at diagnosis
           dmets_stage_i_iii, #y/n dmets after dx for early stage.
           matches("^dmets_[a-z]*"),
           matches("^dx_to_dmets.*yrs$")) %>%
    filter(!(stage_dx_iv %in% c("Stage IV"))) %>%
    select(record_id, ca_seq, matches("^dx_to_dmets.*yrs$"))
  
}


  
  
  
get_progressed_timing <- function(ca_dat, prefix) {
  
  stat_var <- paste0(prefix, '_status')
  tt_d <- paste0('tt_', prefix, '_days')
  tt_m <- paste0('tt_', prefix, '_mos')
  tt_y <- paste0('tt_', prefix, '_yrs')
  
  ca_dat %<>%
    as_tibble(.) %>%
    filter(.data[[stat_var]] %in% 1) %>%
    # A couple of quick variables to make the filter more readable:
    mutate(
      did_not_die = !(os_dx_status %in% 1),
      pfs_time_lt_os_time = case_when(
        is.na(.data[[tt_d]]) ~ T,
        is.na(tt_os_dx_days) ~ T,
        .data[[tt_d]] < tt_os_dx_days ~ T,
        T ~ F
      )
    ) %>% 
    filter(did_not_die | pfs_time_lt_os_time) %>%
    mutate(
      # we give these arbitrary names for use in future functions
      tt_d = .data[[tt_d]],
      tt_m = .data[[tt_m]],
      tt_y = .data[[tt_y]],
      fn_prefix = prefix # save the prefix input.
    ) %>%
    select(record_id,  ca_seq, tt_d, tt_m, tt_y, fn_prefix)
  
  # For cases where we have more than one row in ca_dat (rare but real), 
  #  we can sensibly grab only the first one.
  ca_dat %<>% 
    group_by(record_id) %>%
    arrange(tt_d) %>%
    slice(1) %>%
    ungroup
  
  return(ca_dat)
}


