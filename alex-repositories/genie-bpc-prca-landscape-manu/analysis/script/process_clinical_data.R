# Description:  Filters the data to only index cancer rows for applicable datasets.
#.  We programatically check some basic assumptions about the data and throw errors
#.  if something looks off.
#   Additional filters may be added later on.

library(purrr); library(fs); library(here)
# Load all helper functions
purrr::walk(.x = fs::dir_ls('R'), .f = source)

read_wrap <- function(p) {
  read_csv(file = here("data-raw", p), show_col_types = F)
}

dft_pt <- read_wrap('patient_level_dataset.csv')
dft_ca_ind <- read_wrap('cancer_level_dataset_index.csv')
dft_cpt <- read_wrap('cancer_panel_test_level_dataset.csv')
dft_img <- read_wrap('imaging_level_dataset.csv')
dft_med_onc <- read_wrap('med_onc_note_level_dataset.csv')
dft_path <- read_wrap('pathology_report_level_dataset.csv')
dft_reg <- read_wrap('regimen_cancer_level_dataset.csv')
dft_tm <- read_wrap('tm_level_dataset.csv')
dft_rad <- read_wrap('ca_radtx_dataset.csv')

fs::dir_create(here('data', 'clin'))

# Assumption checking:
if ((dft_pt$record_id %>% duplicated %>% any)) {
  cli_abort("Duplicated records in patient level dataset.")
}

if (any(dft_pt$naaccr_sex_code != "Male")) {
  cli_alert_danger("Non-male sex detected for at least one participant (unexpectedly for Prostate cancer)")
}
# The prostate cancer data does not have duplicate rows in the cancer index data:
if (any(pull(count(dft_ca_ind, record_id), n) > 1, na.rm = T)) {
  cli_abort("Duplicate record IDs in index cancer data - need to fix.")
}





dft_cohort_keys <- dft_ca_ind %>% select(record_id, ca_seq)
chk_keys_unique <- count(dft_cohort_keys, record_id, ca_seq) %>%
  pull(n) %>% 
  max %>%
  is_in(1)
if (!chk_keys_unique) {
  cli::cli_abort("Duplicate keys found in filter_data_for_cohort.R")
}

key_filt_help <- function(dat) {
  left_join(
    dft_cohort_keys,
    dat,
    by = c("record_id", "ca_seq"),
    multiple = "all" # default behavior in SQL and dplyr - just silences.
  )
}

n_row_cpt_old <- nrow(dft_cpt)
n_row_reg_old <- nrow(dft_reg)
n_row_rad_old <- nrow(dft_rad)

dft_cpt %<>% key_filt_help(.)
dft_reg %<>% key_filt_help(.)
dft_rad %<>% key_filt_help(.)

cli_alert_info(glue("{nrow(dft_cpt)-n_row_cpt_old} rows removed from dft_cpt for being related to non index cancers"))
cli_alert_info(glue("{nrow(dft_reg)-n_row_reg_old} rows removed from dft_reg for being related to non index cancers"))
cli_alert_info(glue("{nrow(dft_rad)-n_row_rad_old} rows removed from dft_rad for being related to non index cancers"))


# Create additional derived variables.
lev_st_simple <- c("Primary tumor", "Metastatic", "Other")
dft_cpt %<>%
  mutate(
    sample_type_simple_f = case_when(
      is.na(sample_type) ~ NA_character_,
      sample_type %in% "Local recurrence" ~ lev_st_simple[3],
      sample_type %in% "Lymph node metastasis" ~ lev_st_simple[2],
      sample_type %in% "Metastasis site unspecified" ~ lev_st_simple[2],
      sample_type %in% "Not applicable or hematologic malignancy" ~ lev_st_simple[3],
      sample_type %in% "Primary tumor" ~ lev_st_simple[1],
      T ~ NA_character_,
    ),
    sample_type_simple_f = factor(sample_type_simple_f, levels = lev_st_simple)
  )
      
      


# Not a best practice to use the names, but it will work.
write_help <- function(dat) {
  nm <- deparse(substitute(dat))
  readr::write_rds(
    x = dat,
    file = here('data', 'clin', 
                paste0(nm,'.rds'))
  )
}

write_help(dft_pt)
write_help(dft_ca_ind)
write_help(dft_img)
write_help(dft_med_onc)
write_help(dft_path)
write_help(dft_tm)
write_help(dft_cpt)
write_help(dft_reg)
write_help(dft_rad)



