library(purrr); library(fs); library(here)
purrr::walk(.x = fs::dir_ls('R'), .f = source)

dft_med_onc <- readr::read_csv(
  here('data-raw', 'med_onc_note_level_dataset.csv')
)

dft_med_onc %<>%
  augment_med_onc_imputed_ecog(.) %>%
  filter(md_ecog_imputed != "Not documented in note") 

dft_imputed_ecog <- dft_med_onc %>% 
  select(cohort, phase, record_id, md_visit_number, md_onc_visit_int,
         md_ecog_imputed, md_ecog_imp_source)

readr::write_rds(
  dft_imputed_ecog,
  file = here('data', 'ecog_imputed_not_missing.rds')
)
