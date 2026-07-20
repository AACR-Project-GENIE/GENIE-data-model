library(purrr)
library(here)
library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

samp <- readr::read_tsv(
  file = here('data-raw', 'main_genie', 'data_clinical_sample.txt'),
  comment = "#"
)

clin <- readr::read_tsv(
  file = here('data-raw', 'main_genie', 'data_clinical_patient.txt'),
  comment = "#"
)

# While we're here we'll create a version with all the clinical and genomic features:
samp %<>% rename_all(tolower)
clin %<>% rename_all(tolower)

samp %<>%
  select(-age_at_seq_report_days) %>%
  select(
    sample_id,
    patient_id,
    age_at_seq_report:sample_class
  )

clin %<>%
  # secondary/tertiary race are useless,so:
  select(patient_id, sex, primary_race, ethnicity:year_death)

# will need to fix up some of the age variables, but it's not important now so we'll wait.

samp_aug <- left_join(samp, clin, by = 'patient_id')

samp_aug %<>%
  mutate(
    age_at_seq_report_mod = case_when(
      age_at_seq_report %in% "<18" ~ 17,
      # so annoying, lots of these people are probably 89 not 90.
      age_at_seq_report %in% ">89" ~ 89,
      age_at_seq_report %in% "Unknown" ~ NA_real_,
      T ~ as.numeric(age_at_seq_report)
    )
  ) %>%
  relocate(
    age_at_seq_report_mod,
    .after = age_at_seq_report
  )

readr::write_rds(
  samp_aug,
  here('data', 'combined', 'samp_aug_proto.rds')
)
