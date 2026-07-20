library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

bpc_dat_path <- path('data-raw', 'NSCLC', '3.1-consortium')
ca_ind <- readr::read_csv(path(bpc_dat_path, 'cancer_level_dataset_index.csv'))
cohort <- ca_ind

# select just the stuff we need:
cohort %<>%
  select(
    cohort,
    record_id,
    institution,
    ca_seq
  )

# flow_track monitors attrition at each step for us.
flow_track <- flow_record_helper(cohort, "[1] BPC NSCLC v3.1")

cohort %<>% filter(!(institution %in% 'UHN'))
flow_track %<>% flow_record_helper(cohort, "[2] US cases only", .)

cpt <- readr::read_csv(path(
  bpc_dat_path,
  'cancer_panel_test_level_dataset.csv'
))
samp_kras_g12d <- readr::read_rds(path('data', 'genomic', 'samp_kras_g12d.rds'))
records_kras_g12d <- cpt %>%
  filter(cpt_genie_sample_id %in% samp_kras_g12d) %>%
  pull(record_id) %>%
  unique
cohort <- filter(cohort, record_id %in% records_kras_g12d)

flow_track %<>% flow_record_helper(cohort, "[3] KRAS G12D", .)

cohort <- cohort %>%
  filter(record_id %in% (get_dmet_time(ca_ind)$record_id))
flow_track %<>% flow_record_helper(cohort, "[4] Metastatic", .)

lot <- readr::read_rds(path('data', 'lot.rds'))
cohort <- left_join(
  cohort,
  filter(lot, line_of_therapy %in% 2),
  by = 'record_id'
)
cohort %<>% filter(!is.na(line_of_therapy))
# This point I'm making a call to take the first cancer sequence for everyone.
# Empirically there's one case where someone has two cancer and they occur on the same day.
# No reason to mess with join logic for that:
cohort %<>%
  group_by(record_id) %>%
  arrange(ca_seq) %>%
  slice(1) %>%
  ungroup(.)

flow_track %<>% flow_record_helper(cohort, "[5] Had 2L therapy", .)


drug_strings <- readr::read_rds(here('data', 'drug_string_matches.rds'))
cohort %<>%
  mutate(
    is_chemo = str_detect(
      tolower(regimen_drugs),
      paste(drug_strings$chemo, collapse = '|')
    ),
    is_anti_pd1 = str_detect(
      tolower(regimen_drugs),
      paste(drug_strings$anti_pd1, collapse = '|')
    )
  )


# directly add it into the flow tracker because these are now not-cumulative.
flow_track <- cohort %>%
  filter(is_chemo) %>%
  flow_record_helper(., "[6a] 2L contains chemo", flow_track)

flow_track <- cohort %>%
  filter(is_anti_pd1) %>%
  flow_record_helper(., "[6b] 2L contains anti-PD-(L)1", flow_track)

flow_track <- cohort %>%
  filter(is_chemo & is_anti_pd1) %>%
  flow_record_helper(., "[6c] 2L contains both", flow_track)

readr::write_rds(
  flow_track,
  path('data', 'flow_track.rds')
)
