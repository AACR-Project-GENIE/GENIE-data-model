library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

cohort <- readr::read_rds(
  here('data', 'cohort', 'cohort_part_1.rds')
)
flow_track <- readr::read_rds(
  here('data', 'cohort', 'flow_track_part_1.rds')
)
index_therapies <- readr::read_rds(
  here('data', 'cohort', 'index_therapies.rds')
)
lot <- readr::read_rds(
  here('data', 'cohort', 'lot.rds')
)
rad <- readr::read_csv(
  here('data-raw', 'bpc', 'NSCLC', '3.1-consortium', 'ca_radtx_dataset.csv')
)

prev_ther_checks <- lot %>%
  group_by(record_id, ca_seq) %>%
  summarize(
    had_plat = max(cumsum_plat) >= 1,
    had_io = max(cumsum_io) >= 1,
  )

cohort %<>%
  left_join(
    .,
    prev_ther_checks,
    by = c('record_id', 'ca_seq')
  )

# The people with NA for these variables were not in the lines dataset, meaning
#   they didn't have any therapies that counted as lines.
cohort %<>% replace_na(list(had_plat = FALSE, had_io = FALSE))

cohort %<>% filter(had_plat)
flow_track %<>% flow_record_helper(cohort, "Prior platinum exposure", .)
cohort %<>% filter(had_io)
flow_track %<>% flow_record_helper(cohort, "Prior IO exposure", .)

cohort %<>%
  left_join(
    .,
    index_therapies,
    by = c('record_id', 'ca_seq')
  )

cohort %<>% filter(!is.na(index_line))
flow_track %<>% flow_record_helper(cohort, "Has index therapy", .)

cohort %<>%
  mutate(
    dob_cpt_rep_days = dob_ca_dx_days + dx_cpt_rep_days,
    cpt_within_index_window = (dob_cpt_rep_days - index_dob_reg_start_int) < 30
  ) %>%
  relocate(.after = dx_cpt_rep_days, dob_cpt_rep_days, cpt_within_index_window)

cohort %<>% filter(cpt_within_index_window)
flow_track %<>% flow_record_helper(cohort, "NGS within window", .)

cohort %<>% filter(no_tki_to_index)
flow_track %<>% flow_record_helper(cohort, "No TKI before index", .)

cohort %<>% filter(no_inv_to_index)
flow_track %<>% flow_record_helper(cohort, "No investigational before index", .)


# Could possibly put this in a separate script..
rad_overlap <- rad %>%
  # no idea why ther are missing radiation durations but...
  replace_na(list(rt_rt_int = 0)) %>%
  mutate(
    dx_rt_end_days = dx_rt_start_days + rt_rt_int
  ) %>%
  left_join(
    select(cohort, record_id, ca_seq, dob_ca_dx_days, index_dob_reg_start_int),
    .,
    by = c('record_id', 'ca_seq'),
    relationship = 'one-to-many'
  )

rad_overlap %<>%
  mutate(
    dob_rt_start_int = dx_rt_start_days + dob_ca_dx_days,
    dob_rt_end_int = dx_rt_end_days + dob_ca_dx_days,
    rt_start_index_days = index_dob_reg_start_int - dob_rt_start_int,
    rt_end_index_days = index_dob_reg_start_int - dob_rt_end_int,
    overlap_case_1 = rt_start_index_days >= 0 & rt_start_index_days <= 21,
    overlap_case_2 = rt_end_index_days >= 0 & rt_end_index_days <= 21,
    overlap = overlap_case_1 | overlap_case_2
  )

rad_overlap_sum <- rad_overlap %>%
  group_by(record_id, ca_seq) %>%
  summarize(no_rad_overlap = !any(overlap, na.rm = T))

cohort %<>%
  left_join(., rad_overlap_sum, by = c('record_id', 'ca_seq')) %>%
  replace_na(list(no_rad_overlap = TRUE))

cohort %<>% filter(no_rad_overlap)
flow_track %<>% flow_record_helper(cohort, "No radiation in window", .)

readr::write_rds(
  cohort,
  here('data', 'cohort', 'cohort.rds')
)
readr::write_rds(
  flow_track,
  here('data', 'cohort', 'flow_track.rds')
)
