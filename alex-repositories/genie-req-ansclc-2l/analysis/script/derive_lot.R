library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

reg <- readr::read_csv(
  here(
    'data-raw',
    'bpc',
    'NSCLC',
    '3.1-consortium',
    'regimen_cancer_level_dataset.csv'
  )
)

coh <- readr::read_rds(
  here('data', 'cohort', 'cohort_part_1.rds')
)

reg <- left_join(
  filter(reg, record_id %in% coh$record_id),
  (coh %>%
    select(phase, cohort, record_id, ca_seq, dx_cpt_rep_days)),
  by = c('phase', 'cohort', 'record_id', 'ca_seq')
)

lot <- basic_line_of_therapy(
  start_counting_df = select(
    coh,
    'record_id',
    'ca_seq',
    dob_start_days = dob_ca_dx_days
  ),
  reg,
  remove_duplicates = TRUE,
  verbose = T
)

lot %<>%
  mutate(
    is_plat = str_detect(regimen_drugs, "Cisplatin|Carboplatin"),
    # inductive list:
    is_io = str_detect(
      regimen_drugs,
      "Pembro|Nivolumab|Atezolizumab|Ipilimumab"
    )
  )

lot %<>%
  group_by(record_id, ca_seq) %>%
  arrange(line_of_therapy) %>%
  mutate(
    cumsum_plat = cumsum(is_plat),
    cumsum_io = cumsum(is_io),
    has_both_prev_ther = cumsum_plat >= 1 & cumsum_io >= 1,
    had_both_prev_ther = lag(has_both_prev_ther, default = F)
  ) %>%
  ungroup(.)

readr::write_rds(
  lot,
  here('data', 'cohort', 'lot.rds')
)

index_therapies <- lot %>%
  filter(had_both_prev_ther) %>%
  group_by(record_id, ca_seq) %>%
  slice(1) %>%
  ungroup(.) %>%
  select(
    record_id,
    ca_seq,
    index_line = line_of_therapy,
    index_drugs = regimen_drugs,
    index_regimen = regimen_number,
    index_dob_reg_start_int = dob_reg_start_int
  )

lines_before_index <- index_therapies %>%
  select(record_id, ca_seq, index_line) %>%
  left_join(
    lot,
    .,
    by = c('record_id', 'ca_seq')
  ) %>%
  # this means if the INDEX therapy itself is in one of these sets it will not be flagged.  Change to <= if you want to flag index cases too.
  filter(line_of_therapy < index_line)

# This uses the "e.g." list provided in exclusion criterion #2 in the RFP.
tki_list <- c(
  'osimertinib',
  'erlotinib',
  'gefitinib',
  'crizotinib',
  'ceritinib',
  'alectinib',
  'brigatinib',
  'loratinib'
)

lines_before_index_sum <- lines_before_index %>%
  mutate(
    # This uses the "e.g." list provided in exclusion criterion #2.
    is_tki = str_detect(
      tolower(regimen_drugs),
      paste(tki_list, collapse = '|')
    ),
    is_inv = str_detect(regimen_drugs, "Investigational")
  ) %>%
  group_by(record_id, ca_seq) %>%
  summarize(
    no_tki_to_index = !any(is_tki),
    no_inv_to_index = !any(is_inv)
  )

index_therapies <- left_join(
  index_therapies,
  lines_before_index_sum,
  by = c('record_id', 'ca_seq'),
  relationship = 'one-to-one'
)

readr::write_rds(
  index_therapies,
  here('data', 'cohort', 'index_therapies.rds')
)

# for now I'm just going to print this because it's not clear to me whether it's relevant:
lot %>%
  filter(had_both_prev_ther) %>%
  group_by(record_id) %>%
  summarize(has_any_doce = any(str_detect(regimen_drugs, "Docetaxel"))) %>%
  count(has_any_doce)
