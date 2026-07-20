library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

ca_ind <- readr::read_csv(
  here(
    'data-raw',
    'bpc',
    'NSCLC',
    '3.1-consortium',
    'cancer_level_dataset_index.csv'
  )
)
cpt <- readr::read_csv(
  here(
    'data-raw',
    'bpc',
    'NSCLC',
    '3.1-consortium',
    'cancer_panel_test_level_dataset.csv'
  )
)

one_samp_mult_cancer <- cpt %>%
  group_by(phase, institution, cpt_genie_sample_id, record_id) %>%
  summarize(
    ca_seq_str = paste(unique(ca_seq), collapse = ','),
    n_ca_seq = length(unique(ca_seq)),
    .groups = 'drop'
  ) %>%
  filter(n_ca_seq > 1) %>%
  arrange(desc(n_ca_seq))

# Adding this to check whether this might just be a bug in the derived code on the cancer index data:
one_samp_mult_cancer <- ca_ind %>%
  filter(record_id %in% pull(one_samp_mult_cancer, record_id)) %>%
  group_by(record_id) %>%
  summarize(
    n_ca_seq_in_ca_ind = length(unique(ca_seq)),
    .groups = 'drop'
  ) %>%
  left_join(
    one_samp_mult_cancer,
    .,
    by = 'record_id'
  )

one_samp_mult_cancer %>% filter(n_ca_seq > n_ca_seq_in_ca_ind)
# nope.

# For me: just output them all for easier pasting.
one_samp_mult_cancer %>%
  pull(record_id) %>%
  output_cbio_lines(
    ids = .,
    cohort = 'nsclc_genie_bpc_3_1',
  )

one_samp_mult_cancer %>%
  group_by(institution) %>%
  slice(1:5) %>%
  pull(record_id) %>%
  output_cbio_lines(
    ids = .,
    cohort = 'nsclc_genie_bpc_3_1',
    file = here('analysis', 'explore', 'nsclc_qc_issue_examples_per_site.txt')
  )

one_samp_mult_cancer %>%
  select(
    phase,
    institution,
    cpt_genie_sample_id,
    record_id,
    ca_seq_str,
    n_ca_seq
  ) %>%
  readr::write_csv(
    x = .,
    file = here('analysis', 'explore', 'nsclc_qc_issue_all_cases.csv')
  )
