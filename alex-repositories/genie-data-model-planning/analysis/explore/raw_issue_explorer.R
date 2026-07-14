rad %>%
  filter(record_id %in% 'GENIE-DFCI-619444', redcap_repeat_instance %in% 1)

ca_all %>%
  filter(record_id %in% 'GENIE-DFCI-619444') %>%
  glimpse

cpt %>%
  filter(record_id %in% 'GENIE-DFCI-619444', redcap_repeat_instance %in% 1)

reg %>%
  filter(record_id %in% 'GENIE-DFCI-619444', redcap_repeat_instance %in% 1) %>%
  select(redcap_ca_seq)

prov_raw <- readr::read_csv(
  here(
    '/Users/apaynter/main/projects/genie/bayer_qc/PROV-2026-02-16/data/l0_raw_redcap/2025000753AACR23113C_DATA_2026-02-11_1347.csv'
  )
)

prov_raw %>%
  filter(
    record_id %in% 'GENIE-PROV-025d7360b4',
    redcap_repeat_instrument %in% 'ca_directed_drugs',
    redcap_repeat_instance %in% 1
  ) %>%
  select(matches('drugs_qamajor'))
# Ok, so that doesn't exist.  Let's figure out what happened.

dat %>%
  filter(
    record_id %in% "GENIE-PROV-025d7360b4",
    redcap_repeat_instance %in% 1
  ) %>%
  select(matches('drugs_qamajor'))


prov_raw %>%
  filter(
    record_id %in% 'GENIE-PROV-025d7360b4',
    redcap_repeat_instrument %in% 'ca_directed_radtx',
    redcap_repeat_instance %in% 2
  ) %>%
  select(matches('rt_ca'))
# Ok, so that doesn't exist.  Let's figure out what happened.

dat %>%
  filter(
    record_id %in% "GENIE-PROV-025d7360b4",
    redcap_repeat_instance %in% 1
  ) %>%
  select(matches('rt_ca___'))


prov_raw %>%
  filter(
    record_id %in% 'GENIE-PROV-025d7360b4',
    redcap_repeat_instrument %in% 'ca_directed_radtx',
    redcap_repeat_instance %in% 2
  ) %>%
  select(matches('rt_site'))
# Ok, so that doesn't exist.  Let's figure out what happened.

dat %>%
  filter(
    record_id %in% "GENIE-PROV-025d7360b4",
    redcap_repeat_instance %in% 1
  ) %>%
  select(matches('rt_site'))


msk_raw <- readr::read_csv(
  here(
    '/Users/apaynter/main/projects/genie/bayer_qc/MSK-2026-02-16/data/l0_raw_redcap/DRUG_MASKED_bayer_her2_cohort_11-14-58-2026-01-29.csv'
  )
)
