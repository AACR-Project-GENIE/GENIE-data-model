library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

current_form <- 'cancer_diagnosis'

nested_dat <- readr::read_rds(
  path(qc_config$storage_root, 'data', 'l1_split', 'nested_l1.rds')
)

dict_sub <- nested_dat %>%
  filter(form_in_extract %in% current_form) %>%
  pull(dat_dict_sub) %>%
  .[[1]]

dat <- nested_dat %>%
  filter(form_in_extract %in% current_form) %>%
  pull(tab) %>%
  .[[1]]

ag <- agent_start(
  dat = dat,
  table_name = current_form
)

ag <- add_valid_value_checks_to_agent(
  data_dict = dict_sub,
  ptblank_agent = ag
)

ag <- ag %>%
  col_vals_in_set(
    columns = 'ca_type',
    set = '51',
    preconditions = ~ . %>% dplyr::filter(redcap_ca_index %in% '1'),
    label = 'Index cancers are NSCLC (ca_type = 51).',
    active = TRUE
  )

ag <- ag %>%
  col_vals_in_set(
    columns = 'redcap_ca_index',
    set = '1',
    preconditions = ~ . %>%
      group_by(record_id) %>%
      arrange(desc(redcap_ca_index)) %>%
      slice(1) %>%
      ungroup(.),
    label = 'Each person has at least one index cancer in the cancer_diagnosis form',
    active = TRUE
  )

ag <- ag %>%
  col_vals_not_null(
    columns = c('redcap_ca_seq', 'redcap_ca_index'),
    label = "Cancer seq and index flag completed."
  )

ag <- ag %>%
  rows_distinct(
    columns = c('record_id, redcap_ca_seq'),
    label = "Cancer seq numbers are unique (record_id and redcap_ca_seq form an alternate key)"
  )


ag <- ag %>%
  col_vals_not_null(
    columns = c('ca_stage_iv'),
    preconditions = ~ . %>%
      dplyr::filter(redcap_ca_index %in% 1) %>%
      dplyr::filter(is.na(best_ajcc_stage_cd)),

    label = "ca_stage_iv is complete index cancers without best_ajcc_stage_cd"
  )


ag <- ag %>%
  col_vals_not_null(
    columns = c('redcap_ca_seq'),
    label = "redcap_ca_seq is not blank for any rows."
  )

# Add other checks as needed here.

ag_intel <- ag %>%
  pointblank::interrogate(
    extract_failed = TRUE,
    progress = F
  )


ag_sum <- make_agent_table_row(
  dat_dict = dict_sub,
  interrogated_agent = ag_intel,
  qc_layer = 1,
  site_to_qc = qc_config$inst
)

readr::write_rds(
  ag_sum,
  path(
    qc_config$storage_root,
    'result',
    paste0('l1_', current_form, '.rds')
  )
)
