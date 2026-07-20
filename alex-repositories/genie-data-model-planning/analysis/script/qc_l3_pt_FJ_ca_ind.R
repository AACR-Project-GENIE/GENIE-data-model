library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

qc_layer <- 3
qc_data_name <- 'pt_FJ_ca_ind'

dir_l2 <- path(qc_config$storage_root, 'data', 'l2_derived')
ca_ind <- readr::read_rds(here(dir_l2, 'ca_ind.rds'))
pt <- readr::read_rds(here(dir_l2, 'pt.rds'))

rr_vec <- c('record_id', 'redcap_ca_seq')

pt_FJ_ca_ind <- dplyr::full_join(
  (pt %>%
    mutate(row_exists.pt = T) %>%
    select(record_id, row_exists.pt)),
  (ca_ind %>%
    mutate(row_exists.ca_ind = T) %>%
    select(record_id, row_exists.ca_ind)),
  by = 'record_id',
  suffix = c('.pt', '.ca_all'),
  relationship = 'one-to-many'
)

ag <- agent_start(
  dat = pt_FJ_ca_ind,
  table_name = qc_data_name,
  add_standard_unique = F
)

ag <- ag %>%
  col_vals_not_null(
    columns = 'row_exists.ca_ind',
    label = 'Every row in the patient table has a corresponding row in the index cancer table.'
  )

ag <- ag %>%
  col_vals_not_null(
    columns = 'row_exists.pt',
    label = 'Every row in the index cancer table has a corresponding row in the patient table.'
  )

ag_intel <- ag %>%
  pointblank::interrogate(
    extract_failed = TRUE,
    progress = F
  )

ag_sum <- make_agent_table_row(
  dat_dict = NULL,
  interrogated_agent = ag_intel,
  qc_layer = qc_layer,
  site_to_qc = qc_config$inst
)

readr::write_rds(
  ag_sum,
  path(
    qc_config$storage_root,
    'result',
    glue('l{qc_layer}_{qc_data_name}.rds')
  )
)
