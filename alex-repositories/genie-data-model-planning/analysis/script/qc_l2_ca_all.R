library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

qc_layer <- 2
qc_data_name <- 'ca_all'

dir_l2 <- path(qc_config$storage_root, 'data', 'l2_derived')
ca_all <- readr::read_rds(path(dir_l2, 'ca_all.rds'))

ag <- agent_start(
  dat = ca_all,
  table_name = qc_data_name
)

ag <- ag %>%
  rows_distinct(
    columns = c('record_id, redcap_ca_seq'),
    label = "Cancer seq numbers are unique (record_id and redcap_ca_seq form an alternate key)"
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
