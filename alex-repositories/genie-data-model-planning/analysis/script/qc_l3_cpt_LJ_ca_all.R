library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

qc_layer <- 3
qc_data_name <- 'cpt_LJ_ca_all'

dir_l2 <- path(qc_config$storage_root, 'data', 'l2_derived')
ca_all <- readr::read_rds(path(dir_l2, 'ca_all.rds'))
cpt <- readr::read_rds(path(dir_l2, 'cpt.rds'))

rr_vec <- c('record_id', 'redcap_ca_seq')

cpt_LJ_ca_all <- dplyr::left_join(
  cpt,
  (ca_all %>%
    mutate(row_exists.ca_all = T) %>%
    select(all_of(rr_vec), row_exists.ca_all)),
  by = rr_vec,
  suffix = c('.cpt', '.ca_all'),
  relationship = 'many-to-one'
)

ag <- agent_start(
  dat = cpt_LJ_ca_all,
  table_name = qc_data_name,
  add_standard_unique = F
)

ag <- ag %>%
  col_vals_not_null(
    columns = 'row_exists.ca_all',
    label = 'A cancer diagnosis defined by record and redcap_ca_seq exists for every row of cancer panel test instrument.'
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
