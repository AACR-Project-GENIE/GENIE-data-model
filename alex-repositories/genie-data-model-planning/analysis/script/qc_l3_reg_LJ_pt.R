library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

qc_layer <- 3
qc_data_name <- 'reg_LJ_pt'

dir_l2 <- path(qc_config$storage_root, 'data', 'l2_derived')
pt <- readr::read_rds(here(dir_l2, 'pt.rds'))
reg <- readr::read_rds(here(dir_l2, 'reg.rds'))

rr_vec <- c('record_id', 'redcap_ca_seq')

reg_LJ_pt <- dplyr::left_join(
  reg,
  (pt %>%
    select('record_id', last_alive_int, hybrid_death_int)),
  by = 'record_id',
  suffix = c('.reg', '.ca_all'),
  relationship = 'many-to-one'
)

ag <- agent_start(
  dat = reg_LJ_pt,
  table_name = qc_data_name,
  add_standard_unique = F
)

ag <- ag %>%
  col_vals_lte(
    columns = matches('^drugs_(start|end|last)dt_int_'),
    value = vars(last_alive_int),
    label = 'Regimen interval variable is <= the last alive interval from pt data.',
    na_pass = T
  )

ag <- ag %>%
  col_vals_lte(
    columns = matches('^drugs_(start|end|last)dt_int_'),
    value = vars(hybrid_death_int),
    label = 'Regimen interval variable is <= the hybrid death interval from pt data.',
    na_pass = T
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
