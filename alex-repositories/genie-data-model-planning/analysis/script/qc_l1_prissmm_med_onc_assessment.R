library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

current_form <- 'prissmm_med_onc_assessment'

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

# Add other checks as needed here.

ag_intel <- ag %>% pointblank::interrogate(progress = F)

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
