# space because it's a sparse dataset, and basilisk because it causes death in those who look into it.

library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

dat_ex <- readr::read_csv(
  dir_ls(path(qc_config$storage_root, 'data', 'l0_raw_redcap')),
  # read everything as a character at this stage:
  col_types = cols(.default = col_character())
) %>%
  trim_nameless_cols(.)

dat_dict <- readr::read_rds(
  path(qc_config$storage_root, 'dict', 'aligned', 'dd.rds')
)

# space because it's a pointlessly sparse dataset, and basilisk because it causes death in those who look into it.
basil_agent <- create_agent_space_basilisk(
  dat_dict = dat_dict,
  dat_basil = dat_ex
)

basil_intel <- basil_agent %>% pointblank::interrogate(progress = F)

basil_sum <- make_agent_table_row(
  dat_dict = dat_dict,
  interrogated_agent = basil_intel,
  qc_layer = 0,
  site_to_qc = qc_config$inst
)


basil_sum %>%
  pull(validation_subset) %>%
  .[[1]]
# data rows where the problems occured:
basil_sum %>%
  pull(extracts) %>%
  .[[1]]
# lobstr::obj_sizes(!!!basil_sum) # coolest pattern ever.

readr::write_rds(
  basil_sum,
  path(qc_config$storage_root, 'result', 'l0_raw_redcap.rds')
)

# Some other commands that were useful:
# basil_intel
# all_passed(basil_intel)
# get_agent_report(hydra_intel, display_table = TRUE) # a gt table
# export_report(hydra_intel, filename = 'report-demo.html')
