library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

# library(stringdist)

# Just as an example, MSK NSCLC2 data
curated_path <- dir_ls(path(qc_config$storage_root, 'data', 'l0_raw_redcap'))
dd_path <- dir_ls(path(qc_config$storage_root, 'dict', 'raw'))

aligned_dd <- align_data_dictionary(
  path_to_cur_dat = curated_path,
  path_to_dat_dict = dd_path
)

out_path <- path(qc_config$storage_root, 'dict', 'aligned')
fs::dir_create(out_path)

readr::write_rds(
  aligned_dd,
  file = here(out_path, 'dd.rds')
)
