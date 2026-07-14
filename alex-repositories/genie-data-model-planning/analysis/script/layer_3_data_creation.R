purrr::walk(.x = fs::dir_ls(here::here("R")), .f = source)

dir_l2 <- here('data', 'qc', 'DFCI', 'layer_2_datasets')

ca_ind <- readr::read_rds(here(dir_l2, 'ca_ind.rds'))
ca_non_ind <- readr::read_rds(here(dir_l2, 'ca_non_ind.rds'))
ca_all <- readr::read_rds(here(dir_l2, 'ca_all.rds'))
cpt <- readr::read_rds(here(dir_l2, 'cpt.rds'))
img <- readr::read_rds(here(dir_l2, 'img.rds'))
med_onc <- readr::read_rds(here(dir_l2, 'med_onc.rds'))
path <- readr::read_rds(here(dir_l2, 'path.rds'))
pt <- readr::read_rds(here(dir_l2, 'pt.rds'))
rad <- readr::read_rds(here(dir_l2, 'rad.rds'))
reg <- readr::read_rds(here(dir_l2, 'reg.rds'))


rr_vec <- c('record_id', 'redcap_ca_seq')


rad_LJ_ca_all <- dplyr::left_join(
  rad,
  (ca_all %>%
    mutate(row_exists.ca_all = T) %>%
    select(all_of(rr_vec), row_exists.ca_all)),
  by = rr_vec,
  suffix = c('.rad', '.ca_all'),
  relationship = 'many-to-one'
)

# remaining ones I want to do:
# - pt <-> ca_ind full join, should be identical patient cohorts.
# - path -> pt make sure ids good, probably some time stuff later on.
# - med_onc -> pt make sure ids good, probably some time stuff later on.
# - img -> pt make sure ids good, time later.

# pt_FJ_ca_ind <- dplyr::full_join(
#   (pt %>% select(record_id) %>% mutate(row_exists.pt = T)),
#   (ca_ind %>% select(record_id) %>% mutate(row_exists.pt = T)),
# )

# Note: probably better to just create these datasets before you start the script analyzing them for errors.  All the same, good problems.
