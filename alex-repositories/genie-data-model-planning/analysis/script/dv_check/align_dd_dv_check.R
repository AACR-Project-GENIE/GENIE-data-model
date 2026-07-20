library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

dd_path <- here(
  'data-raw',
  'curated-manual',
  'nonphi_v388_PRISSMM_Dictionary_NSCLC_BPC_P2.csv'
)

# I ended up grabbing these manually to get the latest version:
cur_stub <- here('data-raw', 'bpc', 'step1-curated', 'NSCLC2_manual_validation')

alignment_manifest <- tibble(
  site = c('DFCI', 'MSK', 'UHN', 'VICC'),
  # data dictionary is going in to test the "should never happen" case when we have different data dictionaries for two different sites.
  dd_path = dd_path,
  # makes an assumption of one file per folder - enforced by the loving manual file work I did:
  cur_path = c(
    dir_ls(path(cur_stub, 'DFCI')),
    dir_ls(path(cur_stub, 'MSK')),
    dir_ls(path(cur_stub, 'UHN')),
    dir_ls(path(cur_stub, 'VICC'))
  )
)

# aligned_test <- align_data_dictionary(
#   path_to_cur_dat = alignment_manifest$cur_path[[1]],
#   path_to_dat_dict = alignment_manifest$dd_path[[1]]
# )

aligned_dd <- alignment_manifest %>%
  mutate(
    aligned_dd = purrr::map2(
      .x = cur_path,
      .y = dd_path,
      .f = align_data_dictionary
    )
  )

out_path <- path('data', 'dv', 'aligned_dd')
fs::dir_create(out_path)

readr::write_rds(
  aligned_dd,
  file = here(out_path, 'dd_nested.rds')
)
