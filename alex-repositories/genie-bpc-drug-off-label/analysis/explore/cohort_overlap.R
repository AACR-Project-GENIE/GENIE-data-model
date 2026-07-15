# Description:  Transform data as needed and save in /data/cohort.

library(fs); library(purrr); library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

dft_data_names <- tribble(
  ~short_name, ~synapse_name,
  "ca_ind", "cancer_level_dataset_index.csv",
  "ca_non_ind", "cancer_level_dataset_non_index.csv",
  "cpt", "cancer_panel_test_level_dataset.csv",
  "pt", "patient_level_dataset.csv",
  "reg", "regimen_cancer_level_dataset.csv"
)


dft_clin_dat <- tibble(
  cohort = dir("data-raw")
) %>%
  mutate(cohort_path = here("data-raw", cohort)) %>%
  filter(!(cohort %in% "manual"))


# Run for one cohort example:
# folder_load_helper(slice(dft_clin_dat,1)$cohort_path,
#                    dn = dft_data_names)

dft_clin_dat %<>%
  mutate(
    loaded_data = purrr::map(
      .x = cohort_path,
      .f = (function(p) {
        folder_load_helper(p, dn = dft_data_names)
      })
    )
  ) %>%
  unnest(loaded_data)


dft_index_cases <- dft_clin_dat %>%
  filter(short_name %in% c("ca_ind")) %>%
  mutate(
    dat_small = purrr::map(
      .x = dat,
      .f = \(x) { select(x, record_id, ca_seq) }
    )
  ) %>%
  select(cohort, dat_small) %>%
  unnest(dat_small) 

dft_index_cases %>%
  dplyr::count(cohort, record_id, sort = F) %>%
  arrange(cohort) %>%
  pivot_wider(
    names_from = "cohort",
    values_from = "n"
  ) %>%
  mutate(
    grand_total = kit::psum(BLADDER, BrCa, CRC, NSCLC, PANC, Prostate, na.rm = T)
  ) %>%
  arrange(desc(grand_total))
