
library(purrr); library(fs); library(here)
purrr::walk(.x = fs::dir_ls('R'), .f = source)


dft_ca_ind <- readr::read_csv(
  here('data-raw', 'cancer_level_dataset_index.csv')
)

dft_cpt <- readr::read_csv(
  here('data-raw', 'cancer_panel_test_level_dataset.csv')
)

dft_ca_ind %>% nrow # 3671

dft_path <- readr::read_csv(
  here('data-raw', 'pathology_report_level_dataset.csv')
)

dft_path %>% count(path_proc) # mmmmk some problems already.

dft_exc_timing <- dft_path %>%
  filter(path_proc %in% "Surgical excision") %>%
  select(record_id, dx_path_proc_days) # probably related to FIRST cancer.

dft_exc_timing %>% count(is.na(dx_path_proc_days)) # Nice

vec_pts_excision <- dft_exc_timing %>% count(record_id) %>% pull(record_id) # 2116, wow.





dft_rad <- readr::read_csv(
  here('data-raw', 'ca_radtx_dataset.csv')
)

vec_pts_rad <- dft_rad %>% 
  select(record_id, ca_seq, dx_rt_start_days) %>%
  count(record_id) %>%
  pull(record_id)

vec_pts_excision %>% length
union(vec_pts_excision, vec_pts_rad) %>% length # woah.



# Lacking the information we need.  Here's a look at it:
small_var_set <- dft_ca_ind %>% 
  select(ca_grade:naaccr_path_t_cd) 

small_list %>%
  map_dbl(
    .x = .,
    .f = \(x) {
      mean(!is.na(x))
    }
  )

small_list %>%
  mutate_all(as.character) %>%
  pivot_longer(
    cols = everything()
  ) %>%
  count(name, value) %>% print(n = 500)




