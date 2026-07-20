
library(purrr); library(fs); library(here)
purrr::walk(.x = fs::dir_ls('R'), .f = source)

dft_pt <- readr::read_csv(
  here('data-raw', 'patient_level_dataset.csv')
)
dft_ca_ind <- readr::read_csv(
  here('data-raw', 'cancer_level_dataset_index.csv')
)
dft_med_onc <- readr::read_csv(
  here('data-raw', 'med_onc_note_level_dataset.csv')
)
dft_img <- readr::read_csv(
  here('data-raw', 'imaging_level_dataset.csv')
)




dft_med_onc_prog <- dft_med_onc %>% 
  filter(md_ca_status %in% "Progressing/Worsening/Enlarging") %>%
  select(
    record_id, 
    # need a better name for this:
    dob_prog_days = md_onc_visit_int
  ) %>%
  mutate(prog_type = "med_onc")

dft_img_prog <- dft_img %>% 
  filter(image_overall %in% "Progressing/Worsening/Enlarging") %>%
  select(
    record_id,
    dob_prog_days = image_ref_scan_int
  ) %>%
  mutate(prog_type = "img")

# Sanity check:  These two give the same result right?
# dft_pt %>%
#   count(is.na(hybrid_death_int))
# 
# dft_ca_ind %>%
#   group_by(record_id) %>%
#   slice(1) %>%
#   ungroup(.) %>%
#   count(os_dx_status)

dft_death_prog <- dft_pt %>%
  filter(!is.na(hybrid_death_int)) %>%
  select(record_id,
         dob_prog_days = hybrid_death_int) %>%
  mutate(prog_type = "death")

dft_prog <- bind_rows(
  dft_med_onc_prog,
  dft_img_prog,
  dft_death_prog
) %>%
  arrange(record_id, dob_prog_days)

dft_prog %<>%
  mutate(
    prog_i = if_else(prog_type %in% c("img", "death"), T, F),
    prog_m = if_else(prog_type %in% c("med_onc", "death"), T, F)
  )

readr::write_rds(
  dft_prog,
  here('data', 'prog_events.rds')
)
