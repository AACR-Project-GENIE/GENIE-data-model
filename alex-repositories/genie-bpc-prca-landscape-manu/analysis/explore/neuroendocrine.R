# This is an exploration of whether we have many neuroendocrine type prostate 
#   cases.  This came up in a separate manuscript, for off label uses, but
#.  I didn't have the pathology datasets integrated yet there.

library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source) # also

path <- readr::read_rds(here('data', 'clin', 'dft_path.rds'))
reg <- readr::read_rds(here('data', 'clin', 'dft_reg.rds'))

dft_neuro <- path %>% 
  group_by(record_id) %>%
  summarize(any_neuro = any(str_detect(path_pros_pat, "Neuroendocrine"), na.rm = T)) %>%
  mutate(any_neuro = if_else(is.na(any_neuro), F, any_neuro)) 

reg %>% filter(str_detect(regimen_drugs, "Lutetium")) %>% count(regimen_drugs)

dft_lut <- reg %>% 
  filter(ca_seq %in% 0) %>%
  filter(drugs_ct_yn %in% "No") %>%
  group_by(record_id) %>%
  summarize(
    any_lut = any(str_detect(regimen_drugs, "Lutetium"), na.rm = T)
  ) 

dft_lut %>%
  filter(any_lut) %>%
  left_join(dft_neuro) %>%
  count(any_neuro)
