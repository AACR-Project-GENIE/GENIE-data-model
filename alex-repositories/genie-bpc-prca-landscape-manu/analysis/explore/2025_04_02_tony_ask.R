as_list(here("R", dir(here("R"))))
purrr::walk(.x = here("R", dir(here("R"))), .f = source)

dft_ca_ind <- read_wrap("cancer_level_dataset_index.csv")
dft_med_onc <- read_wrap("med_onc_note_level_dataset.csv")

dft_cast_block <- make_cast_status_block(
  dft_med_onc, 
  dft_ca_ind, 
  remove_hspc_after_crpc = T
)
met_time <- get_dmet_time(dft_ca_ind)

# get just the blocks that have some time where the patient is metastatic.
met_cast_blocks <- left_join(
  dft_cast_block,
  met_time,
  by = 'record_id'
) %>%
  filter(!is.na(dx_dmet_yrs)) %>%
  filter(dx_block_end > dx_dmet_yrs)

met_cast_blocks %>% count(record_id) %>% nrow(.) # number metastatic - one off, ok with me

met_cast_blocks %>% count(md_cast_status_f)

# What if we limit the minimum block size to 30 days?
met_cast_blocks %<>% 
  mutate(block_length = dx_block_end - pmax(dx_block_start, dx_dmet_yrs)) 
met_cast_blocks %>%
  filter(block_length > 30/365.25) %>%
  count(md_cast_status_f)
# Fairly durable.

# Just checking:
met_cast_blocks %>% 
  filter(md_cast_status_f %in% "Castrate-Resistant") %>%
  select(contains("dx_block"), "dx_dmet_yrs") %>% 
  View(.)

# How many people have both type of blocks we're interested in?
met_cast_blocks %>%
  filter(md_cast_status_f %in% c("Castrate-Resistant", "Hormone Sensitive")) %>%
  count(record_id) %>%
  filter(n %in% 2) %>%
  nrow(.)
