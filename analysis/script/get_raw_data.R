library(fs); library(purrr); library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

synLogin()

dft_datasets_to_get <- tribble(
  ~synapse_name, ~dat_name,
  "cancer_level_dataset_index.csv", "ca_ind",
  "cancer_level_dataset_non_index.csv", "ca_non_ind",
  "cancer_panel_test_level_dataset.csv", "cpt",
  "imaging_level_dataset.csv", "img",
  "med_onc_note_level_dataset.csv", "med_onc",
  "pathology_report_level_dataset.csv", "path",
  "patient_level_dataset.csv", "pt",
  "regimen_cancer_level_dataset.csv", "reg",
  "tm_level_dataset.csv", "tm",
)

# each folder below is the clinical data release
dft_folders <- tibble::tribble(
  ~cohort, ~synid,
  # consortium releases:
  "BLADDER_1.2-consortium", "syn53018574",
  'CRC_1.2-consortium', 'syn26046784',
  "NSCLC_2.1-consortium", 'syn25982471',
  'BrCa_1.2-consortium', 'syn39802381',
  'PANC_1.2-consortium', 'syn50612197',
  'Prostate_1.2-consortium', 'syn50612196'
)

dft_datasets <- dft_folders %>%
  mutate(
    children = purrr::map(
      .x = synid,
      .f = (function(id) {
        dat <- get_syn_children_df(id) %>%
          # only need limited info for this project
          select(
            dat_name = name,
            dat_synid = id
          ) %>%
          filter(dat_name %in% dft_datasets_to_get$synapse_name) %>%
          rename(synapse_name = dat_name)
        
        dat %<>% left_join(., dft_datasets_to_get,
                           by = c("synapse_name"))
        return(dat)
      })
    )
  ) %>%
  unnest(children)




dft_datasets %<>%
  mutate(
    dat = purrr::map(
      .x = dat_synid,
      .f = get_synapse_entity_csv
    )
  )

dft_datasets %>% 
  mutate(
    nrow_grabbed = purrr::map_dbl(
      .x = dat,
      .f = nrow
    )
  ) %>% 
  select(cohort, dat_name, nrow_grabbed) %>% View(.)

write_rds(
  dft_datasets,
  here('data', 'all_dat.rds')
)
