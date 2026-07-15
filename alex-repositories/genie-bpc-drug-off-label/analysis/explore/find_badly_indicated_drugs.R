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

dft_drug <- dft_clin_dat %>% 
  filter(short_name %in% "reg") %>%
  mutate(
    just_the_drugs = purrr::map(
      .x = dat,
      .f = \(z) select(z, record_id, ca_seq, regimen_number, regimen_drugs)
    )
  ) %>%
  select(cohort, just_the_drugs) %>%
  unnest(just_the_drugs) %>%
  separate(
    data = .,
    col = regimen_drugs,
    into = c("drug_1", "drug_2", "drug_3", "drug_4", "drug_5"),
    sep = ","
  ) %>%
  pivot_longer(
    cols = c(drug_1, drug_2, drug_3, drug_4, drug_5)
  ) %>%
  mutate(
    name = str_replace_all(name, "drug_", ""),
    name = as.numeric(name),
    value = str_trim(value)
  ) %>%
  rename(drug_number = name, agent = value) %>%
  filter(!is.na(agent)) 
  

dft_ind_v_use <- dft_drug %>%
  group_by(agent) %>%
  summarize(
    n = n(),
    n_cohorts = length(unique(cohort))
  )
    
    

dft_ind_mapped <- readr::read_rds(
  here('data', 'warner_materials', 'indications_mapped.rds')
)

dft_ind_v_use <- dft_ind_mapped %>%
  group_by(mapped_agent) %>%
  summarize(
    n_ind_rows = n(),
    n_conditions = length(unique(condition[condition != "NONE"]))
  ) %>%
  left_join(
    rename(dft_ind_v_use, n_uses = n),
    .,
    by = c(agent = "mapped_agent")
  )

gg <- ggplot(
  data = dft_ind_v_use,
  aes(x = n_conditions, y = n_uses, text = agent)
) + 
  geom_point() + 
  theme_bw()

plotly::ggplotly(gg)


gg <- ggplot(
  data = dft_ind_v_use,
  aes(x = n_conditions, y = n_cohorts, text = agent)
) + 
  geom_jitter(height = 0.5, width = 0.1, size = 0.25) + 
  theme_bw()

plotly::ggplotly(gg)








dft_ind_rel <- dft_ind_mapped %>%
  mutate(date = ymd(date)) %>%
  filter(regulator %in% "FDA" & date > "1997-01-01")

dft_ind_rel %<>% 
  filter(!is.na(mapped_cohort))

dft_ind_rel %>% count(!is.na(with), sort = T)
