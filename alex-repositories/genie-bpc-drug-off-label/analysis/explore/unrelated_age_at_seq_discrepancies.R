library(fs); library(purrr); library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

dft_clin_samp <- readr::read_tsv(
  here('data-raw', 'main_genie', 'data_clinical_sample.txt'),
  comment = "#"
) %>%
  rename_all(tolower)

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

dft_clin_dat %>% filter(short_name %in% "cpt") %>% slice(1) %>% pull(dat) %>% `[[`(.,1) %>% glimpse

dft_cpt <- dft_clin_dat %>%
  filter(short_name %in% "cpt") %>%
  mutate(
    dat = purrr::map(
      .x = dat,
      .f = \(z) {z %>% 
        select(cpt_genie_sample_id, institution, dob_cpt_report_days, cpt_seq_date, record_id, ca_seq) %>%
          arrange(ca_seq) %>%
          group_by(cpt_genie_sample_id) %>%
          slice(1) %>%
          ungroup(.)
      }
    )
  ) %>%
  select(cohort, dat) %>%
  unnest(dat)

# dft_cpt %>% count(cpt_genie_sample_id, sort = T)

dft_clin_samp %<>% 
  filter(!(age_at_seq_report_days %in% c("<6570", ">32485"))) %>%
  mutate(age_at_seq_report_days_n = case_when(
    age_at_seq_report_days %in% "Unknown" ~ NA_real_,
    T ~ as.numeric(age_at_seq_report_days)
  ))

# just checking:
dft_clin_samp %>% 
  filter(is.na(age_at_seq_report_days_n & !(age_at_seq_report_days %in% "Unknown")))

dft_comb <- left_join(
  dft_cpt,
  select(dft_clin_samp, cpt_genie_sample_id = sample_id, age_at_seq_report_days_n),
  by = "cpt_genie_sample_id"
)

dft_comb %<>%
  mutate(diff_bpc_main = age_at_seq_report_days_n - dob_cpt_report_days)

ggplot(
  dft_comb,
  aes(x = diff_bpc_main, y = 1, color = cpt_seq_date)
) + 
  theme_bw() + 
  geom_jitter(height = 1, width = 0, size = 0.5) +
  facet_wrap(vars(institution), ncol = 1) + 
  scale_color_viridis_c(option = "inferno") + 
  theme(
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom"
  ) + 
  labs(
    title = "Difference (Main-BPC) in DOB->Seq interval",
    subtitle = "One dot per NGS test (several per person possible)",
    x = "Difference (days, Main-BPC)"
  )

ggplot(
  dft_comb,
  aes(x = diff_bpc_main, y = 1, color = cpt_seq_date)
) + 
  theme_bw() + 
  geom_jitter(height = 1, width = 0, size = 0.5) +
  facet_grid(rows = vars(institution), cols = vars(cohort)) + 
  scale_color_viridis_c(option = "inferno") + 
  theme(
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom"
  ) + 
  labs(
    title = "Difference (Main-BPC) in DOB->Seq interval",
    subtitle = "One dot per NGS test (several per person possible)",
    x = "Difference (days, Main-BPC)"
  )

ggplot(
  filter(dft_comb, age_at_seq_report_days_n > 15000 & age_at_seq_report_days_n < 20000),
  aes(x = age_at_seq_report_days_n, y = 1)
) + 
  geom_jitter(width = 0) + 
  facet_wrap(vars(institution), ncol = 1)




dft_clin_dat %>%
  filter(short_name %in% "ca_ind") %>%
  slice(1) %>% pull(dat) %>% `[[`(.,1)

dft_ca_ind <- dft_clin_dat %>%
  filter(short_name %in% "ca_ind" & !(cohort %in% "BrCa")) %>%
  mutate(
    dat = purrr::map(
      .x = dat,
      .f = \(z) {z %>% 
          select(record_id, ca_seq, dob_ca_dx_days) 
      }
    )
  ) %>%
  select(cohort, dat) %>%
  unnest(dat)

left_join(
  select(dft_ca_ind, -cohort),
  dft_cpt,
  by = c("record_id", "ca_seq")
) %>%
  mutate(diff = dob_cpt_report_days - dob_ca_dx_days) %>%
  ggplot(.,
         aes(x = diff, y = 1)) + 
  geom_jitter(width = 0, height = 1)
