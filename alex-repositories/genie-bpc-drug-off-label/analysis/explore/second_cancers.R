library(fs); library(purrr); library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

dft_clin_dat <- readr::read_rds(
  here('data-raw', 'clin_dat_untouched.rds')
)

dft_ca_type <- dft_clin_dat %>%
  filter(short_name %in% c("ca_ind", "ca_non_ind"))

dft_ca_type %<>%
  mutate(
    rel_var = purrr::map(
      .x = dat,
      .f = \(z) {
        select(
          z,
          record_id, ca_seq, 
          any_of(c("ca_cadx_int", "dob_ca_dx_days")), # breast cancer strikes again...
          ca_d_site,
          matches("ca_type"), matches("ca_heme")
        )
      }
    )
  ) %>%
  mutate(
    is_index = case_when(
      short_name %in% "ca_ind" ~ T,
      short_name %in% "ca_non_ind" ~ F,
      T ~ NA
    )
  ) %>% 
  select(cohort, is_index, rel_var) %>%
  unnest(rel_var)

dft_ca_type %<>%
  mutate(
    dob_ca_dx_days = if_else(
      is.na(dob_ca_dx_days), 
      ca_cadx_int,
      dob_ca_dx_days
    )
  ) %>%
  select(-ca_cadx_int)

# for our purposes I think this would help:
dft_ca_type %<>%
  mutate(
    ca_type = case_when(
      is.na(ca_type) & ca_heme_malig %in% "Yes" ~ "(heme)",
      # This code is already used:
      is.na(ca_type) ~ "Ill Defined/Cancer of Unknown Primary",
      T ~ ca_type
    )
  )

dft_ca_type %<>%
  select(cohort, record_id, ca_seq, is_index, dob_ca_dx_days, ca_type)

list_index <- dft_ca_type %>%
  filter(is_index) %>%
  filter(
    # obvious mistake:
    !(cohort %in% "NSCLC" & ca_type %in% "Small Cell Lung Cancer")
  ) %>%
  group_by(cohort) %>%
  summarize(
    index_type_list = list(unique(sort(ca_type))),
    .groups = "drop"
  )

list_index <- (list_index$index_type_list %>% 
                 set_names(., list_index$cohort))


dft_min_ca_seq <- dft_ca_type %>%
  filter(is_index) %>%
  group_by(cohort, record_id) %>%
  summarize(
    .min_ca_seq = min(ca_seq, na.rm = T),
    min_ca_seq_f = case_when(
      .min_ca_seq %in% 0 ~ "First and only",
      .min_ca_seq %in% 1 ~ "First of multiple",
      .min_ca_seq >= 2 ~ "Not first of multiple"
    ),
    .groups = "drop"
  ) %>%
  select(-.min_ca_seq)
      
tabyl(dft_min_ca_seq,
      min_ca_seq_f, cohort)

vec_bladder_mult_recoverable <- dft_min_ca_seq %>%
  filter(min_ca_seq_f %in% "First of multiple" & cohort %in% "BLADDER") %>%
  pull(record_id)

dft_blad <- dft_ca_type %>%
  filter(record_id %in% vec_bladder_mult_recoverable) %>%
  arrange(record_id, ca_seq) %>%
  mutate(
    cancer_of_index_type = ca_type %in% list_index$BLADDER
  ) %>%
  group_by(record_id) %>%
  summarize(
    only_has_index_types_after = all(cancer_of_index_type),
    first_nonindex = first(ca_type[!cancer_of_index_type], default = NA)
  )

dft_blad %>%
  count(only_has_index_types_after)

dft_blad %>% 
  filter(!only_has_index_types_after) %>%
  count(first_nonindex, sort = T)
  
  
