# Completed unrelated thing I didn't feel like downloading the data again.
# For the Bayer HER2 project.
library(purrr)
library(here)
library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

out_dir <- here('analysis', 'explore', 'nsclc_out')

dft_clin_dat_wide <- readr::read_rds(
  here('data', 'cohort', 'clin_dat_wide.rds')
)


dft_reg_nsclc <- dft_clin_dat_wide %>%
  filter(cohort %in% "NSCLC") %>%
  pull(reg) %>%
  `[[`(., 1)


dft_ca_ind_nsclc <- fread(
  here('data-raw', 'NSCLC', 'cancer_level_dataset_index.csv')
)


dft_ca_non_ind_nsclc <- fread(
  here('data-raw', 'NSCLC', 'cancer_level_dataset_non_index.csv')
)

dft_cpt_nsclc <- fread(
  here('data-raw', 'NSCLC', 'cancer_panel_test_level_dataset.csv')
)

dft_ca_non_ind_nsclc %>%
  select(record_id, ca_seq, ca_type, ca_d_site) %>%
  inner_join(
    .,
    dft_cpt_nsclc,
    by = c('record_id', 'ca_seq')
  ) %>%
  as_tibble

dft_ca_non_ind_nsclc %>%
  as_tibble(.) %>%
  select(record_id, ca_seq, ca_type, ca_d_site) %>%
  inner_join(
    .,
    as_tibble(dft_cpt_nsclc),
    by = c('record_id', 'ca_seq')
  ) %>%
  as_tibble %>%
  glimpse


dft_reg_nsclc %>%
  filter(str_detect(tolower(regimen_drugs), "aflibercept")) # no cases

create_drug_dat(dft_reg_nsclc) %>%
  count(agent)


call_an_na_an_na <- function(col) {
  if (!is.character(col)) {
    return(col)
  } else {
    dplyr::case_when(
      col %in% c("", "Unknown", "NO VALUE ENTERED", "99") ~ NA_character_,
      T ~ col
    )
  }
}

# Missing variables in the lung dataset:
dft_ca_ind_nsclc <- fread(
  here('data-raw', 'NSCLC', 'cancer_level_dataset_index.csv')
) |>
  as_tibble() |>
  mutate(across(.cols = everything(), .fns = call_an_na_an_na))
dft_ca_non_ind_nsclc <- fread(
  here('data-raw', 'NSCLC', 'cancer_level_dataset_non_index.csv')
) |>
  as_tibble() |>
  mutate(across(.cols = everything(), .fns = call_an_na_an_na))


# Just needed to get the function above.
# purrr::walk(
#   .x = names(dft_ca_ind_nsclc |> select(where(is.character))),
#   .f = \(z) print(count(dft_ca_ind_nsclc, .data[[z]], sort = T), n = 5)
# )

var_data <- tibble(
  variable = names(dft_ca_ind_nsclc),
  exists_index = T,
  n_complete_index = purrr::map_dbl(.x = dft_ca_ind_nsclc, .f = \(z) {
    sum(!is.na(z))
  })
)
var_data %<>%
  mutate(prop_complete_index = n_complete_index / nrow(dft_ca_ind_nsclc))

var_data_nonindex <- tibble(
  variable = names(dft_ca_non_ind_nsclc),
  exists_non_index = T,
  n_complete_non_index = purrr::map_dbl(.x = dft_ca_non_ind_nsclc, .f = \(z) {
    sum(!is.na(z))
  })
)
var_data_nonindex %<>%
  mutate(
    prop_complete_non_index = n_complete_non_index / nrow(dft_ca_non_ind_nsclc)
  )

var_data <- full_join(
  var_data,
  var_data_nonindex,
  by = "variable"
)

var_data_plot <- var_data |>
  # rev for plot axis ease.
  mutate(
    variable = fct_rev(forcats::fct_inorder(variable))
  ) %>%
  replace_na(list(prop_complete_index = 0, prop_complete_non_index = 0)) %>%
  select(variable, matches("^prop_complete")) %>%
  pivot_longer(
    cols = matches("^prop_complete"),
    names_to = "data_source",
    values_to = "prop_complete"
  ) %>%
  mutate(
    data_source = str_replace_all(data_source, "prop_complete_", "")
  )


gg_var <- ggplot(
  var_data_plot,
  aes(y = variable, x = data_source, fill = prop_complete)
) +
  geom_tile() +
  scale_x_discrete(position = 'top') +
  scale_y_discrete(position = 'right') +
  scale_fill_viridis_c(
    name = "Prop. complete",
    option = 'mako',
    begin = 0.2,
    end = 0.8
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90),
    legend.position = 'left',
    legend.title = element_text(angle = -90, hjust = 0.5)
  )

ggsave(
  plot = gg_var,
  file = here(out_dir, 'variable_completeness_heatmap.pdf'),
  height = 18,
  width = 3.5
)
gg_var_line <- ggplot(
  var_data_plot,
  aes(y = variable, x = prop_complete, color = data_source)
) +
  geom_blank() +
  geom_segment(
    inherit.aes = F,
    data = tibble(x1 = 0, x2 = 1, y = 1:nrow(var_data)),
    mapping = aes(x = x1, xend = x2, y = y),
    color = "gray50"
  ) +
  geom_point(alpha = 0.6) +
  coord_cartesian(xlim = c(0, 1)) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 90),
    legend.position = "top"
  )

ggsave(
  plot = gg_var_line,
  file = here(out_dir, 'variable_completeness_line.pdf'),
  height = 18,
  width = 3.5
)


dft_ca_both_nsclc <- bind_rows(
  (dft_ca_ind_nsclc %>%
    select(phase, cohort, record_id, ca_seq, ca_d_site, ca_type)),
  (dft_ca_non_ind_nsclc %>%
    select(phase, cohort, record_id, ca_seq, ca_d_site, ca_type))
)

# Just checking:
dft_ca_both_nsclc %>% count(record_id, ca_seq, sort = T)

dft_ca_both_nsclc %>% count(record_id) %>% nrow(.) # number of people
dft_ca_both_nsclc %>%
  filter(ca_seq %in% 0) %>%
  nrow(.) # number of people with one lung cancer (most restrictive)
dft_ca_both_nsclc %>%
  group_by(record_id) %>%
  summarize(
    only_lung = all(ca_type %in% "Non Small Cell Lung Cancer", na.rm = T)
  ) %>%
  filter(only_lung) %>%
  nrow(.)

dft_ca_both_nsclc %>%
  group_by(record_id) %>%
  summarize(
    only_lung = all(
      ca_type %in%
        c(
          "Non Small Cell Lung Cancer",
          "Lung Cancer, NOS",
          "Small Cell Lung Cancer"
        ),
      na.rm = T
    )
  ) %>%
  filter(only_lung) %>%
  nrow(.)

dft_ca_both_nsclc %>%
  group_by(record_id) %>%
  summarize(
    only_lung = all(
      ca_type %in%
        c(
          "Non Small Cell Lung Cancer",
          "Lung Cancer, NOS",
          "Small Cell Lung Cancer",
          "Prostate Cancer"
        ),
      na.rm = T
    )
  ) %>%
  filter(only_lung) %>%
  nrow(.)

get_dmet_time(dft_ca_ind_nsclc)

dft_ca_ind_nsclc %>%
  filter(stage_dx %in% "Stage III") %>%
  mutate(
    stage_detailed_comb = case_when(
      !is.na(best_ajcc_stage_cd) ~ best_ajcc_stage_cd,
      !is.na(ca_path_group_stage) ~ ca_path_group_stage,
      T ~ NA_character_
    )
  ) %>%
  count(stage_detailed_comb)

vec_stage_3b_dx <- dft_ca_ind_nsclc %>%
  filter(stage_dx %in% c("Stage III", "Stage IV")) %>%
  mutate(
    stage_detailed_comb = case_when(
      !is.na(best_ajcc_stage_cd) ~ best_ajcc_stage_cd,
      !is.na(ca_path_group_stage) ~ ca_path_group_stage,
      T ~ NA_character_
    )
  ) %>%
  filter(
    stage_dx %in% "Stage IV" | stage_detailed_comb %in% c("3B", "3C", "IIIB")
  ) %>%
  pull(record_id)

union(
  vec_stage_3b_dx,
  pull(get_dmet_time(dft_ca_ind_nsclc), record_id)
) %>%
  length
