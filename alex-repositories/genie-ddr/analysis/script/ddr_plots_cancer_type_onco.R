library(purrr)
library(here)
library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

samp_aug_fs <- readr::read_rds(
  here('data', 'combined', 'samp_aug_onco_first_sample.rds')
)


# We'll calculate this as a vector then put it back in.
# rowSums() is just a huge pain with pipes, but it's fast.
any_ddr <- samp_aug_fs %>%
  select(any_of(custom_ddr_list())) %>%
  rowSums(., na.rm = T) %>%
  magrittr::is_greater_than(., 0)
samp_aug_fs %<>% mutate(any_ddr = any_ddr)

samp_aug_fs %<>% add_custom_cancer_type(.)

cts_long <- samp_aug_fs %>% aggregate_ddr_by_cancer_type()

gg_pos <- plot_ddr_heatmap(
  filter(cts_long, feature != "any_DDR"),
  plot_title = "Proportion of first samples altered by cancer type",
  plot_subtitle = "Denominator is the number of samples TESTED for individual genes"
)
ggsave(
  gg_pos,
  height = 15,
  width = 10,
  filename = here('output', 'fig', '1A_fig_bladder_manu_main_genie_ddr_pos.pdf')
)

gg_test <- plot_ddr_heatmap(
  (cts_long %>% filter(!(feature %in% "any_DDR"))),
  plot_title = "Proportion of first samples tested for each cancer type",
  plot_subtitle = "Denominator is the number of samples (first sample for each person)",
  fill_col = 'test',
  vir_theme = 'viridis',
  vir_begin = 0.2,
  vir_end = 0.8,
  tile_color = 'gray20',
  scale_name = 'Proportion tested'
)

ggsave(
  gg_test,
  height = 15,
  width = 10,
  filename = here(
    'output',
    'fig',
    'S1_fig_bladder_manu_main_genie_ddr_test.pdf'
  )
)
