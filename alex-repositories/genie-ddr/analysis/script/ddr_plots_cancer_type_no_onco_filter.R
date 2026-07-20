library(purrr)
library(here)
library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

samp_aug_fs <- readr::read_rds(
  here('data', 'combined', 'samp_aug_any_ddr_first_sample.rds')
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
  plot_title = "Proportion of first samples altered - no oncogenic filter",
  plot_subtitle = "Denominator is the number of samples TESTED for individual genes"
)
ggsave(
  gg_pos,
  height = 15,
  width = 10,
  filename = here(
    'output',
    'fig',
    'S2_fig_bladder_manu_main_genie_ddr_pos_no_onco_filter.pdf'
  )
)

# Testing is the same obviously, no need to redo that plot.
