library(purrr)
library(here)
library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

output_dir <- here('output', 'manu', 'fig4')

pal_cape_guide <- c('#c23917', '#177bc2', '#22856e')

dft_cohort_cases <- readr::read_rds(
  here('data', 'cohort', 'clin_dat_wide.rds')
)


dft_hdrug_cohort <- dft_cohort_cases %>%
  select(cohort, hdrug) %>%
  unnest(hdrug)

dft_nccn_sum_panc <- readr::read_rds(
  here('data', 'guideline_parse', 'nccn_panc_capecitabine_sum.rds')
)

dft_nccn_sum_panc %<>%
  mutate(
    char_norm_match = n_text_matches / alpha_char_count,
    word_norm_match = n_text_matches / word_count
  )

gg_panc_cape <- dft_hdrug_cohort %>%
  filter(
    cohort %in% "Pancreas",
    agent %in% "Capecitabine"
  ) %>%
  group_by(record_id) %>%
  arrange(drug_start_date_max) %>%
  slice(1) %>%
  ungroup(.) %>%
  plot_drug_use_times(
    .,
    plot_title = "Pancreatic capecitabine exposures"
  ) +
  geom_vline(xintercept = ymd("2022-12-14"), color = pal_cape_guide[2]) +
  coord_cartesian(xlim = ymd(paste0(c(2000, 2023), "01-01"))) +
  theme(
    plot.margin = margin(t = 0.25, r = 0.5, b = 0.25, l = 0.25, unit = 'cm')
  )


gg_panc_all_drug <- dft_hdrug_cohort %>%
  filter(
    cohort %in% "Pancreas",
  ) %>%
  plot_drug_use_times(
    .,
    plot_title = "Pancreatic exposures (any drug)",
  ) +
  coord_cartesian(xlim = ymd(paste0(c(2000, 2023), "01-01")))

gg_nccn_panc_cape <- plot_guideline_str(
  dat = dft_nccn_sum_panc,
  y_var = 'char_norm_match',
  plot_title = "Capecitabine mentions in NCCN pancreas guidelines",
) +
  scale_x_date(
    date_breaks = '2 years',
    date_labels = '%Y'
  ) +
  # scale_y_continuous(
  #   limits = c(0, 3 * 10^-4),
  #   expand = expansion(add = c(0, 0.5 * 10^-4), mult = c(0, 0))
  # ) +
  coord_cartesian(
    xlim = ymd(paste0(c(2000, 2023), "01-01")),
  ) +
  geom_vline(
    xintercept = ymd('2005-01-01'),
    color = pal_cape_guide[1]
  ) +
  annotate(
    geom = "text",
    size = 4,
    color = pal_cape_guide[1],
    x = ymd('2005-07-01'),
    y = 0.3 * 10^(-4),
    hjust = 0,
    label = "Added to guidelines"
  ) +
  # geom_vline(
  #   xintercept = ymd('2012-01-01'),
  #   color = pal_cape_guide[3]
  # ) +
  # annotate(
  #   geom = "text",
  #   size = 4,
  #   color = pal_cape_guide[3],
  #   x = ymd('2012-07-01'),
  #   y = 0.3 * 10^(-4),
  #   hjust = 0,
  #   label = "Widely recommended"
  # ) +
  geom_vline(
    xintercept = ymd('2022-12-14'),
    color = pal_cape_guide[2]
  ) +
  annotate(
    geom = "text",
    size = 4,
    color = pal_cape_guide[2],
    x = ymd('2022-07-01'),
    y = 0.9 * 10^(-4),
    hjust = 1,
    label = "FDA approved"
  )

fig4 <- cowplot::plot_grid(
  gg_panc_cape + theme(plot.title.position = 'panel'),
  gg_panc_all_drug + theme(plot.title.position = 'panel'),
  gg_nccn_panc_cape + theme(plot.title.position = 'panel'),
  ncol = 1,
  align = 'v',
  rel_heights = c(0.3, 0.3, 0.4),
  labels = 'AUTO'
)

manu_save_helper(
  plot = fig4,
  dir = output_dir,
  name = 'manu-fig4-guideline-capecitabine-nccn',
  height = 5,
  width = 6
)
