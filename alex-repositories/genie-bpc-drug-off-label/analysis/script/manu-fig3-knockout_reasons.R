# This could be made a bit more robust with a plot function helper.

library(purrr)
library(here)
library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

output_dir <- here('output', 'manu', 'fig3')

dft_hdrug_det <- readr::read_rds(
  here('data', 'linked_approvals', 'hdrug_determinations.rds')
)

# Borrowed from the stepwise tree script - should probably put in the det data.

failure_lev <- c(
  "Cancer type indicated",
  "Metastatic",
  "Biomarker",
  "Concomitant Medication",
  "Approved before exposure"
)

dft_hdrug_det %<>%
  mutate(
    failure_cat = case_when(
      failure_type %in% "test_ind_exists" ~ failure_lev[1],
      failure_type %in% "test_met" ~ failure_lev[2], #changed this one.
      str_detect(failure_type, "_biom_") ~ failure_lev[3],
      str_detect(failure_type, "_with_") ~ failure_lev[4],
      failure_type %in% "test_date_definite" ~ failure_lev[5]
    ),
    failure_cat = factor(failure_cat, failure_lev)
  )

if (
  nrow(
    dft_hdrug_det %>%
      filter(is.na(failure_cat) & !is.na(failure_type))
  )
) {
  cli_abort("Something wrong with failure_cat creation code")
}

off_lab_broad_cat <- count_off_label(
  dft_hdrug_det,
  group_vars = c('cohort', 'failure_cat')
)


off_lab_broad_cat %<>%
  complete(cohort, failure_cat, fill = list(n_off_label = 0, n_uses = 0))

off_lab_broad_cat %<>%
  group_by(cohort) %>%
  mutate(cohort_exp = sum(n_uses)) %>%
  ungroup(.) %>%
  filter(!is.na(failure_cat))

off_lab_broad_cat %<>%
  group_by(cohort) %>%
  arrange(failure_cat) %>%
  mutate(
    prop_failed = n_off_label / cohort_exp,
    prop_failed_cum = cumsum(n_off_label) / cohort_exp
  ) %>%
  ungroup(.)


off_lab_broad_cat %<>%
  mutate(
    cohort = cohort_release_order(cohort)
  )

off_lab_broad_cat %<>%
  mutate(failure_cat = fct_rev(failure_cat))

gg_fail_at_step <- ggplot(
  off_lab_broad_cat,
  aes(x = cohort, y = failure_cat, fill = prop_failed)
) +
  geom_tile(color = 'gray20') +
  scale_fill_viridis_c(
    name = "Eliminated at step",
    labels = label_percent(),
    option = 'rocket',
    direction = -1,
    begin = 0.2,
    end = 1,
    breaks = 0:3 / 10,
    limits = c(0, .3)
  ) +
  scale_x_discrete(position = 'top', name = NULL, expand = c(0, 0)) +
  scale_y_discrete(position = 'right', name = NULL, expand = c(0, 0)) +
  theme_bw() +
  theme(
    axis.text.y = element_text(hjust = 0),
    axis.ticks = element_blank(),
    legend.position = "bottom",
  ) +
  guides(
    fill = guide_colorbar(
      nbin = 13,
      display = 'rectangles'
    )
  )

gg_fail_cum <- ggplot(
  off_lab_broad_cat,
  aes(x = cohort, y = failure_cat, fill = prop_failed_cum)
) +
  geom_tile(color = 'gray20') +
  scale_fill_viridis_c(
    name = "Off label at step",
    labels = label_percent(),
    option = 'rocket',
    direction = -1,
    begin = 0.2,
    end = 1,
    breaks = 0:4 / 10,
    limits = c(0, 0.4)
  ) +
  scale_x_discrete(position = 'top', name = NULL, expand = c(0, 0)) +
  scale_y_discrete(position = 'right', name = NULL, expand = c(0, 0)) +
  theme_bw() +
  theme(
    axis.text.y = element_text(hjust = 0),
    axis.ticks = element_blank(),
    legend.position = "bottom",
  ) +
  guides(
    fill = guide_colorbar(
      nbin = 4 * 4 + 1,
      display = 'rectangles'
    )
  )

manu_save_helper(
  plot = gg_fail_at_step,
  dir = output_dir,
  name = 'manu-fig3-elim-at-step',
  height = 3,
  width = 6
)

manu_save_helper(
  plot = gg_fail_cum,
  dir = output_dir,
  name = 'manu-fig3-cumu-off-lab-at_step',
  height = 3,
  width = 6
)
