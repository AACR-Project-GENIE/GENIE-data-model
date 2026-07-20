# This script attempts to answer the question:
# "How many off label uses are found with each test that we add?"
# This is done by adding one test column at a time and evaluating
# the data.

library(purrr)
library(here)
library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

dir_output <- here('data', 'linked_approvals', 'stepwise')

dft_poss_app <- readr::read_rds(
  here('data', 'linked_approvals', 'possible_approvals.rds')
)

# The "all" test column is recalculated after each test below.
dft_poss_app %<>% select(-test_all)

vec_pa_cols <- names(dft_poss_app)
vec_tests <- vec_pa_cols[str_detect(vec_pa_cols, "^test")]
vec_other_cols <- vec_pa_cols[!str_detect(vec_pa_cols, "^test")]

dft_step <- tibble(
  step = vec_tests
)

dft_step %<>%
  mutate(
    cols_to_select = purrr::map(
      .x = seq_along(vec_tests),
      .f = \(z) {
        c(vec_other_cols, vec_tests[1:z])
      }
    )
  ) %>%
  mutate(
    selected_dat = purrr::map(
      .x = cols_to_select,
      .f = \(z) {
        select(dft_poss_app, all_of(z))
      }
    )
  )

dft_step %<>%
  mutate(
    hdrug_det = purrr::map(
      .x = selected_dat,
      .f = \(z) {
        # by default selects all columns starting with "test"
        add_check_multiple_tests(dat_poss_app = z) %>%
          summarize_possible_approvals_no_fail_type(.)
      }
    )
  )

dft_step <- dft_step %>%
  select(step, hdrug_det) %>%
  unnest(hdrug_det)

# for coherence we will add a row representing no tests at all (raw data)
dft_step_start <- dft_step %>%
  # just to select one row per drug use:
  filter(step %in% "test_ind_exists") %>%
  mutate(step = "<raw data>", valid_ind_exists = T)

dft_step <- bind_rows(
  dft_step_start,
  dft_step
) %>%
  mutate(step = fct_inorder(step))


readr::write_rds(
  x = dft_step,
  file = here(dir_output, 'stepwise_dat.rds')
)

# We will nest the data and do an anti-join.  This gets us more
#   consistent output than filtering the very-long data in the case
#   when a test produces no new off label uses (quite common)
dft_off_label <- dft_step %>%
  nest(dat = c(-step)) %>%
  mutate(
    off_label = purrr::map(
      .x = dat,
      .f = \(z) {
        filter(z, !valid_ind_exists)
      }
    ),
    # custom lag with the default being the first row:
    lag_off_label = off_label[c(1, 1:(n() - 1))],
    new_off_label = purrr::map2(
      .x = off_label,
      .y = lag_off_label,
      .f = \(x, y) {
        anti_join(
          x,
          y,
          by = c(
            'cohort',
            'record_id',
            'ca_seq',
            'regimen_number',
            'drug_number',
            'agent',
            'valid_ind_exists'
          )
        )
      }
    )
  )

dft_off_label %<>%
  select(step, off_label, new_off_label) %>%
  mutate(
    n_new_olu = purrr::map_dbl(
      .x = new_off_label,
      .f = nrow
    )
  )

readr::write_rds(
  x = dft_off_label,
  file = here(dir_output, 'stepwise_new_off_label.rds')
)


dft_step_cohort <- dft_step %>%
  group_by(step, cohort) %>%
  summarize(
    n_off_label = sum(!valid_ind_exists),
    n_on_label = sum(valid_ind_exists),
    n_total = n(),
    prop_off_label = n_off_label / n_total,
    .groups = "drop"
  )

readr::write_rds(
  x = dft_step_cohort,
  file = here(dir_output, 'step_sum_cohort.rds')
)


dft_rect <- tibble(
  y1 = 0.5 + seq(0, 200, by = 2),
  y2 = 1.5 + seq(0, 200, by = 2)
) %>%
  mutate(x1 = -1, x2 = 1000) # may need to jump this up.


gg_step_cohort_n <- ggplot(
  data = mutate(dft_step_cohort, step = fct_rev(step)),
  aes(y = step, x = n_off_label, color = cohort, group = cohort)
) +
  geom_rect(
    inherit.aes = F,
    data = dft_rect,
    aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2),
    fill = 'gray90',
    color = 'gray90',
    size = 0.01
  ) +
  geom_line() +
  theme_bw() +
  theme(
    strip.text = element_text(hjust = 0),
    legend.position = "bottom",
    axis.text.y = element_text(hjust = 1),
    panel.grid = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  scale_x_continuous(
    expand = expansion(add = c(0, 0.01), mult = c(0, 0)),
    n.breaks = 6,
    name = "Count of off label exposures"
  ) +
  scale_color_vibrant() +
  coord_cartesian(
    xlim = c(0, max(dft_step_cohort$n_off_label)),
    ylim = c(1, length(unique(dft_step_cohort$step)))
  ) +
  scale_y_discrete(position = "right") +
  scale_color_vibrant() +
  labs(
    y = paste0(
      "←first",
      paste(rep(" ", 10), collapse = ""),
      "Step",
      paste(rep(" ", 10), collapse = ""),
      "last→"
    )
  )


readr::write_rds(
  gg_step_cohort_n,
  here(dir_output, 'gg_step_cohort_n.rds')
)

dft_rect <- tibble(
  y1 = 0.5 + seq(0, 200, by = 2),
  y2 = 1.5 + seq(0, 200, by = 2)
) %>%
  mutate(x1 = -1, x2 = 1000) # may need to jump this up.


gg_step_cohort_prop <- ggplot(
  data = mutate(dft_step_cohort, step = fct_rev(step)),
  aes(y = step, x = prop_off_label, color = cohort, group = cohort)
) +
  geom_blank() +
  geom_rect(
    inherit.aes = F,
    data = dft_rect,
    aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2),
    fill = 'gray90',
    color = 'gray90',
    size = 0.01
  ) +
  geom_line() +
  theme_bw() +
  theme(
    strip.text = element_text(hjust = 0),
    legend.position = "bottom",
    axis.text.y = element_text(hjust = 1),
    panel.grid = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  scale_x_continuous(
    expand = expansion(add = c(0, 0.01), mult = c(0, 0)),
    n.breaks = 6,
    labels = label_percent(),
    name = "Prop off label"
  ) +
  scale_color_vibrant() +
  scale_y_discrete(position = "right") +
  coord_cartesian(
    xlim = c(0, max(dft_step_cohort$prop_off_label)),
    ylim = c(1, length(unique(dft_step_cohort$step)))
  ) +
  labs(
    y = paste0(
      "←last",
      paste(rep(" ", 10), collapse = ""),
      "Step",
      paste(rep(" ", 10), collapse = ""),
      "first→"
    )
  )

gg_step_cohort_prop

readr::write_rds(
  gg_step_cohort_prop,
  here(dir_output, 'gg_step_cohort_prop.rds')
)
