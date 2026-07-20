library(purrr)
library(here)
library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

output_dir <- here('output', 'manu', 'fig1')

dft_drug_tracking <- readr::read_rds(
  here('data', 'cohort', 'drug_tracking_02.rds')
)

dft_drug_tracking %<>%
  mutate(step = forcats::fct_inorder(step))

drug_sum_help <- function(dat) {
  dat %>%
    summarize(
      n_uses = n(),
      n_drug = length(unique(agent)),
      n_pts = length(unique(record_id))
    )
}

dft_flow_by_cohort <- dft_drug_tracking %>%
  mutate(
    sum = purrr::map(
      .x = drug_key,
      .f = \(x) {
        x %>%
          group_by(cohort) %>%
          drug_sum_help(.)
      }
    )
  ) %>%
  select(-drug_key) %>%
  unnest(sum)

dft_flow_by_cohort %<>%
  fix_cohort_names(.) %>%
  mutate(
    cohort = cohort_release_order(cohort)
  )

dft_flow_by_cohort_long <- dft_flow_by_cohort %>%
  select(step, cohort, n_exposures = n_uses, n_pts, n_drug) %>%
  pivot_longer(
    cols = matches("^n_"),
    names_to = 'metric',
    values_to = 'value'
  ) %>%
  mutate(metric = str_sub(metric, 3, end = -1L))

dft_flow_by_cohort_long %<>%
  group_by(cohort, metric) %>%
  arrange(step) %>%
  mutate(
    prop_diff = (lag(value) - value) / lag(value),
    prop_lost = 1 - value / max(value)
  ) %>%
  ungroup(.)

dft_flow_by_cohort_long %<>%
  mutate(
    metric = case_when(
      metric %in% "pts" ~ "Patients",
      metric %in% 'drug' ~ 'Drugs',
      T ~ str_to_title(metric)
    ),
    metric = factor(metric, levels = c('Exposures', 'Patients', 'Drugs'))
  )


attrition_triple_plot <- function(
  dat,
  col,
  universal_scale_max = NULL,
  rel_widths = c(0.45, 0.25, 0.25),
  legend_rect_breaks = 9
) {
  metric_levs <- dat %>% count(metric) %>% arrange(metric) %>% pull(metric)

  # we want three scales, so we make a helper here:
  plot_help <- function(
    dat,
    met,
    pal_col,
    y_labs = F
  ) {
    dat %<>%
      filter(metric %in% met) %>%
      mutate(step = fct_rev(step))

    gg <- ggplot(dat, aes(x = cohort, y = step, fill = .data[[col]])) +
      geom_tile(color = 'gray20') +
      ggsci::scale_fill_material(
        palette = pal_col,
        labels = scales::label_percent(),
        name = met,
        limits = if (!is.null(universal_scale_max)) {
          c(0, universal_scale_max)
        } else {
          NULL
        }
      ) +
      scale_x_discrete(expand = c(0, 0), name = NULL) +
      scale_y_discrete(expand = c(0, 0), name = NULL) +
      theme(
        legend.position = 'bottom',
      ) +
      guides(
        fill = guide_colorbar(
          nbin = legend_rect_breaks,
          display = 'rectangles'
        )
      )

    if (!y_labs) {
      gg <- gg +
        theme(
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank()
        )
    }
    gg
  }

  composite <- cowplot::plot_grid(
    plot_help(dat, met = metric_levs[1], pal_col = 'deep-orange', y_labs = T),
    plot_help(dat, met = metric_levs[2], pal_col = 'blue'),
    plot_help(dat, met = metric_levs[3], pal_col = 'green'),
    nrow = 1,
    rel_widths = rel_widths
  )

  composite
}

fig1_stepwise_prop_diff <- dft_flow_by_cohort_long %>%
  filter(!(step %in% 'Raw data')) %>%
  attrition_triple_plot(
    dat = .,
    col = 'prop_diff',
    universal_scale_max = 1
  )

fig1_total_lost <- dft_flow_by_cohort_long %>%
  attrition_triple_plot(
    dat = .,
    col = 'prop_lost',
    universal_scale_max = 1
  )

manu_save_helper(
  plot = fig1_stepwise_prop_diff,
  dir = output_dir,
  name = 'manu-fig1-attrition-stepwise',
  height = 3,
  width = 14
)

manu_save_helper(
  plot = fig1_total_lost,
  dir = output_dir,
  name = 'manu-fig1-attrition-cumu',
  height = 3,
  width = 14
)
