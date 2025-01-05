library(fs); library(here); library(purrr)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)
pal_vib <- khroma::color("vibrant")


sim_sum_all <- readr::read_rds(
  file = here('sim', 'combined_evals', 'sim_sum_all.rds')
)
sim_sum_avg <- readr::read_rds(
  file = here('sim', 'combined_evals', 'sim_sum_avg.rds')
)


gg_abs_bias <- plot_one_sim_metric(
  dat_sim_all = sim_sum_all,
  dat_sim_avg = sim_sum_avg,
  x_var = "avg_abs_bias",
  x_lab = "Avg. absolute bias (logHR)"
) + 
  labs(title = "Average absolute bias over all coefficients",
       subtitle = "Each point is one simulation") +
  theme(
    plot.title.position = 'plot'
  )

gg_bias <- plot_one_sim_metric(
  dat_sim_all = sim_sum_all,
  dat_sim_avg = sim_sum_avg,
  x_var = "avg_bias",
  x_lab = "Bias (logHR scale)"
) 
  
ggsave(
  plot = gg_abs_bias,
  filename = here('output', 'fig', 'gg_abs_bias.pdf'),
  height = 4, width = 6
)

ggsave(
  plot = gg_abs_bias,
  filename = here('output', 'fig', 'gg_abs_bias.jpeg'),
  height = 4, width = 6
)
  
# cowplot::plot_grid(
#   gg_bias,
#   gg_abs_bias,
#   ncol = 1
# )










gg_bias_no_cox <- gg_bias +
  coord_cartesian(xlim = c(-1,1))

gg_abs_bias_no_cox <- gg_abs_bias +
  coord_cartesian(xlim = c(0,1)) + 
  labs(title = "Average absolute bias over all coefficients (zoomed)")

ggsave(
  plot = gg_abs_bias_no_cox,
  filename = here('output', 'fig', 'gg_abs_bias_zoom.pdf'),
  height = 4, width = 6
)

ggsave(
  plot = gg_abs_bias_no_cox,
  filename = here('output', 'fig', 'gg_abs_bias_zoom.jpeg'),
  height = 4, width = 6
)


# cowplot::plot_grid(
#   gg_bias_no_cox,
#   gg_abs_bias_no_cox,
#   ncol = 1
# )








gg_sens_spec <- ggplot(
  data = sim_sum_all,
  aes(
    x = spec_at_thresh, 
    y = sens_at_thresh,
    color = analysis_method_f
  )
) + 
  geom_jitter(alpha = 0.7, size = 0.5, shape = 16,
              height = 0.01, width = 0.01) +   
  geom_point(
    data = sim_sum_avg, stroke = 1,
    size = 3, alpha = 1, shape = 21, fill = 'black'
  ) +
  facet_grid(vars(n_lab), vars(analysis_method_f)) + 
  theme_bw() + 
  scale_color_vibrant(
    name = 'Over-sim means'
  ) +
  coord_cartesian(xlim = c(1,0), ylim = c(0,1)) +
  theme(
    legend.position = "bottom"
  ) + 
  labs(
    x = "Specificity (=1-FPR, note axis direction)",
    y = "Sensitivity"
  )

gg_sens_spec

