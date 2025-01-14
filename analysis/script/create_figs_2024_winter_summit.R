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
  labs(title = "Average absolute bias (all coefficients)",
       subtitle = "Each point is one simulation") +
  theme(
    plot.title.position = 'plot'
  )

gg_abs_bias_selected <- plot_one_sim_metric(
  dat_sim_all = sim_sum_all,
  dat_sim_avg = sim_sum_avg,
  x_var = "avg_abs_bias_selected",
  x_lab = "Avg. absolute bias"
) + 
  labs(title = "Average absolute bias (SELECTED coefficients)",
       subtitle = "Each point is one simulation") +
  theme(
    plot.title.position = 'plot'
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

ggsave(
  plot = gg_abs_bias_selected,
  filename = here('output', 'fig', 'gg_abs_bias_selected.pdf'),
  height = 4, width = 6
)

ggsave(
  plot = gg_abs_bias_selected,
  filename = here('output', 'fig', 'gg_abs_bias_selected.jpeg'),
  height = 4, width = 6
)
  










gg_abs_bias_no_cox <- gg_abs_bias +
  coord_cartesian(xlim = c(0,1)) + 
  labs(title = "Average absolute bias over all coefficients (zoomed)")

gg_abs_bias_selected_no_cox <- gg_abs_bias_selected +
  coord_cartesian(xlim = c(0,1)) + 
  labs(title = "Average absolute bias over selected coefficients (zoomed)")

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

ggsave(
  plot = gg_abs_bias_selected_no_cox,
  filename = here('output', 'fig', 'gg_abs_bias_selected_zoom.pdf'),
  height = 4, width = 6
)

ggsave(
  plot = gg_abs_bias_selected_no_cox,
  filename = here('output', 'fig', 'gg_abs_bias_selected_zoom.jpeg'),
  height = 4, width = 6
)






gg_power_selectivity <- ggplot(
  data = sim_sum_avg,
  aes(
    y = power, x = selectivity, color = analysis_method_f
  )
) +
  theme_bw() + 
  geom_point(
    data = sim_sum_avg, stroke = 1,
    size = 3, alpha = 0.7
  ) +
  facet_wrap(vars(n_lab), scales = "free_x") +
  scale_color_vibrant(
    name = 'Analysis Method'
  ) +
  scale_x_continuous(limits = c(0,1), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  # coord_cartesian(xlim = c(1,0), ylim = c(0,1)) +
  theme(
    legend.position = "bottom",
    axis.title.y = element_markdown(angle = 0, vjust = 0.5),
    plot.title.position = 'plot'
  )  + 
  labs(
     x = "Selectivity (1 - Type I error rate)",
     y = "Power<br>(1 - Type II error rate)",
     title = "Average performance over all simulations",
     subtitle = "Ideal performance = top right"
  )

gg_power_selectivity

ggsave(
  plot = gg_power_selectivity,
  filename = here('output', 'fig', 'gg_power_selectivity.pdf'),
  height = 4, width = 10
)

ggsave(
  plot = gg_power_selectivity,
  filename = here('output', 'fig', 'gg_power_selectivity.jpeg'),
  height = 4, width = 10
)

    








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

