
plot_one_sim_metric <- function(
    dat_sim_all,
    dat_sim_avg,
    x_var,
    x_lab
) {
  gg <- ggplot(
    data = dat_sim_all, 
    aes(
      x = .data[[x_var]],
      y = analysis_method_f, 
      color = analysis_method_f
    )
  ) +
    geom_vline(xintercept = 0, linetype = "13") + 
    geom_boxplot(outlier.shape = NA, coef = 0) + 
    geom_jitter(height = 0.2, width = 0, alpha = 0.5, size = 0.25) +   
    geom_point(data = dat_sim_avg,
               size = 3, stroke = 1, alpha = 1, shape = 20,
               color = "gray50") +
    facet_wrap(vars(n_lab)) + 
    theme_classic() + 
    scale_color_vibrant() + 
    labs(x = x_lab) + 
    theme(
      legend.position = "none",
      axis.title.y = element_blank()
    )
  
  return(gg)
}
