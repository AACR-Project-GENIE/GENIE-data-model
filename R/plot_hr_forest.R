plot_hr_forest <- function(coef_dat, 
                           pt_color_col,
                           legend_title = NULL,
                           plot_title = NULL,
                           vir_opt = "mako",
                           vir_begin = 0.8,
                           vir_end = 0.2,
                           pt_shape = 15,
                           pt_size = 4) {
  gg <- ggplot(coef_dat,
               aes(x = hr, y = feature)) + 
    geom_vline(xintercept = 1, color = "#bb5566", 
               linewidth = 2, alpha = 0.5) + 
    geom_point(aes(color = .data[[pt_color_col]]),
               shape = pt_shape, size = pt_size)  +
    theme_bw() + 
    scale_x_continuous(n.breaks = 8) + 
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      plot.title.position = 'plot',
      legend.position = "bottom"
    ) + 
    labs(
      x = "Hazard ratio",
      title = plot_title,
      y = NULL
    ) + 
    scale_color_viridis_c(
      option = vir_opt, begin = vir_begin, end = vir_end
    )
  
  return(gg)
}
