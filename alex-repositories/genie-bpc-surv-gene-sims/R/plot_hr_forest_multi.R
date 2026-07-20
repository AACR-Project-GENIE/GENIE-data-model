plot_hr_forest_multi <- function(
    coef_dat, 
    strat_var,
    legend_title = NULL,
    plot_title = NULL,
    pal = c("black", "#ee7733", "#0077bb", "#ee3377"),
    pt_size = 2
) {
  
  
  gg <- ggplot(coef_dat,
               aes(x = hr, y = term)) + 
    geom_vline(xintercept = 1, color = "#bb5566", 
               linewidth = 2, alpha = 0.5) + 
    geom_jitter(aes(color = .data[[strat_var]]), width = 0.1, height = 0,
               shape = 1, size = pt_size, stroke = 1)  +
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
    scale_color_manual(
      values = pal, name = NULL
    )
  
  return(gg)
}
