
plot_time_dist_violin_ecdf <- function(
    dat,
    time_var,
    time_var_lab,
    ecdf_color = "#004488",
    violin_line = "#994455",
    violin_fill = "#f5c6d0",
    plotlab_violin = "Density (quartile bars)",
    plotlab_ecdf = "eCDF",
    x_coord_cap = NULL
) {
  
  gg_violin <- ggplot(
    data = dat,
    aes(x = .data[[time_var]], y = 1)) + 
    geom_vline(xintercept = 0, color = "black", size = 0.5) + 
    geom_violin(
      fill = violin_fill, 
      color = violin_line,
      draw_quantiles = c(0.25, 0.5, 0.75)
    ) + 
    geom_jitter(
      width = 0, 
      height = 0.25, 
      alpha = 0.2, 
      size = 0.25
    ) + 
    theme_bw() + 
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      strip.text.x = element_text(hjust = 0),
      legend.position = "none"
    ) + 
    scale_x_continuous(
      name = time_var_lab,
      n.breaks = 8, 
      expand = expansion(add = 0, mult = c(0, 0.05))
    ) 
  
  gg_ecdf <- ggplot(
    data = dat,
    aes(x = .data[[time_var]])) + 
    geom_vline(xintercept = 0, color = "gray70", size = 0.5) + 
    stat_ecdf(
      pad = F,
      color = ecdf_color
    ) + 
    theme_bw() + 
    theme(
      strip.text.x = element_text(hjust = 0),
      legend.position = "none"
    ) + 
    scale_x_continuous(
      name = NULL,
      n.breaks = 4, 
      expand = expansion(add = 0, mult = c(0, 0.05))
    ) +
    scale_y_continuous(
      position = "right", name = NULL,
      breaks = seq(0, 1,by = 0.25),
      labels = c("0%", "", "50%", "", "100%"),
      expand = expansion(add = 0, mult = 0.01)
    ) 
  
  if (!is.null(x_coord_cap)) {
    gg_violin <- gg_violin +
      coord_cartesian(
        xlim = c(NA, x_coord_cap)
      )
    
    gg_ecdf <- gg_ecdf + 
      coord_cartesian(
        xlim = c(NA, x_coord_cap)
      )
  }
  
  gg_comb <- cowplot::plot_grid(
    (gg_violin + 
       labs(title = plotlab_violin)),
    (gg_ecdf + 
       labs(title = plotlab_ecdf)),
    nrow = 1,
    rel_widths = c(0.7, 0.3),
    align = 'h'
  )
  
  return(gg_comb)
}