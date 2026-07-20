plot_boxplot_by_panel <- function(
    dat,
    x_var,
    x_lab = NULL,
    plot_title = NULL,
    plot_subtitle = NULL,
    add_to_x = 0.1
) {      
  
  gg <- ggplot(
    data = dat,
    aes(
      x = .data[[x_var]] + add_to_x, 
      y = panel_lab,
      fill = institution
    )
  ) + 
    stat_boxplot(outlier.shape = NA, coef = 100) + 
    # geom_jitter(width = 0, height = 1) + 
    theme_classic() + 
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = x_lab
    ) +
    theme(
      plot.title.position = "plot",
      axis.title.y = element_blank(),
      axis.text.y = element_text(hjust = 0),
      strip.text = element_text(hjust = 0)
    ) + 
    scale_x_continuous(
      expand = expansion(mult = 0.01, add = 0),
      trans = 'log10'
    ) +
    scale_y_discrete(limits=rev) + 
    guides(fill = "none") + 
    facet_wrap(
      vars(sample_type_simple_f), 
      ncol = 1,
      scales = "free" # easiest way to get scales added in.
    ) + 
    coord_cartesian(
      xlim = c(NA, max(dat[[x_var]] + add_to_x * 2))
    ) + 
    scale_fill_vibrant()
  
  return(gg)
  
}


