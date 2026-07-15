plot_lasso_model_comp <- function(
    dat,
    pal = c("#009988", "#ee3377"),
    plot_title = NULL
    #pal = c("#004488", "#bb5566")
) {
  gg_stab <- ggplot(
    dat,
    aes(x = stability, y = term, color = model)
  ) + 
    geom_vline(xintercept = 0, linewidth = 0.75, color = "gray50") + 
    geom_point(alpha = 0.8, size = 1.5) + 
    theme_bw() + 
    scale_color_manual(values = pal) + 
    scale_y_discrete(limits = rev) + 
    labs(
      title = plot_title
    ) + 
    theme(
      axis.title.y = element_blank(),
      legend.position = "bottom",
      plot.title.position = "plot"
    ) 
  
  gg_est <- ggplot(
    dat,
    aes(x = log_hr, y = term, color = model)
  ) + 
    geom_vline(xintercept = 0, linewidth = 0.75, color = "gray50") + 
    geom_point(alpha = 0.8, size = 1.5) + 
    theme_bw() + 
    scale_color_manual(values = pal) + 
    scale_y_discrete(limits = rev) + 
    theme(
      axis.title.y = element_blank(),
      legend.position = "bottom"
    ) 
  
  comb <- cowplot::plot_grid(
    gg_stab, gg_est, nrow = 1, align = 'hv', axis = 't'
  )
  return(comb)
  
}