

plot_mat_metric <- function(
    mat,
    fill_label,
    plot_title = NULL,
    plot_subtitle = NULL,
    remove_diag = T,
    remove_upper_tri = T,
    vir_opt = "magma",
    log_fill = F,
    rotate_x_labs = F 
) {
  cn <- colnames(mat)
  
  if (all(rownames(mat) != cn)) {
    cli_alert_danger("Rownames do not match col names - is this the right type of object?")
  }
  
  plot_dat <- expand_grid(
    x = cn,
    y = cn
  ) %>%
    mutate(
      val = as.vector(mat),
      x = factor(x, levels = cn, ordered = T),
      y = factor(y, levels = cn, ordered = T)
    )
  
  if (remove_diag) {
    plot_dat %<>%
      filter(!(x == y))
  }
  if (remove_upper_tri) {
    plot_dat %<>%
      filter(x <= y)
  }
  if (log_fill) {
    plot_dat %<>%
      mutate(val = -log10(val))
  }
  
  gg <- ggplot(
    data = plot_dat,
    aes(x = x, y = y, fill = val)
  ) +
    geom_tile() + 
    labs(
      title = plot_title,
      subtitle = plot_subtitle
    ) + 
    theme_classic() + 
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "bottom",
      plot.title.position = "plot",
      axis.text.x = element_text(angle = if_else(rotate_x_labs, 90, 0))
    ) + 
    scale_fill_viridis_c(
      name = fill_label,
      option = vir_opt,
      begin = 0.2, end = 0.8
    ) + 
    scale_x_discrete(drop = F) +
    scale_y_discrete(drop = F, limits = rev)
  
  
  
  return(gg)
}
