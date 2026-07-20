

plot_alt_onco_impact <- function(
    dat,
    y_var,
    x_var = "n",
    x_lab = "Alterations (bulk count)",
    y_lab = NULL,
    plot_subtitle = NULL
) {
  
  pal_onco <- diverging_hcl(
    palette = "Tropic", n = 5
  )
  
  dat %<>%
    mutate(var_div = if_else(
      oncogenic %in% c("Oncogenic", "Likely Oncogenic"),
      .data[[x_var]],
      -.data[[x_var]])
    ) %>%
    arrange(.data[[y_var]], oncogenic)
  
  gg <- ggplot(
    dat,
    aes(x = var_div, fill = oncogenic, y = .data[[y_var]])
  ) + 
    geom_col(
      position = "stack", orientation = 'y'
    ) +
    scale_fill_manual(
      values = pal_onco,
      name = NULL
    ) +
    guides(
      fill = guide_legend(reverse = T)
    ) + 
    labs(
      y = y_lab,
      x = x_lab,
      title = "Oncogenic annotation impact",
      subtitle = plot_subtitle
    ) + 
    theme_classic() + 
    theme(
      legend.position = "bottom",
      plot.title.position = "plot"
    )
}
