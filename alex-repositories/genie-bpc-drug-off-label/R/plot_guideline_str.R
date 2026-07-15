plot_guideline_str <- function(
  dat,
  y_var,
  x_lab = "Guideline date",
  y_lab = "Number of text matches",
  plot_title = "String matches from NCCN PDF",
  plot_subtitle = NULL
) {
  gg <- ggplot(
    dat,
    aes(x = date, y = .data[[y_var]])
  ) +
    geom_point() +
    labs(
      x = x_lab,
      y = y_lab,
      title = plot_title,
      subtitle = plot_subtitle
    ) +
    scale_x_date(
      date_breaks = '2 years'
    ) +
    geom_smooth(
      se = F,
      color = "#7f7f7f55",
      method = 'loess',
      span = 1,
      linewidth = 2
    ) +
    scale_y_continuous(
      breaks = 0:10 * 10^(-4),
      labels = 0:10,
      name = "Matches<br>(per 10k char)",
      limits = c(-0.01 * 10^-4, NA),
      expand = expansion(add = c(0, 0), mult = c(0, 0.1))
    ) +
    theme_bw() +
    theme(
      plot.title.position = "plot",
      axis.title.y = element_markdown()
    )

  return(gg)
}
