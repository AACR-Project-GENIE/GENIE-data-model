plot_drug_use_times <- function(
  dat,
  x_var = "drug_start_date_max",
  x_lab = "Drug start dates (max possible)",
  date_breaks = "2 years",
  plot_title = NULL,
  plot_subtitle = NULL
) {
  gg <- ggplot(
    dat,
    # add half a quarter to align the bars to my liking.
    aes(x = .data[[x_var]])
  ) +
    # gives us breaks of a quarter long.
    geom_histogram(binwidth = 365.25 / 4, boundary = 0, closed = "left") +
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      y = "Exposures",
      x = x_lab
    ) +
    theme_bw() +
    theme(
      plot.title.position = "plot"
    ) +
    scale_x_date(
      date_breaks = date_breaks,
      date_labels = "%Y"
    ) +
    scale_y_continuous(
      expand = expansion(add = c(0, 0), mult = c(0, 0.02))
    )

  return(gg)
}
