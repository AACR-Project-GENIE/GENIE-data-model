plot_ddr_heatmap <- function(
  dat_long,
  y_lab_col = 'custom_cancer_type_pos_lab',
  fill_col = 'pos',
  scale_name = 'Proportion altered',
  plot_title = NULL,
  plot_subtitle = NULL,
  vir_theme = 'magma',
  vir_begin = 0,
  vir_end = 1,
  tile_color = 'gray50'
) {
  ggplot(
    dat_long,
    aes(x = feature, y = .data[[y_lab_col]], fill = .data[[fill_col]])
  ) +
    geom_tile(color = tile_color) +
    scale_fill_viridis_c(
      option = vir_theme,
      begin = vir_begin,
      end = vir_end,
      name = scale_name
    ) +
    scale_y_discrete(position = 'right', expand = c(0, 0)) +
    scale_x_discrete(position = 'top', expand = c(0, 0)) +
    theme_bw() +
    labs(title = plot_title, subtitle = plot_subtitle) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      legend.position = 'top'
    )
}
