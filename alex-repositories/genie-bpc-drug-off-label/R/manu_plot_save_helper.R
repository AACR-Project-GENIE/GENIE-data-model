manu_plot_save_helper <- function(
  plot,
  dir,
  name,
  width,
  height,
  as_pdf = T,
  as_png = T
) {
  fs::dir_create(dir)
  if (as_pdf) {
    ggsave(
      plot,
      height = height,
      width = width,
      filename = here(dir, paste0(name, '.pdf'))
    )
  }
  if (as_png) {
    ggsave(
      plot,
      height = height,
      width = width,
      filename = here(dir, paste0(name, '.png'))
    )
  }

  invisible()
}
