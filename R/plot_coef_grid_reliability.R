plot_coef_grid_reliability <- function(
    dat, 
    color_col = "reliability",
    ncol = 4, 
    txt_size = 2.5, 
    plot_title = NULL,
    vir_opt = "mako",
    vir_begin = 1,
    vir_end = 0.1
) {
  
  nr <- nrow(dat)
  x_pts <- rep(seq(0, .75, length.out = ncol), each = ceiling(nr/ncol))
  x_pts <- x_pts[1:nr]
  y_pts <- rep(seq(1, 0.01, length.out = ceiling(nr/ncol)), times = ncol)
  y_pts <- y_pts[1:nr]
  
  dat %<>% mutate(x = x_pts, y = y_pts)
  
  gg <- ggplot(data = dat,
               aes(x = x, y = y, label = feature, 
                   color = .data[[color_col]])) +
    geom_text(size = txt_size, hjust = 0) + 
    theme_void() + 
    theme(
      plot.title.position = "panel",
      legend.position = "bottom"
    ) + 
    labs(title = plot_title) + 
    coord_cartesian(xlim = c(0,1), ylim = c(0,1)) + 
    scale_color_viridis_c(
      option = vir_opt, begin = vir_begin, end = vir_end
    )
      
  
  return(gg)
  
}
