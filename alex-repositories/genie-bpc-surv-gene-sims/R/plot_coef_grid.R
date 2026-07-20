plot_coef_grid <- function(dat, 
                           ncol = 4, 
                           txt_size = 2.5, 
                           pal = c("#004488", "#bb5566"),
                           plot_title = NULL) {
  nr <- nrow(dat)
  x_pts <- rep(seq(0, .75, length.out = ncol), each = ceiling(nr/ncol))
  x_pts <- x_pts[1:nr]
  y_pts <- rep(seq(1, 0.01, length.out = ceiling(nr/ncol)), times = ncol)
  y_pts <- y_pts[1:nr]
  
  dat <- dat %>%
    mutate(
      x = x_pts,
      y = y_pts,
      nonzero = abs(hr-1) > 0.001
    )
  
  gg <- ggplot(data = dat,
               aes(x = x, y = y, label = feature, color = nonzero)) +
    geom_text(size = txt_size, hjust = 0) + 
    theme_void() + 
    theme(
      legend.position = "none",
      plot.title.position = "panel"
    ) + 
    labs(title = plot_title) + 
    coord_cartesian(xlim = c(0,1), ylim = c(0,1)) + 
    scale_color_manual(values = pal)
  
  return(gg)
  
  
}
