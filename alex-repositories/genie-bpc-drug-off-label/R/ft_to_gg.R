ft_to_gg <- function(
    ft
) {
  gg <- ggplot(
    data = data.frame(x = c(0,1), y = c(0,1)),
    aes(x = x, y = y)
  ) + 
    geom_blank() +
    theme_void() + 
    annotation_custom(
      gen_grob(
        ft, 
        fit = "width",
        scaling = "full"
      ),
      xmin  = 0, xmax = 1, ymin = 0, ymax = 1
    )
  
  return(gg)
}
