plot_pct_agree <- function(
  mat,
  remove_upper_tri = T,
  remove_diag = T
) {
  col_lev <- colnames(mat)
  
  dat <- mat %>%
    as_tibble(., rownames = "var1") %>%
    pivot_longer(
      cols = -var1,
      names_to = "var2",
      values_to = "pct_agree"
    ) %>%
    mutate(
      var1 = factor(var1, levels = col_lev, ordered = T),
      var2 = factor(var2, levels = col_lev, ordered = T)
    )
  
  if (remove_upper_tri) {
    dat %<>% filter(var1 <= var2)
  } 
  
  if (remove_diag) {
    dat %<>% filter(var1 != var2)
  }
  
  
  gg <- ggplot(
    data = dat,
    aes(x = var1, y = var2, fill = pct_agree)
  ) + 
    geom_tile() + 
    scale_x_discrete(drop = F) +
    scale_y_discrete(drop = F) + 
    scale_fill_viridis_c(
      option = "rocket",
      begin = 0,
      end = 1,
      limits = c(0,1)
    ) + 
    theme_classic()
  
  
  return(gg)
  
  
}