
plot_weib_draws <- function(shape, scale, n = 1000, alpha = 0.25) {
  dat <- tibble(
    draws = replicate(
      gen_time_flat_weibull(shape, scale),
      n = n
    )
  )
  
  true_median <- (log(2)^(1/shape))/scale
  
  ggplot(data = dat,
         aes(x = draws, y = -0.5)) +
    stat_ecdf() + 
    annotate(geom = "point", x = true_median, y = 0.5, color = "red", size = 1) + 
    annotate(geom = "text", x = true_median + 1, y = 0.5, color = "red",
             label = paste0("Derived median:", round(true_median,2)),
             hjust = 0
    ) + 
    # geom_boxplot(outlier.shape = NA) + 
    geom_jitter(height = 0.25, width = 0, alpha = 0.5) + 
    theme_classic() + 
    coord_cartesian(xlim = c(0,20)) # wildly arbitrary, helps me.
}
