plot_regimen_sunburst <- function(sun_dat, seed = 94, pal = NULL) {
  if (is.null(pal)) {
    set.seed(seed)
    pal <- make_sun_pal(nrow(sun_dat))
  }
  js <- sunburstR::sunburst(
    sun_dat,
    legend = F,
    count = T,
    colors = pal
  )
  return(js)
}