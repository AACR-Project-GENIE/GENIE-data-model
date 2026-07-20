drop_dots <- function(dat) {
  dplyr::select(dat, -matches('^\\.'))
}
