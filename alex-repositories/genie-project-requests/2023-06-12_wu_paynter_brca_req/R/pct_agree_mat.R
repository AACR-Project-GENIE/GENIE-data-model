

pct_agree_mat <- function(dat) {
  if (is.data.frame(dat)) {
    dat <- as.matrix(dat)
  }
  
  # testing equality of each pair, combine into a with a mean.
  gen_mmult(`==`, mean)(t(dat), dat)
}
