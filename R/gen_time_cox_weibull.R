#

# https://stats.stackexchange.com/questions/135124/how-to-create-a-toy-survival-time-to-event-data-with-right-censoring
# taking the parametrization h(t|a,p) = pa^pt^(p-1), so that
# H(t) = a^pt^p, and if p < 1 (shape < 1) the hazard is decreasing, p > 1 => increasing.
# See Marco Carone's course notes for this parametrization.
# p = shape, lambda = scale.
gen_time_cox_weibull <- function(beta, x, shape, scale) {
  num <- log(runif(n = nrow(x), 0, 1))
  denom <- as.vector((scale^shape)*exp(x %*% beta)) # constant * rel haz
  draws <- (-num/denom)^(1/shape)
  
  return(draws)
}


