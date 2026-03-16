# taking the parametrization h(t|a,p) = pa^pt^(p-1), so that
# H(t) = a^pt^p, and if p < 1 (shape < 1) the hazard is decreasing, p > 1 => increasing.
# See Marco Carone's course notes for this parametrization.
# p = shape, lambda = scale.
rweibull_new_params <- function(n, shape, scale) {
  sigma = 1/scale # a = p so no transform needed there.
  rweibull(n, shape = shape, scale = sigma)
}
