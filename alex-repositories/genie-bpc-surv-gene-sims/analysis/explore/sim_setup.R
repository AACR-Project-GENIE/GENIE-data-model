# Description: Figuring out the weibull dis, etc. 

library(fs)
library(here)
library(purrr)

purrr::walk(.x = fs::dir_ls('R'), .f = source)

dft_surv_dmet <- readr::read_rds(
  file = here('data', 'survival', 'prepared_data',
              'surv_dmet_all.rds')
)

# grab the binary covariates for now:
sim_skel <- dft_surv_dmet %>% 
  select(
    record_id, 
    white, hispanic, 
    matches("_mut$"),
    matches("_cna$"),
    matches("_fus$")
  )

colnames(sim_skel) <- c(
  "sim_rec_id", 
  paste0(
    "x_", 
    str_pad(
      1:(ncol(sim_skel)-1), 
      width = 3,
      pad = 0, 
      side = "left"
    )
  )
)

# Important:  set a seed for random reshuffling of rows.  This is critical 
# because people will be truncated in this study, so our n will draw the first
# rows first to get a consistent sample size.
set.seed(23098)

sim_skel %<>% 
  sample_frac(., size = 1, replace = F) %>%
  add_id(., prefix = "rec-", name = "sim_rec_id") %>%
  # make sure all the non-record ID columns are doubles:
  mutate(
    across(
      .cols = -sim_rec_id,
      .fns = as.double
    )
  )



test_x <- select(sim_skel, -sim_rec_id)
test_beta <- test_x %>% 
  create_surv_beta_unif(., proportion_null = 0.8)
# test_x %<>% as.matrix(.)

gen_data_one(
  dat = test_x, beta = test_beta,
  surv_shape = 0.7, surv_scale = 0.3,
  trunc_shape = 0.9, trunc_scale = 1,
  censor_min = 5, censor_max = 20,
  limit_obs_n = 500,
  return_type = "observed_combined"
) %>%
  glimpse


# Next steps:
# - cleanup
# - repeat for 1000 betas and 1000 dataframes.
test_beta*test_x

-log(runif(nrow(test_x), 0, 1))/exp(t(test_x)%*%test_beta)

# A super simple example to make sure I'm lot losing it:
# The frist covariate increases risk, the rest do nothing.
test_beta = c(0.7, 0, 0, 0)
# Only participant #2 has the fist coviarate positive, so they should be at higher risk.
test_x = matrix(c(0, 1, 1, 1,
                  1, 0, 0, 0,
                  0, 1, 0, 0),
                nrow = 3,
                byrow = T)
test_x %*% test_beta # col vector output
t(test_beta) %*% t(test_x) # row vector output.
purrr::map_dfr(.x = 1:100, .f = (function(do_nothing) {
  vec <- -log(runif(nrow(test_x), 0, 1))/exp(test_x %*% test_beta)
  tibble(y1 = vec[1], y2 = vec[2], y3 = vec[3])
})) %>%
  colMeans(.)
# They tend to die sooner, great.

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

rweibull_new_params <- function(n, shape, scale) {
  sigma = 1/scale # a = p so no transform needed there.
  rweibull(n, shape = shape, scale = sigma)
}




# test one:
gen_time_cox_weibull(test_beta, test_x, shape = 0.5, scale = 1)

gen_time_cox_weibull(test_beta, test_x, shape = 0.7, scale = 0.25)
# test mnay times:
purrr::map_dfr(.x = 1:100, .f = (function(do_nothing) {
  vec <- gen_time_cox_weibull(test_beta, test_x, shape = 0.9, scale = 0.25)
  tibble(y1 = vec[1], y2 = vec[2], y3 = vec[3])
})) %>%
  colMeans(.)
# Mean for the undisturbed (no covaritaes affect) vectors should be
gamma(1+1/0.9)/0.25
# So that seems about right.
# Median should be 
log(2)^(1/0.7)/0.15

# you can use x = 0, beta = 0 to draw from a straight weibull:
gen_time_cox_weibull(0, matrix(0), 0.9, 0.25)
purrr::map_dbl(.x = rep(0, 10000), .f = (function(x) {
  gen_time_cox_weibull(x, matrix(0), 0.9, 0.25) 
})) %>%
  median(.)
# which allows us to check our function:
rweibull_new_params(n = 10000, shape = 0.9, scale = 0.25) %>% median(.)


gen_data_one(
  dat = test_x, beta = test_beta,
  surv_shape = 0.7, surv_scale = 0.3,
  trunc_shape = 0.9, trunc_scale = 1,
  censor_min = 5, censor_max = 20,
  return_type = "observed_combined"
)


plot_weib_draws(0.9, 1)

# New test data to test some extremes: person 1 is low risk, person 2 high, person 3 just straight weibull draws
test_x = matrix(c(0, 1, 1, 1,
                  1, 0, 0, 0,
                  0, 1, 0, 0),
                nrow = 3,
                byrow = T)
test_beta = c(0.5, 0, -0.5, 0)

# Good starting parameters:
purrr::map_dfr(
  .x = 1:1000,
  .f = (function(x) {
    gen_data_one(
      dat = test_x, beta = test_beta,
      surv_shape = 0.7, surv_scale = 0.3,
      trunc_shape = 0.9, trunc_scale = 2,
      censor_min = 4, censor_max = 15
    )
  })
) %>%
  group_by(id) %>%
  summarize(
    median_t = median(t),
    median_x = median(x),
    prop_trunc = mean(x > t),
    prop_event = mean(x <= t & event %in% 1),
    prop_censored = mean(x <= t & event %in% 0)
  )






