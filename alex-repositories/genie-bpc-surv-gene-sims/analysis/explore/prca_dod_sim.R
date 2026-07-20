# A stream of consciousness mess of a script to get feasibility numbers in
#  time for the DoD grant.

library(tidyverse)
library(magrittr)

# load in the functions from this project
library(fs); library(here); library(purrr)
purrr::walk(.x = fs::dir_ls('R'), .f = source)

test_beta = c(0.7, 0, 0, 0)
# Only participant #2 has the fist coviarate positive, so they should be at higher risk.
test_x = matrix(c(0, 1, 1, 1,
                  1, 0, 0, 0,
                  0, 1, 0, 0),
                nrow = 3,
                byrow = T)

# First we need to figure out what a good base parameter simulation would be.
# Our goal is the prostate metastatic cohort surv distribution:
#   - median = 3.5 years.
#   - first quartile roughly 1.5 years, third quartile roughly 6 years.
#   - when we do add censoring in, we'll shoot for about 40% censored.

# To do this we'll just make a cohort with a 
n_part <- 100000
test_beta = c(0.5) # doesnt matter at all, because all have beta = 0 in the matrix.
# Only participant #2 has the fist coviarate positive, so they should be at higher risk.
test_x = matrix(rep(1, times = n_part),
                ncol = 1,
                byrow = T)

# Ready for some legendarily unscientific optimization?  We just played with the params here
#   until summary() spit out numbers that looked good to me.
gen_time_cox_weibull(
  beta = test_beta,
  x = test_x,
  shape = 1.21,
  scale = 0.21
) %>%
  summary(.) %>%
  # quantile(., probs = c(0.5, 0.25, 0.75)) %>%
  round(., 2)


# With that out of the way we'll build a function to simulate a covariate based
#   on a univariate beta proportion.
# Throughout I'm just going to set the defaults that are correct for the very 
# specific question we want to answer.  The thing we'll vary is the true beta
# value, and the proportion who are altered.
# This function borrows a lot from gen_data_one, leaving off the truncation bits. 
gen_data_univar_cox <- function(
    beta,
    beta_prop,
    censor_min = 1, # in years
    censor_max = 10, # in years
    n_subj = 750,
    surv_shape = 1.21,
    surv_scale = 0.21
) {
  if (length(beta) != 1) {
    cli::cli_abort("This is intended for a univariate beta dependence - just one number.")
  }
  if (beta_prop >= 1 | beta_prop <= 0) {
    cli::cli_abort("The proportion of subjects altered (beta_prop) needs to be between zero and one (strict on both sides)")
  }
  
  # Rather than sampling with probability beta_prop, we select exactly
  # beta_prop people to be altered.  This reflects the fact that we already
  # know the number of people who are black, have TP53, etc - it's not a random
  # draw in our experiments.
  x_vec <- rep(0, times = n_subj)
  x_vec[sample(seq_along(x_vec), round(beta_prop*n_subj))] <- 1
  
  x_mat <- matrix(
    x_vec,
    ncol = 1
  )
  
  t <- gen_time_cox_weibull(
    beta = beta,
    x = x_mat,
    shape = surv_shape,
    scale = surv_scale
  )
  
  c <- runif(n = n_subj, min = censor_min, max = censor_max)
  
  rtn <- tibble(x1 = x_vec, t = t, c = c) 
  
  rtn %<>% 
    add_id(., prefix = "s-", name = "id") %>% 
    mutate(
      event = if_else(t < c, 1, 0),
      y = pmin(t,c)
    ) %>%
    select(id, everything())
  
  # t and c are not really observed, so we trim this up to only list the 
  #   observed values:
  rtn %<>% select(-c(t,c))
  
}

# Ok let's see how we're doing:
ex_dat <- gen_data_univar_cox(
  beta = 1,
  beta_prop = 0.02
)
ex_dat %>% summary(.)
# Mean event of 0.6 => 40% censored - good.
# x1 has exactly the correct percentage altered.
# y dist can't be checked without constructing a KM curve (because we have 
#   censoring now, yay).
n_sim <- 1000 # takes about 30s with 100.

dft_surv <- expand_grid(
  beta = log(c(1.05, 1.1, 1.2, 1.4, 1.5, 1.6, 1.8, 2.0, 3.0)),
  beta_prop = c(0.01, 0.02, 0.05, 0.1, 0.2, 0.5)
) %>%
  slice(rep(1:n(), each = n_sim)) %>%
  # sid = simulation id
  add_id(prefix = "sid_")

# generate a dataset for each row:
dft_surv %<>%
  mutate(
    dat = purrr::map2(
      .x = beta,
      .y = beta_prop,
      .f = \(x,y) gen_data_univar_cox(
        beta = x, beta_prop = y
      )
    )
  )

# Here's what one cox model fit output looks like on this:
method_univar_cox_dod_prostate(ex_dat)

# Now we do that for all the rows:
dft_surv %<>%
  mutate(
    tidy_fit = purrr::map(.x = dat, .f = method_univar_cox_dod_prostate)
  )

dft_surv %<>%
  unnest(tidy_fit) %>%
  mutate(effect_identified = p.value < 0.05) 

dft_surv_sum <- dft_surv %>%
  group_by(beta, beta_prop) %>%
  summarize(power = mean(effect_identified, na.rm = T),
            .groups = "drop")

dft_surv_sum %>%
  pivot_wider(names_from = beta_prop, values_from = power)

dft_surv_sum %<>%
  # go back to the hazard ratio scale for user friendliness:
  mutate(hr = exp(beta),
         power_lab = formatC(power, format = 'f', digits = 2))

gg_sim <- dft_surv_sum %>%
  filter(hr >= 1.1) %>%
  mutate(hr = fct_rev(factor(hr)), beta_prop = factor(beta_prop)) %>%
  ggplot(
    .,
    aes(y = hr, x = beta_prop, fill = power)
  ) + 
  theme_bw() + 
  geom_tile() + 
  geom_text(aes(label = power_lab), color = "white") +
  scale_fill_viridis_c(option = "rocket", begin = 0.7, end = 0.1) +
  labs(y = "Hazard Ratio", x = "Proportion of samples with feature",
       title = "Power to detect under univariate Cox model",
       sutitle = "Parameters designed to match GENIE BPC metastatic prostate cohort"
  )

ggsave(gg_sim, filename = here('analysis', 'explore', 'gg_dod_prostate_power.pdf'),
       height = 3, width = 6)
