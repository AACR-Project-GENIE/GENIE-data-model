# Description:  Use the surv data for simulations.

# determines the seeds for each simualtion through a random draw below.
top_level_seed <- 2405
n_datasets <- 1000 

library(fs)
library(here)
library(purrr)

purrr::walk(.x = fs::dir_ls('R'), .f = source)

dft_surv_dmet <- readr::read_rds(
  file = here('data', 'survival', 'prepared_data',
              'surv_dmet_all.rds')
)

# grab the binary covariates for now:
sim_fodder <- dft_surv_dmet %>% 
  select(
    record_id, 
    white, hispanic, 
    matches("_mut$"),
    matches("_cna$"),
    matches("_fus$")
  )

colnames(sim_fodder) <- c(
  "sim_rec_id", 
  paste0(
    "x_", 
    str_pad(
      1:(ncol(sim_fodder)-1), 
      width = 3,
      pad = 0, 
      side = "left"
    )
  )
)

# Important:  set a seed for random reshuffling of rows.  This is critical 
# because people will be truncated in this study, so our n will draw the first
# rows first to get a consistent sample size.
set.seed(top_level_seed)

sim_fodder %<>% 
  sample_frac(., size = 1, replace = F) %>%
  add_id(., prefix = "rec-", name = "sim_rec_id") %>%
  # make sure all the non-record ID columns are doubles:
  mutate(
    across(
      .cols = -sim_rec_id,
      .fns = as.double
    )
  )
sim_fodder %<>% select(-sim_rec_id)


sim <- tibble(
  sim_seed = sample.int(n = 10^7, size = n_datasets)
)

# Add a column storing the true beta values.
sim %<>%
  mutate(
    # returns a list of vectors, each one contains the betas for that simulation.
    beta = purrr::map(
      .x = sim_seed,
      .f = (function(x) {
        create_surv_beta_unif(
          dat = sim_fodder,
          proportion_null = 0.8,
          seed = x
        )
      })
    )
  ) %>%
  add_id(.) %>%
  select(id, sim_seed, beta)

# Generate data according to those beta values.
sim_n500 <- sim %>%
  mutate(
    gen_method = "gen_dat_one",
    gen_dat = purrr::map2(
      .x = sim_seed, 
      .y = beta,
      .f = (function(x,y) {
        gen_data_one(
          # These parameters were just played around with until we got 
          #   reasonable rates of censoring and truncation (10-20% each),
          #   medians survivals in the ballpark we expect, etc.
          dat = sim_fodder, beta = y,
          surv_shape = 0.7, surv_scale = 0.3,
          trunc_shape = 0.9, trunc_scale = 2,
          censor_min = 4, censor_max = 15,
          limit_obs_n = 500,
          return_type = "observed_combined",
          # The *2 here is not needed - just feels odd to feed the exact 
          # same seeds in.
          seed = x * 2
        ) 
      })
    )
  )

sim_n500 %<>%
  mutate(
    # Valid = only columns that have some variance, i.e. not all zero.
    gen_dat_valid = purrr::map(
      .x = gen_dat,
      .f = (function(d) filter_dat_to_var_gte0(dat = d))
    ), 
    beta_valid = purrr::map2(
      .x = beta,
      .y = gen_dat_valid,
      .f = (function(b, d) filter_beta_to_cols_in_dat(beta = b, dat = d))
    )
  )

sim_n500 %<>%
  select(id, sim_seed, beta, beta_valid, gen_method, gen_dat, gen_dat_valid)


readr::write_rds(
  x = sim_n500,
  file = here("sim", "gen_data", "gen_dat_one_n500.rds")
)
      


# Do the same with n = 80 to test the n ~ p situation.

sim_n80 <- sim %>%
  mutate(
    gen_method = "gen_dat_one",
    gen_dat = purrr::map2(
      .x = sim_seed, 
      .y = beta,
      .f = (function(x,y) {
        gen_data_one(
          # These parameters were just played around with until we got 
          #   reasonable rates of censoring and truncation (10-20% each),
          #   medians survivals in the ballpark we expect, etc.
          dat = sim_fodder, beta = y,
          surv_shape = 0.7, surv_scale = 0.3,
          trunc_shape = 0.9, trunc_scale = 2,
          censor_min = 4, censor_max = 15,
          limit_obs_n = 80,
          return_type = "observed_combined",
          # The *2 here is not needed - just feels odd to feed the exact 
          # same seeds in.
          seed = x * 2
        ) 
      })
    )
  )

sim_n80 %<>%
  mutate(
    # Valid = only columns that have some variance, i.e. not all zero.
    gen_dat_valid = purrr::map(
      .x = gen_dat,
      .f = (function(d) filter_dat_to_var_gte0(dat = d))
    ), 
    beta_valid = purrr::map2(
      .x = beta,
      .y = gen_dat_valid,
      .f = (function(b, d) filter_beta_to_cols_in_dat(beta = b, dat = d))
    )
  )

sim_n80 %<>%
  select(id, sim_seed, beta, beta_valid, gen_method, gen_dat, gen_dat_valid)

readr::write_rds(
  x = sim_n80,
  file = here("sim", "gen_data", "gen_dat_one_n80.rds")
)








# # Code showing good starting parameters:
# purrr::map_dfr(
#   .x = 1:1000,
#   .f = (function(x) {
#     gen_data_one(
#       dat = test_x, beta = test_beta,
#       surv_shape = 0.7, surv_scale = 0.3,
#       trunc_shape = 0.9, trunc_scale = 2,
#       censor_min = 4, censor_max = 15,
#       return = "latent"
#     )
#   })
# ) %>%
#   summarize(
#     median_t = median(t),
#     median_x = median(x),
#     prop_trunc = mean(x > t),
#     prop_event = mean(x <= t & event %in% 1),
#     prop_censored = mean(x <= t & event %in% 0)
#   )



# For debugging - see also the sim_setup.R script in explore.
# Example of how to generate one beta and one simulated dataset:

# test_beta <- create_surv_beta_unif(
#   sim_fodder,
#   proportion_null = 0.8, 
#   seed = 2398
# )
# gen_data_one(
#   dat = sim_fodder, beta = test_beta,
#   surv_shape = 0.7, surv_scale = 0.3,
#   trunc_shape = 0.9, trunc_scale = 2,
#   censor_min = 4, censor_max = 15,
#   limit_obs_n = NULL,
#   return_type = "observed_combined",
#   seed = 398
# ) %>%
#   glimpse



