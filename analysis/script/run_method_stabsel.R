# Description: Runs stability selection on the simulated datasets

library(fs)
library(here)
library(purrr)

purrr::walk(.x = fs::dir_ls('R'), .f = source)

sim_n80 <- readr::read_rds(
  here('sim', 'gen_data', 'gen_dat_one_n80.rds')
)

sim_n500 <- readr::read_rds(
  here('sim', 'gen_data', 'gen_dat_one_n500.rds')
)


sim_n80 %<>% slice_sample(prop = 0.01)
cli::cli_warn("sim_n80 subsetted to 1% of rows for testing - remove before full run.")

sim_n80 %<>%
  mutate(
    analysis_method = "method_stabsel",
    fit = furrr::future_map2(
      .x = gen_dat_valid,
      .y = sim_seed,
      .f = (function(d, s) {
        set.seed(s * 13)
        method_stabsel(dat = d, seed = s * 13)
      }),
      .progress = TRUE
    )
  )

readr::write_rds(
  x = sim_n80,
  file = here('sim', 'run_methods', 'gen_dat_one_n80_stabsel.rds')
)


sim_n500 %<>%
  mutate(
    analysis_method = "method_stabsel",
    fit = furrr::future_map2(
      .x = gen_dat_valid,
      .y = sim_seed,
      .f = (function(d, s) {
        set.seed(s * 13)
        method_stabsel(dat = d, seed = s * 13)
      }),
      .progress = TRUE
    )
  )

readr::write_rds(
  x = sim_n500,
  file = here('sim', 'run_methods', 'gen_dat_one_n500_stabsel.rds')
)

cli::cli_alert_success("Ran stability selection method.")
