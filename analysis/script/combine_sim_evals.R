# Description: Combines the simulation runs into dataframes for display and analysis.

library(fs); library(here); library(purrr);
purrr::walk(.x = fs::dir_ls('R'), .f = source)


read_wrap <- function(f) {
  readr::read_rds(
    here(
      'sim', 'evaled_methods', f
    )
  )
}

n80_cox <- read_wrap('gen_dat_one_n80_cox_uni.rds')
n500_cox <- read_wrap('gen_dat_one_n500_cox_uni.rds')

n80_lasso_5fcv <- read_wrap('gen_dat_one_n80_lasso_5fcv.rds')
n500_lasso_5fcv <- read_wrap('gen_dat_one_n500_lasso_5fcv.rds')

n80_lasso_loocv <- read_wrap('gen_dat_one_n80_lasso_loocv.rds')
n500_lasso_loocv <- read_wrap('gen_dat_one_n500_lasso_loocv.rds')

n80_lasso_cv_boot <- read_wrap(
  'gen_dat_one_n80_lasso_cv_boot_f200.rds' 
)
n500_lasso_cv_boot <- read_wrap(
  'gen_dat_one_n80_lasso_cv_boot_f200.rds' 
)



n80_cox %<>% mutate(n = 80)
n500_cox %<>% mutate(n = 500)

n80_lasso_5fcv %<>% mutate(n = 80)
n500_lasso_5fcv %<>% mutate(n = 500)

n80_lasso_loocv %<>% mutate(n = 80)
n500_lasso_loocv %<>% mutate(n = 500)

n80_lasso_cv_boot %<>% mutate(n = 80)
n500_lasso_cv_boot %<>% mutate(n = 500)

# Commenting this out for now, you can return for plots later on:
# cli::cli_alert_danger(
#   text = "Temp fix:  Cutting all models down to the first 200 runs for matching"
# )
# n80_cox %<>% slice(1:200)
# n500_cox %<>% slice(1:200)
# n80_lasso_5fcv %<>% slice(1:200)
# n500_lasso_5fcv %<>% slice(1:200)
# n80_lasso_loocv %<>% slice(1:200)
# n500_lasso_loocv %<>% slice(1:200)




sim_sum_all <- bind_rows(
  n80_cox,
  n500_cox,
  n80_lasso_5fcv,
  n500_lasso_5fcv,
  n80_lasso_loocv,
  n500_lasso_loocv,
  n80_lasso_cv_boot,
  n500_lasso_cv_boot
)

lev_meth <- c(
  "Univariate models",
  "LASSO (5-CV)",
  "LASSO (LOOCV)",
  "LASSO (5-CV boot)"
)

sim_sum_all %<>% select(
  id, gen_method, n, analysis_method, auc, 
  contains("bias"),
  contains("sens"),
  contains("spec")
) %>%
  mutate(
    n_lab = glue("n={n}"),
    n_lab = fct_inorder(n_lab)
  ) %>%
  mutate(
    analysis_method_f = case_when(
      analysis_method %in% "method_univar_cox" ~ lev_meth[1],
      analysis_method %in% "method_lasso_5fcv" ~ lev_meth[2],
      analysis_method %in% "method_lasso_loocv" ~ lev_meth[3],
      analysis_method %in% "method_lasso_cv_boot" ~ lev_meth[4]
    ),
    analysis_method_f = factor(
      analysis_method_f,
      levels = lev_meth
    )
  )
    

sim_sum_avg <- sim_sum_all %>%
  group_by(analysis_method_f, n_lab) %>%
  summarize(
    across(
      .cols = c(auc, avg_bias, avg_abs_bias, sens_at_thresh, spec_at_thresh),
      .fns = mean
    ),
    .groups = "drop"
  )

readr::write_rds(
  x = sim_sum_all,
  file = here('sim', 'combined_evals', 'sim_sum_all.rds')
)

readr::write_rds(
  x = sim_sum_avg,
  file = here('sim', 'combined_evals', 'sim_sum_avg.rds')
)
