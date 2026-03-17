# Description:  Top level workflow for the simulations.

library(purrr)
library(here)
library(fs)

purrr::walk(.x = fs::dir_ls('R'), .f = source)


# Simulation specific setup.  Does require some previous steps to be done.
source(here('analysis', 'script', 'sim_folder_setup.R'))
source(here('analysis', 'script', 'create_surv_data_real.R'))


# Cox univarate models (runs quickly - about 2 minutes with 6 workers)
source(here('analysis', 'script', 'run_method_univar_cox.R'))
source(here('analysis', 'script', 'run_method_lasso_5fcv.R'))
source(here('analysis', 'script', 'run_method_lasso_loocv.R'))
source(here('analysis', 'script', 'run_method_lasso_cv_boot.R'))

# Lasso method with 5-fold CV - just one time.
source(here('analysis', 'script', 'eval_univar_cox.R'))
source(here('analysis', 'script', 'eval_univar_cox_fdr_corrected.R'))
source(here('analysis', 'script', 'eval_lasso_5fcv.R'))
source(here('analysis', 'script', 'eval_lasso_loocv.R'))
source(here('analysis', 'script', 'eval_lasso_cv_boot.R'))

# Combine all the simluation evaluations for display
source(here('analysis', 'script', 'combine_sim_evals.R'))
source(here('analysis', 'script', 'upload_sim_data_backup.R'))
