# Description:  Permute the dmet surv data for simulations.

library(fs)
library(here)
library(purrr)

purrr::walk(.x = fs::dir_ls('R'), .f = source)

dft_surv_dmet <- readr::read_rds(
  file = here('data', 'survival', 'prepared_data',
              'surv_dmet_all.rds')
)

# Find features truly associated for "real" covariates
surv_obj <- with(
  dft_surv_dmet,
  Surv(
    time = tt_cpt_dmet_yrs_pos,
    time2 = tt_os_dmet_yrs,
    event = os_dx_status
  )
)

vec_vars_to_test <- dft_surv_dmet %>% 
  select(
    white, hispanic, stage_dx_iv_num, age_dx_c, birth_year_c,
    matches("_mut$"),
    matches("_cna$"),
    matches("_fus$")
  ) %>%
  names

uni_cox_helper <- function(dat, surv_obj, var) {
  if (length(unique(dat[[var]])) == 2) {
    n_pos_binary <- dat %>% filter(.data[[var]] %in% 1) %>% nrow
  } else {
    n_pos_binary <- NA
  }
  
  tryCatch(
    {rtn <- coxph(
      formula = as.formula(paste0("surv_obj ~ ", var)), 
      data = dat
    ) %>%
      broom::tidy(.)
    },
    error = tibble(term = NA, estimate = NA, std.error = NA, statistic = NA, p.value = NA, n_pos_binary = NA)
  )
    
  rtn %<>% mutate(n_pos_binary = n_pos_binary)
  
  return(rtn)
  
}

uni_cox_helper(dat = dft_dmet_surv_all, surv_obj = surv_obj, var = "white")

univariate_tests <- purrr::map_dfr(
  .x = vec_vars_to_test, 
  .f = (function(v) {
    uni_cox_helper(
      dat = dft_dmet_surv_all, 
      surv_obj = surv_obj, 
      var = v
    )
  })
)

univariate_tests %>%
  arrange(p.value) %>%
  select(term, estimate, p.value, n_pos_binary) %>%
  print(n = 100)

# Let's start with pretty arbirary choice:  I'll select some strongly associated mutations that have a range of sample sizes.
true_covariates <- c(
  "TP53_mut", # n = 408
  "ERBB2_cna", # n = 135
  "GATA3_mut", # n = 99
  "ESR1_mut", # = 30
  "EGFR_cna", # n = 15
  "JAK2_cna" # n = 6
)


  

univariate_tests %>% glimpse



coxph(surv_obj ~ white, data = dft_dmet_surv_all) %>%
  broom::tidy(.)

dft_surv_dmet %>% 
  select(
    white:birth_year_c,
    matches("_mut$"),
    matches("_cna$"),
    matches("_fus$")
  ) %>%
  names

