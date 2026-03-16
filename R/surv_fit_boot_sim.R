
#' @description Big wrapper that creates the surv object, fits the survival,
#'   returns the datasets. Because the model is really specific to this one use
#'   case, we didn't put this wrapper into the /R directory.
#' @param dat A dataset with one row per {record_id}.
#' @param rep The number of bootstrap resamples to do.
surv_fit_boot_sim <- function(
    dat, 
    main_seed, 
    rep = 300, 
    ignore_cols = "id_obs") { 
  
  set.seed(main_seed) # for drawing each boot seed below
  dft_rep <- tibble(ind = 1:rep)%>%
    mutate(seed = sample.int(n = 10^7, size = n()))
  
  rtn <- dft_rep %>%
    mutate(
      fit = purrr::map(
        .x = seed,
        .f = (function(b) {
          fit_boot_cv_ltrc_lasso_2(
            dat = dat,
            x_col = "x",
            y_col = "y",
            event_col = "event",
            boot_seed = b,
            cv_folds = 5,
            ignore_cols = ignore_cols
          )
        })
      )
    )
  
  return(rtn)
}
