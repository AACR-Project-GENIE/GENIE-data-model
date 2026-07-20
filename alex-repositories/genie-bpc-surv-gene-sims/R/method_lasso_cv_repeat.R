#' @description Lasso with cross validation, done repeatedly on the same data to assess reliability of results.  There is no bootstrapping or subsampling at all in this method.
#' @param dat Required columns:  x, y, event.
#' @param main_seed The main seed used for generating r
#' @param ignore_cols Columns to exclude from the covariate matrix.
#' @param rep Repetitions to do - default 100.
method_lasso_cv_repeat <- function(
    dat,
    main_seed,
    ignore_cols = "id_obs",
    rep = 100
) {
  
  res <- surv_fit_no_boot_sim(
    dat, 
    main_seed = main_seed, 
    rep = rep,
    ignore_cols = ignore_cols
  )
  
  res <- resample_coef_helper(
    res,
    # These are all default, just making them explicit:
    col_with_fits = "fit",
    filter_log_hr_above = 4.60517,
    stab_thres = NULL # we'll do this later on.
  )
  
  return(res)
  
}