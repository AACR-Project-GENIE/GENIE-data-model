#' @description Big wrapper that creates the surv object, fits the survival,
#'   returns the datasets. Because the model is really specific to this one use
#'   case, we didn't put this wrapper into the /R directory.
#' @param dat A dataset with one row per {record_id}.
#' @param rep The number of bootstrap resamples to do.
surv_fit_no_boot_sim <- function(
    dat, 
    main_seed, 
    rep = 300, 
    ignore_cols = "id_obs") { 
  
  y_dmet_os <- with(
    dat,
    Surv(
      time = x,
      time2 = y,
      event = event
    )
  )
  
  x_dmet_os <- dat %>%
    select(-c(x,y,event), -any_of(ignore_cols)) %>% 
    as.matrix(.)
  
  set.seed(main_seed) # for drawing each boot seed below
  dft_rep_dmet_os <- tibble(ind = 1:rep)%>%
    mutate(seed = sample.int(n = 10^7, size = n()))
  
  rtn <- dft_rep_dmet_os %>%
    mutate(
      fit = purrr::map(
        .x = 1:n(),
        .f = (function(x) {
          fit_cv_ltrc_lasso(
            x_mat = x_dmet_os,
            y_df = dat,
            y_t = "x",
            y_t2 = "y",
            y_e = "event"
          )
        })
      )
    )
  
  return(rtn)
}