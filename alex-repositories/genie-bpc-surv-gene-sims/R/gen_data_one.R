#' @param dat A tibble or matrix of numeric covariates.
#' @param beta A vector of betas to to be used in modeling covariates.
#' @param surv_shape The shape parameter for a weibull model. (surv_shape = 1 reduces it to exponential I think).
#' @param surv_scale The scale parameter for a weibull model.
#' @param trunc_shape The shape paramter for truncation time (weibull).
#' @param trunc_scale The scale parameter for truncation time (weibull).
#' @param censor_min The minimum censoring time (uniform).
#' @param censor_max The maximum censoring time.
#' @param return_type Controls how the data is returned.
#' @param seed The seed set before random draws happen.
gen_data_one <- function(
    dat,
    beta,
    surv_shape,
    surv_scale,
    trunc_shape,
    trunc_scale,
    censor_min,
    censor_max,
    return_type = "latent",
    limit_obs_n = NULL,
    seed = NULL
) {
  if (ncol(dat) != length(beta)) {
    cli::cli_abort("Dataframe must have a number of columns equivalent to the length of beta vector.")
  }
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  mat_dat <- as.matrix(dat)
  n <- nrow(dat)
  p <- ncol(dat)
  
  # t = time to event
  t <- gen_time_cox_weibull(
    beta = beta, 
    x = mat_dat, 
    shape = surv_shape, 
    scale = surv_scale
  )
  
  # x = time to truncation
  x <- rweibull_new_params(
    n = n,
    shape = trunc_shape,
    scale = trunc_scale
  )
  
  c <- runif(n = n, min = censor_min, max = censor_max)
  
  latent_df <- tibble(x = x, t = t, c = c) 
  
  latent_df %<>% 
    add_id(., prefix = "s-", name = "id") %>% 
    mutate(
      event = if_else(t < c, 1, 0),
      y = pmin(t,c)
    ) %>%
    select(id, everything())
  
  if (!(return_type %in% "latent")) {
    observed_df <- latent_df %>%
      select(x, y, event) %>%
      mutate(obs = y >= x)
    
    observed_ind <- observed_df$obs
    
    if (!is.null(limit_obs_n)) {
      if (limit_obs_n > sum(observed_ind)) {
        cli::cli_abort("limit_obs_n is greater than the number of observed observations - adjust parameters!")
      }
      observed_ind <- if_else(cumsum(observed_ind) <= limit_obs_n,
                              observed_ind,
                              F)
    }
    
    observed_df <- observed_df[observed_ind,] %>% 
      select(-obs) %>%
      add_id(., prefix = "obs-", name = "id_obs") %>%
      relocate(id_obs, .before = 1)
    dat_limited <- dat[observed_ind,]
  }
  
  
  
  if (return_type %in% "latent") {
    return(latent_df)
  } else if (return_type %in% "observed") {
    observed_df <- observed_df %>%
      # # We do not get to see the latent survival/censor times, but we
      # #.  know the cohort entry event time (x) for those that entered the cohort.
      select(id_obs, x, y, event)
    
    return(observed_df)
  } else if (return_type %in% "observed_combined") {
    return(
      cbind(
        observed_df,
        dat_limited
      )
    )
  } else {
    cli::cli_abort(message = "Invalid return type")
  }
  
  return(rtn)
  
  
}
