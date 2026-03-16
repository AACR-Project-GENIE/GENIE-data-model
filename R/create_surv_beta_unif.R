#' @param dat The data, only including columns a beta value is desired for.
#' @param log_beta_min The minimum value of log(beta) which can be drawn.
#' @param log_beta_max The maximum value of log(beta) which can be drawn.
#' @param proportion_zero The proportion of values set to exactly 1 (0 on the log scale).  The floor function is taken to round this.
#' @param return_exp Return exponentiated (on the hazard ratio scale) coefficients?
#' @param seed The seed for drawing the beta vector.
create_surv_beta_unif <- function(
    dat, 
    log_beta_min = -0.693, 
    log_beta_max = 0.693,
    proportion_null = 0.8,
    return_exp = F,
    seed = NULL) {
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  len_beta <- ncol(dat)
  beta_null <- 1:len_beta %in% sample(
    1:len_beta, 
    floor(proportion_null*len_beta), 
    replace = F
  ) 
  
  beta <- if_else(
    beta_null,
    0,
    runif(n = len_beta, log_beta_min, log_beta_max)
  )
  
  names(beta) <- colnames(dat)
  
  if (return_exp) {
    return(exp(beta))
  } else {
    return(beta)
  }
  
}
