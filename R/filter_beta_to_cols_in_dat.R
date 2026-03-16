#' @description Filter a named list of variables to those in the dataset.
#' 
#' @details Used to take a dataset with nonzero cols removed, then apply that to the true covariates list.
#' 
#' @param beta a character vector of (true) coef values.
#' @param dat A dataset with the valid columns (extras are fine)
filter_beta_to_cols_in_dat <- function(beta, dat) {
  beta <- beta[names(beta) %in% names(dat)]
  return(beta)
}