#' @description Grabs a dataframe of all covariates at every lambda value from a cv.glmnet object.
#' @param cv_glmnet_fit The output of cv.glmnet.
get_coefs_all_lambda <- function(cv_glmnet_fit) {
  lam_vals <- cv_glmnet_fit$lambda # whole lambda trace
  
  coef_dat <- coef(cv_glmnet_fit, s = lam_vals)
  colnames(coef_dat) <- paste0("l_", lam_vals)
  
  coef_dat %<>% 
    as.matrix(.) %>%
    as_tibble(rownames = "term")
  
  coef_dat %<>%
    pivot_longer(
      cols = -term,
      names_to = "lambda",
      values_to = "estimate"
    )
  
  coef_dat %<>%
    mutate(
      lambda = str_sub(lambda, 3),
      lambda = as.numeric(lambda)
    )
  
  return(coef_dat)
}