#' @description Find the largest lambda for each term where the coefficient was nonzero (i.e. an associated predictor)
#' @param all_coef_df The output of get_coefs_all_lambda(.)
#' @param tol The tolerance for a variable to be considered nonzero (absolute value threshold).
find_dropout_lambda <- function(all_coef_df, tol = 0.00001) {
  rtn <- all_coef_df %>%
    group_by(term) %>%
    arrange(lambda) %>%
    mutate(zeroed = abs(estimate) < tol) %>%
    filter(zeroed) %>%
    slice(1) %>%
    ungroup(.) %>%
    select(-zeroed)
  
  return(rtn) 
}