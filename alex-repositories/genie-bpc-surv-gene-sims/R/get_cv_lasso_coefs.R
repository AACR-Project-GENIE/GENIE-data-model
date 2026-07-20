
# values for lambda_value are lambda.1se or lambda.min or a fixed lambda.
get_cv_lasso_coefs <- function(las_fit, lambda_value = 'lambda.min') {
  las_fit %>%
    coef(., s = lambda_value) %>%
    # Need the coefficients on log scale to average.
    tidy_cv_glmnet(., exp_coef = F, remove_zero = F)
}
