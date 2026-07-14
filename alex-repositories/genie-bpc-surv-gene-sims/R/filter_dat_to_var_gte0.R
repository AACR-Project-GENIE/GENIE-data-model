
#' @description Filter data to only columns with some covariates.
#' 
#' @param dat The data to be filtered
#' @param ignore_cols Columns to be ignored in the filtering.  They will be returned with the data.
filter_dat_to_var_gte0 <- function(
    dat, 
    ignore_cols = c("id_obs", "x", "y", "event")
) {
  cov_dat <- dat %>% select(-all_of(ignore_cols))
  
  var_by_col <- cov_dat %>% 
    summarize(
      across(
        .cols = everything(),
        .fn = (function(x) var(x, na.rm = T))
      )
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = "variable",
      values_to = "variance"
    ) 
  
  nonzero_var_cols <- var_by_col %>%
    filter(variance > 0) %>%
    pull(variable)
  
  dat_filt <- dat %>%
    select(
      all_of(ignore_cols),
      all_of(nonzero_var_cols)
    )
  
  return(dat_filt)
  
  
}