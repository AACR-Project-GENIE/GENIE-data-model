model_compare_boot_lasso <- function(
    big_model_sum,
    little_model_sum,
    coef_col_name = "log_hr" # allows us to use hr or AFT coefs if needed.
) {
  req_cols <- c("term", "stability", "log_hr")
  if (!all(req_cols %in% colnames(big_model_sum))) {
    cli_abort(glue("big_model_sum must have columns {paste(req_cols, collapse = ', ')}"))
  }
  if (!all(req_cols %in% colnames(little_model_sum))) {
    cli_abort(glue("little_model_sum must have columns {paste(req_cols, collapse = ', ')}"))
  }
  
  big_model_sum %<>% 
    mutate(model = "big") %>%
    select(
      term,
      model,
      stability = stability,
      "{coef_col_name}" := all_of(coef_col_name)
    ) 
  
  vec_term_levs <- big_model_sum %>%
    arrange(desc(stability)) %>%
    pull(term)
  
  little_model_sum %<>% 
    mutate(model = "little") %>%
    select(
      term,
      model,
      stability = stability,
      "{coef_col_name}" := all_of(coef_col_name)
    )
  
  rtn <- bind_rows(
    big_model_sum,
    little_model_sum
  ) %>%
    mutate(term = factor(term, levels = vec_term_levs)) %>%
    arrange(term, model)
  
  return(rtn)
}



  