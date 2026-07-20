# This function WILL remove features that are never altered.
split_gene_features <- function(
    dat_alt,
    vec_type,
    vec_function
) {
  if (length(intersect(vec_type, vec_function) > 0)) {
    cli::cli_abort("vec_type and vec_function cannot have any overlap")
  }
  
  dat_cov <- dat_alt %<>%
    mutate(
      split_type = if_else(hugo %in% vec_type, 1, 0),
      split_function = if_else(hugo %in% vec_function, 1, 0)
    ) %>%
    mutate(
      type_suffix = case_when(
        alt_type %in% "Mutation" ~ "_mut",
        alt_type %in% "CNA" ~ "_cna",
        alt_type %in% "Fusion" ~ "_fus",
      ),
      function_suffix = case_when(
        mut_eff_simple %in% "Loss" ~ "_lof",
        mut_eff_simple %in% "Gain" ~ "_gof",
        T ~ "_remove" # We ignore the very few switch or other cases for simplicity.
      )
    )
  
  dat_cov %<>%
    mutate(
      term = case_when(
        split_type %in% 0 & split_function %in% 0 ~ paste0(hugo, "_gene"),
        split_type %in% 1 ~ paste0(hugo, type_suffix),
        split_function %in% 1 ~ paste0(hugo, function_suffix)
      )
    )
  
  dat_cov %<>%
    filter(!str_detect(term, "_remove"))
  
  dat_cov %<>%
    group_by(sample_id, term) %>%
    # we don't care if there is more than one alteration for a given class:
    slice(1) %>%
    summarize(altered = 1, .groups = "drop")
  
  rtn <- dat_cov %>%
    arrange(term) %>%
    pivot_wider(
      names_from = term, 
      values_from = altered,
      values_fill = 0
    ) %>%
    arrange(sample_id)
  
  return(rtn)
}