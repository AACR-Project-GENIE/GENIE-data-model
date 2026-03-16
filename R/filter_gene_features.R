
#' @description Filters gene features for non-zero variance (obvious criterion)
#'   and so that a certain proportion of participants (0.5% by default,
#'   arbitrary) are positive.
#' @param dat a dataframe with one row per case ({record_id, ca_seq})
#' @param prop_filter the proportion of participants which must be positive for
#'   the feature to stay in the dataset.
#' @param additional_features By default, the function will apply this filtering
#'   to any column which ends in '_mut', '_cna' or '_fus', a convention for the
#'   types of genomic alterations we're considering.  If there are additional
#'   custom gene features to be considered, add their names here.
filter_gene_features <- function(dat, prop_filter = 0.005, additional_features = character(0)) {
  
  
  dat_gene <- dat %>%
    select(
      record_id, ca_seq,
      matches("_mut$"),
      matches("_cna$"),
      matches("_fus$"),
      all_of(additional_features),
    ) 
  
  # pull out the non-gene features, we can combine them again at the end.
  cols_not_gene <- setdiff(names(dat), names(dat_gene)) 
  dat_not_gene <- select(dat, record_id, ca_seq, all_of(cols_not_gene))
  
  
  dat_gene %<>%
    pivot_longer(
      cols = -c(record_id, ca_seq),
      names_to = "feature",
      values_to = "value"
    )
  
  # Require that there is some variance in the gene test results.
  dat_gene %<>%   
    group_by(feature) %>%
    mutate(
      f_var = var(value, na.rm = T),
      prop_pos = mean(value %in% 1)
    ) %>%
    ungroup(.) %>%
    # 0.5% of dmet cases is about 3 people
    filter(f_var > 0 & prop_pos > prop_filter)
  
  dat_gene %<>% select(-c(f_var, prop_pos))
  
  dat_gene <- pivot_wider(
    dat_gene,
    names_from = "feature", values_from = "value"
  )
  
  rtn <- left_join(
    dat_not_gene,
    dat_gene,
    by = c("record_id", "ca_seq")
  )
  
  return(rtn)
}