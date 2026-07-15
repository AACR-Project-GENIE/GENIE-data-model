#' @description Filters the index cancer dataset to only cases which are before any
#'   non-index cancer diagnoses (based on the ca_seq variable, since perplexingly
#'   the time variable is missing even when this is filled out.
#'   
filter_pre_nonindex <- function(dat_ind, dat_non_ind) {
  dat_first_non_ind <- dat_non_ind %>%
    group_by(record_id) %>%
    summarize(
      first_non_ind_ca_seq = min(ca_seq, na.rm = T), 
      .groups = 'drop'
    )
  
  rtn <- left_join(
    dat_ind, 
    dat_first_non_ind,
    by = "record_id"
  ) %>%
    mutate(
      first_non_ind_ca_seq = if_else(
        is.na(first_non_ind_ca_seq), # i.e. if no non-index cases exist.
        Inf, 
        first_non_ind_ca_seq
      )
    )
  
  rtn %<>% 
    filter(ca_seq < first_non_ind_ca_seq) %>%
    select(-first_non_ind_ca_seq)
  
  return(rtn)
}