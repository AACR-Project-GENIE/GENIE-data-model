

summarize_records_by_index <- function(dat_ind, dat_non_ind) {
  dat_ind %<>% 
    select(cohort, record_id, ca_seq) %>%
    group_by(cohort, record_id) %>%
    summarize(
      has_index_cancer = T,
      multiple_index_cancers = if_else(n() >= 2, T, F, F),
      ca_seq_of_first_index = min(ca_seq, na.rm = T),
      .groups = "drop"
    ) %>%
    ungroup(.)
  
  dat_non_ind %<>% 
    select(cohort, record_id, ca_seq) %>%
    group_by(cohort, record_id) %>%
    summarize(
      has_non_index_cancer = T,
      ca_seq_of_first_non_index = min(ca_seq, na.rm = T),
      .groups = "drop"
    ) %>%
    ungroup(.)
  
  # Sanity checks on the data:
  chk_1 <- setdiff(dat_non_ind$record_id, dat_ind$record_id)
  if (length(chk_1) > 0) {
    cli::cli_abort(
      glue(
        "There are non-index record IDs not found in the record ID data: {chk_1}"
      )
    )
  }
  
  chk_2 <- length(unique(c(dat_non_ind$cohort, dat_ind$cohort)))
  if (length(chk_2) > 1) {
    cli::cli_abort(
      glue(
        "There is more than one cohort in this data: {chk_2}"
      )
    )
  }
  
  rtn <- left_join(
    dat_ind,
    select(dat_non_ind, -cohort),
    by = "record_id"
  )
  
  rtn %<>% select(cohort, record_id, everything())
  
  return(rtn)
  
}