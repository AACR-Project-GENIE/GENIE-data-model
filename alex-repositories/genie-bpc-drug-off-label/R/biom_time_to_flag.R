biom_time_to_flag <- function(
    tt_ref, # drug start time, for the moment
    tt_test,
    tt_pos
) {
  # half day tolerance around all
  rtn <- case_when(
    is.na(tt_ref) | is.na(tt_test) ~ NA_real_, # should never happen with drugs
    tt_ref < tt_test ~ NA_real_, # not tested at ref time
    is.na(tt_pos) ~ 0,
    tt_ref >= tt_pos ~ 1,
    tt_ref < tt_pos ~ 0,
    T ~ -Inf # should never happen, to check.
  )
  
  chk_biom <- !(any(is.infinite(rtn)))
  if (!chk_biom) {
    cli_abort("Error in processing biomarker flags - infinite value.")
  }
  
  return(rtn)
  
}