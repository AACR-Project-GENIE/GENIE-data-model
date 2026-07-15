

add_regimen_lastadm_vars <- function(dat_reg, move_after_reg_end = T) {
  req_cols <- c(
    paste0('dx_drug_end_or_lastadm_int_', 1:5)
  )
  mis_cols <- req_cols[!(req_cols %in% colnames(dat_reg))]
  if (length(mis_cols) > 0) {
    cli::cli_abort("Required columns are missing: {paste(mis_cols, collapse = ', ')}")
  }
  
  dat_reg %<>%
    mutate(
      dx_reg_end_or_lastadm_any_int = pmin(
        dx_drug_end_or_lastadm_int_1,
        dx_drug_end_or_lastadm_int_2,
        dx_drug_end_or_lastadm_int_3,
        dx_drug_end_or_lastadm_int_4,
        dx_drug_end_or_lastadm_int_5,
        na.rm = T
      ),
      dx_reg_end_or_lastadm_all_int = pmax(
        dx_drug_end_or_lastadm_int_1,
        dx_drug_end_or_lastadm_int_2,
        dx_drug_end_or_lastadm_int_3,
        dx_drug_end_or_lastadm_int_4,
        dx_drug_end_or_lastadm_int_5,
        na.rm = T
      )
    )
  
  if (move_after_reg_end) {
    dat_reg %>%
      relocate(
        dx_reg_end_or_lastadm_any_int,
        dx_reg_end_or_lastadm_all_int,
        .after = dx_reg_end_all_int
      )
  }
  
  return(dat_reg)
  
}