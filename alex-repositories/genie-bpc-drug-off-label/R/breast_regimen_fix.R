#' @title Breast cancer BPC regimen data fix
#' 
#' @description Fix for the unexplained deviances from the norm which happen
#'   with the breast cancer dataset.  Can be applied to any regimen data.
#' @param dat A PRISSMM (GENIE BPC) regimen dataset.
#' @param remove_nonstandard Remove columns after using them to calculate standard
#'   variables.
breast_regimen_fix <- function(dat, remove_nonstandard = T) {
  # Assuming that if they don't have _1, they don't have _2, _3, etc.
  if (!('dx_drug_end_int_1' %in% colnames(dat))) {
    dat %<>%
      mutate(
        dx_drug_end_int_1 = drug_start_end_int_1 + dx_drug_start_int_1,
        dx_drug_end_int_2 = drug_start_end_int_2 + dx_drug_start_int_2,
        dx_drug_end_int_3 = drug_start_end_int_3 + dx_drug_start_int_3,
        dx_drug_end_int_4 = drug_start_end_int_4 + dx_drug_start_int_4,
        dx_drug_end_int_5 = drug_start_end_int_5 + dx_drug_start_int_5
      )
  }
  
  if (!('dx_drug_end_or_lastadm_int_1' %in% colnames(dat))) {
    dat %<>%
      mutate(
        dx_drug_end_or_lastadm_int_1 = drug_start_end_or_lastadm_int_1 +
          dx_drug_start_int_1,
        dx_drug_end_or_lastadm_int_2 = drug_start_end_or_lastadm_int_2 +
          dx_drug_start_int_2,
        dx_drug_end_or_lastadm_int_3 = drug_start_end_or_lastadm_int_3 +
          dx_drug_start_int_3,
        dx_drug_end_or_lastadm_int_4 = drug_start_end_or_lastadm_int_4 +
          dx_drug_start_int_4,
        dx_drug_end_or_lastadm_int_5 = drug_start_end_or_lastadm_int_5 +
          dx_drug_start_int_5,
      )
  }
  
  if (!('dx_reg_end_any_int' %in% colnames(dat))) {
    dat %<>%
      mutate(
        dx_reg_end_any_int = reg_start_end_any_int + dx_reg_start_int
      )
  }
  
  if (!('dx_reg_end_all_int' %in% colnames(dat))) {
    dat %<>%
      mutate(
        dx_reg_end_all_int = reg_start_end_all_int + dx_reg_start_int
      )
  }
  
  nonstandard_vars <- c(
    paste0('drug_start_end_or_lastadm_int_', 1:5),
    paste0('drug_start_end_int_', 1:5),
    'reg_start_end_any_int',
    'reg_start_end_all_int'
  )
  if (remove_nonstandard) {
    dat %<>%
      select(
        -any_of(
          nonstandard_vars
        )
      )
  }
  
  return(dat)
}
