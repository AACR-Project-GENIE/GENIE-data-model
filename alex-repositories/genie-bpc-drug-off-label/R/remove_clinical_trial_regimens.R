
#' @title Remove clinical trial regimens
#' @param dat_reg A dataset similar in structure to PRISSMM's regimen data.

remove_clinical_trial_regimens <- function(
    dat_reg,
    verbose = T
) {
  if (!('drugs_ct_yn' %in% names(dat_reg))) {
    if (verbose) {
      cli::cli_alert_info("drugs_ct_yn variable not present - nothing done.")
    }  
  } else {
    before_rows <- nrow(dat_reg)
    
    dat_reg <- dat_reg |>
      filter(!(drugs_ct_yn %in% "Yes"))
    if (verbose) {
      rows_rem <- before_rows - nrow(dat_reg) 
      cli::cli_alert_info(
        "Removed {rows_rem} rows from data for being part of a clinical trial."
      )
    }
  }
  return(dat_reg)
}