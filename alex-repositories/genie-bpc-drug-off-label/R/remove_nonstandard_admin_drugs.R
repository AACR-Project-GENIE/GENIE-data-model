remove_nonstandard_admin_drugs <- function(
    dat_reg,
    verbose = T
) {
  if (!('drugs_admin' %in% names(dat_reg))) {
    if (verbose) {
      cli::cli_alert_info("drugs_admin variable not present - nothing done.")
    }  
  } else {
    before_rows <- nrow(dat_reg)
    # In PRISSMM this variable is missing for drugs which are administered
    # in a standard way (oral/IV/infusion).  Therefore, if we want only
    # standard administration drugs we want rows missing in this var.
    dat_reg <- dat_reg |>
      filter(is.na(drugs_admin))
    if (verbose) {
      rows_rem <- before_rows - nrow(dat_reg) 
      cli::cli_alert_info(
        "Removed {rows_rem} rows from data for 'nonstandard' administration routes"
      )
    }
  }
  return(dat_reg)
}