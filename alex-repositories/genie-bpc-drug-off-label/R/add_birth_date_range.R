#' @title Add birth date range
#' @description Adds two variables for the maximum and minimum possible birth date
#'    each person could have
#' @param dat_pt A PRISSMM patient data dataset.
#' @return A PRISSMM patient level dataset with two columns added to the input.
add_birth_date_range <- function(
    dat_pt
) {
  if (!('birth_year' %in% names(dat_pt))) {
    cli::cli_abort("'birth_year' is not in the data provided.")
  }
  
  rtn <- dat_pt %>%
    mutate(
      birth_date_min = ymd(paste0(birth_year,'-01-01')),
      birth_date_max = ymd(paste0(birth_year,'-12-31'))
    ) %>%
    relocate(
      birth_date_min, birth_date_max,
      .after = birth_year
    )
  
  return(rtn)
  
}