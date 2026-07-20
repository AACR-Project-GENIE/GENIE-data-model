#' @title Add drug start range
#' @description Adds two variables for the maximum and minimum possible drug start
#'    date each drug use could have.
#' @param dat_pt A PRISSMM patient data dataset with birth date range added (i.e. add_birth_date_range() has been applied already).
#' @param dat_drug A drug-keyed dataset, which is derived from the PRISSMM regimen dataset.
#' @return A drug dataset with two columns added to the input.
add_drug_start_range <- function(
    dat_pt,
    dat_drug
) {
  if (!(all(c('birth_date_min', 'birth_date_max') %in% names(dat_pt)))) {
    cli::cli_abort("'birth_date_min' and 'birth_date_max' are required in dat_pt.")
  }
  
  if (!('drugs_startdt_int' %in% names(dat_drug))) {
    cli::cli_abort("'drugs_startdt_int' is required in dat_drug.")
  }
  
  dat_pt_lim <- dat_pt %>%
    select(record_id, birth_date_min, birth_date_max)
  
  rtn <- left_join(
    dat_drug,
    dat_pt_lim,
    by = 'record_id'
  )
  
  rtn %<>%
    mutate(
      drug_start_date_min = birth_date_min + days(drugs_startdt_int),
      drug_start_date_max = birth_date_max + days(drugs_startdt_int)
    ) %>%
    select(-c(birth_date_min, birth_date_max))
  
  # Sanity checks
  if (any(rtn$drug_start_date_min >= rtn$drug_start_date_max, na.rm = T)) {
    cli_abort("Error: Drug start min is not less than drug start max.")
  }
  
  return(rtn)
}
