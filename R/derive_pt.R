#' Derive the patient data table.
#'
#' @param tab Curated pt table.
#' @param dat_dict_sub Data dictionary with columns relevant to pt table.
#'
#' @returns Derived patient table.
#' @export
#'
#' @examples
derive_pt <- function(
  tab,
  dat_dict_sub
) {
  tab <- column_exclusion_helper_derived(tab)

  rtn <- common_data_derivation_operations(
    dat = tab,
    dict = dat_dict_sub
  )

  rtn <- add_institution(rtn)

  # map all the dropdowns that aren't in the data dictionary:
  rtn <- rtn |>
    dplyr::mutate(
      naaccr_ethnicity_code = map_naaccr_eth_code(naaccr_ethnicity_code),
      naaccr_race_code_primary = map_naaccr_race(naaccr_race_code_primary),
      naaccr_race_code_secondary = map_naaccr_race(naaccr_race_code_secondary),
      naaccr_race_code_tertiary = map_naaccr_race(naaccr_race_code_tertiary),
      naaccr_sex_code = map_naaccr_sex_code(naaccr_sex_code),
      hybrid_death_source = map_hybrid_death_source(hybrid_death_source)
    )

  return(rtn)
}
