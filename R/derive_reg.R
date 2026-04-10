#' Derive cancer-directed regimen form
#'
#' @param tab Cancer-directed drugs (regimen) form from the raw redcap.
#' @param dat_dict_sub The subset of the data dictionary with variables for this instrument.
#'
#' @returns A dataframe similar to regimen with additional derivations.
#' @export
#'
#' @examples # Going to need synthetic data probably...
derive_reg <- function(
  tab,
  dat_dict_sub
) {
  tab <- column_exclusion_helper_derived(tab)

  rtn <- common_data_derivation_operations(
    dat = tab,
    dict = dat_dict_sub
  )

  rtn <- drugs_ca_rearrangement(rtn, prefix = '^drugs_ca')

  rtn <- rtn |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::matches('^drugs_(start|end|last)dt_int_'),
        .fns = as.numeric
      )
    )

  rtn <- rtn |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::matches('^drugs_drug_\\d+$'),
        .fns = map_drug_name
      )
    )

  rtn <- derive_regimen_number(rtn)

  # Notes:
  # probably need remove _mask columns too - but not yet.
  return(rtn)
}
