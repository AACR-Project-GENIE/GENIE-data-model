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

  rtn <- rtn |>
    dplyr::mutate(
      drugs_dc_ynu = map_drugs_dc_ynu(drugs_dc_ynu)
    )

  rtn <- rtn |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::matches("^drugs_enddt_int_\\d+$"),
        .fns = \(x) derive_drug_end_or_lastadm_int(
          drugs_enddt_int = x,
          drugs_lastdt_int = .data[[stringr::str_replace(
            dplyr::cur_column(),
            "^drugs_enddt_int_",
            "drugs_lastdt_int_"
          )]],
          drugs_dc_ynu = drugs_dc_ynu
        ),
        .names = "{stringr::str_replace(.col, '^drugs_enddt_int_', 'drugs_drug_end_or_lastadm_int_')}"
      )
    )

  rtn <- derive_regimen_number(rtn)

  rtn <- derive_regimen_drugs(rtn)

  # Notes:
  # probably need remove _mask columns too - but not yet.
  return(rtn)
}
