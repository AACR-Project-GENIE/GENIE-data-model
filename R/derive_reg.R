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

  # Match legacy: drugs_enddt_int_N is only populated when the regimen was
  # discontinued (drugs_dc_ynu == "Yes") and the corresponding drug slot is
  # not an investigational drug. This cascades to dx_drug_end_int_N via the
  # subtraction in derive_dx_drug_int().
  rtn <- rtn |>
    dplyr::mutate(
      drugs_enddt_int_1 = dplyr::case_when(
        drugs_dc_ynu %in% "Yes" & !(drugs_drug_1 %in% "Investigational Drug") ~
          drugs_enddt_int_1,
        .default = NA_real_
      ),
      drugs_enddt_int_2 = dplyr::case_when(
        drugs_dc_ynu %in% "Yes" & !(drugs_drug_2 %in% "Investigational Drug") ~
          drugs_enddt_int_2,
        .default = NA_real_
      ),
      drugs_enddt_int_3 = dplyr::case_when(
        drugs_dc_ynu %in% "Yes" & !(drugs_drug_3 %in% "Investigational Drug") ~
          drugs_enddt_int_3,
        .default = NA_real_
      ),
      drugs_enddt_int_4 = dplyr::case_when(
        drugs_dc_ynu %in% "Yes" & !(drugs_drug_4 %in% "Investigational Drug") ~
          drugs_enddt_int_4,
        .default = NA_real_
      ),
      drugs_enddt_int_5 = dplyr::case_when(
        drugs_dc_ynu %in% "Yes" & !(drugs_drug_5 %in% "Investigational Drug") ~
          drugs_enddt_int_5,
        .default = NA_real_
      )
    )

  rtn <- rtn |>
    dplyr::mutate(
      drugs_drug_end_or_lastadm_int_1 = derive_drug_end_or_lastadm_int(
        drugs_enddt_int_1,
        drugs_lastdt_int_1,
        drugs_dc_ynu
      ),
      drugs_drug_end_or_lastadm_int_2 = derive_drug_end_or_lastadm_int(
        drugs_enddt_int_2,
        drugs_lastdt_int_2,
        drugs_dc_ynu
      ),
      drugs_drug_end_or_lastadm_int_3 = derive_drug_end_or_lastadm_int(
        drugs_enddt_int_3,
        drugs_lastdt_int_3,
        drugs_dc_ynu
      ),
      drugs_drug_end_or_lastadm_int_4 = derive_drug_end_or_lastadm_int(
        drugs_enddt_int_4,
        drugs_lastdt_int_4,
        drugs_dc_ynu
      ),
      drugs_drug_end_or_lastadm_int_5 = derive_drug_end_or_lastadm_int(
        drugs_enddt_int_5,
        drugs_lastdt_int_5,
        drugs_dc_ynu
      )
    )

  rtn <- derive_regimen_number(rtn)

  rtn <- derive_regimen_drugs(rtn)

  rtn <- derive_reg_start_int(rtn)

  rtn <- derive_reg_end_any_int(rtn)

  # Notes:
  # probably need remove _mask columns too - but not yet.
  return(rtn)
}
