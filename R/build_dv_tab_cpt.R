#' Build derived variable table for cancer panel test form
#'
#' @param tab Cancer panel test form from the raw redcap.
#' @param dat_dict_sub The subset of the data dictionary with variables for
#'   this instrument.
#'
#' @returns A dataframe similar to cpt with additional derivations.
#' @export
#'
#' @examples
#' # build_dv_tab_cpt(cpt, dat_dict_sub)
build_dv_tab_cpt <- function(
  tab,
  dat_dict_sub
) {
  tab <- column_exclusion_helper_derived(tab)

  cpt_ca_cols <- dat_dict_sub |>
    dplyr::filter(stringr::str_detect(field_name, "^cpt_ca___")) |>
    dplyr::pull(field_name)

  rtn <- common_data_derivation_operations(
    dat = tab,
    dict = dat_dict_sub,
    exclude_cols = cpt_ca_cols
  )

  rtn <- drugs_ca_rearrangement(rtn, prefix = "^cpt_ca")

  return(rtn)
}
