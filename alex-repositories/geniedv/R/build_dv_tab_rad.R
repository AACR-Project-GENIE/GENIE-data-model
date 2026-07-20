#' Build derived variable table for radiation therapy form
#'
#' @param tab Radiation therapy form from the raw redcap.
#' @param dat_dict_sub The subset of the data dictionary with variables for
#'   this instrument.
#'
#' @returns A dataframe similar to rad with additional derivations.
#' @export
#'
#' @examples
#' # build_dv_tab_rad(rad, dat_dict_sub)
build_dv_tab_rad <- function(
  tab,
  dat_dict_sub
) {
  tab <- column_exclusion_helper_derived(tab)

  rtn <- common_data_derivation_operations(
    dat = tab,
    dict = dat_dict_sub
  )

  rtn <- drugs_ca_rearrangement(rtn, prefix = "^rt_ca")

  return(rtn)
}
