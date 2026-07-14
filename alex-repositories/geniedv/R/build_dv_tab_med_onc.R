#' Build derived variable table for medical oncologist form
#'
#' @param tab Medical oncologist form from the raw redcap.
#' @param dat_dict_sub The subset of the data dictionary with variables for
#'   this instrument.
#'
#' @returns A dataframe similar to med_onc with additional derivations.
#' @export
#'
#' @examples
#' # build_dv_tab_med_onc(med_onc, dat_dict_sub)
build_dv_tab_med_onc <- function(
  tab,
  dat_dict_sub
) {
  tab <- column_exclusion_helper_derived(tab)

  rtn <- common_data_derivation_operations(
    dat = tab,
    dict = dat_dict_sub
  )

  return(rtn)
}
