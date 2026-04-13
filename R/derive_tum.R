#' Derive tumor marker form
#'
#' @param tab Tumor marker form from the raw redcap.
#' @param dat_dict_sub The subset of the data dictionary with variables for
#'   this instrument.
#'
#' @returns A dataframe similar to tum with additional derivations.
#' @export
#'
#' @examples
#' # derive_tum(tum, dat_dict_sub)
derive_tum <- function(
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
