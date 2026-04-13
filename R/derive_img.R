#' Derive imaging form
#'
#' @param tab Imaging form from the raw redcap.
#' @param dat_dict_sub The subset of the data dictionary with variables for
#'   this instrument.
#'
#' @returns A dataframe similar to img with additional derivations.
#' @export
#'
#' @examples
#' # derive_img(img, dat_dict_sub)
derive_img <- function(
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
