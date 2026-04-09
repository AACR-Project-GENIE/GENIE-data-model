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

  rtn <- rtn |>
    dplyr::mutate(
      naaccr_ethnicity_code = map_naaccr_eth_code(naaccr_ethnicity_code)
    )

  return(rtn)
}
