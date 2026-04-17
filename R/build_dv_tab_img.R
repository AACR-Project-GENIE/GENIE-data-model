#' Build derived variable table for imaging form
#'
#' @param tab Imaging form from the raw redcap.
#' @param dat_dict_sub The subset of the data dictionary with variables for
#'   this instrument.
#'
#' @returns A dataframe similar to img with additional derivations.
#' @export
#'
#' @examples
#' # build_dv_tab_img(img, dat_dict_sub)
build_dv_tab_img <- function(
  tab,
  dat_dict_sub
) {
  tab <- column_exclusion_helper_derived(tab)

  rtn <- common_data_derivation_operations(
    dat = tab,
    dict = dat_dict_sub
  )

  rtn <- add_institution(rtn)

  rtn <- derive_scan_number(rtn)

  rtn <- repair_scan_sites(rtn)

  rtn <- rtn |>
    dplyr::mutate(
      image_inst_perf = map_image_inst(image_inst_perf),
      image_inst_inter = map_image_inst(image_inst_inter),
      image_scan_type = map_image_scan_type(image_scan_type)
    )

  return(rtn)
}
