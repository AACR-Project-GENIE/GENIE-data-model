#' Build derived variable table for pathology form
#'
#' @param tab Pathology form from the raw redcap.
#' @param dat_dict_sub The subset of the data dictionary with variables for
#'   this instrument.
#'
#' @returns A dataframe similar to path with additional derivations.
#' @export
#'
#' @examples
#' # build_dv_tab_path(path, dat_dict_sub)
build_dv_tab_path <- function(
  tab,
  dat_dict_sub
) {
  tab <- column_exclusion_helper_derived(tab)

  rtn <- common_data_derivation_operations(
    dat = tab,
    dict = dat_dict_sub
  )

  rtn <- derive_path_proc_number(rtn)

  rtn <- derive_path_rep_number(rtn)

  rtn <- rtn |>
    dplyr::mutate(
      path_proc_type = map_path_proc_type(path_proc_type),
      path_proc = map_path_proc(path_proc),
      path_proc_margins = map_path_proc_margins(path_proc_margins)
    )

  path_site_cols <- grep("^path_site[0-9]+$", names(rtn), value = TRUE)
  rtn <- rtn |>
    dplyr::mutate(dplyr::across(dplyr::all_of(path_site_cols), map_path_site))

  path_insitu_cols <- grep("^path_insitu[0-9]+$", names(rtn), value = TRUE)
  rtn <- rtn |>
    dplyr::mutate(dplyr::across(dplyr::all_of(path_insitu_cols), map_path_insitu))

  rtn <- derive_path_insitu_any(rtn)

  return(rtn)
}
