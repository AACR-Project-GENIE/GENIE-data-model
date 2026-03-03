#' Read and clean up a PRISSMM REDCap data dictionary
#'
#' @param dat_dict_path path to the data dictionary csv
#'
#' @returns a data dictionary with renamed columns and other small improvements
#' @export
#'
#' @examples
dd_readr <- function(dat_dict_path) {
  dat_dict <- readr::read_csv(
    dat_dict_path
  ) %>%
    # jeez these headers suck.
    dplyr::rename(
      field_name = `Variable / Field Name`,
      form = `Form Name`,
      field_type = `Field Type`,
      # this isn't actually limited to valid values, there's calculations in here for some odd reason:
      choices_calc = `Choices, Calculations, OR Slider Labels`,
      required = `Required Field?`
    ) %>%
    dplyr::rename_all(~ stringr::str_replace_all(tolower(.x), " ", "_")) %>%
    dplyr::rename_all(~ stringr::str_replace_all(tolower(.x), "\\?", "")) %>%
    dplyr::rename_all(
      ~ stringr::str_replace_all(tolower(.x), "_\\(.*\\)", "")
    ) %>%
    dplyr::mutate(
      required = dplyr::if_else(required %in% "y", T, F) # fixing y/NA coding.
    )

  dat_dict <- dat_dict %>%
    dplyr::mutate(
      choices_calc = dplyr::case_when(
        field_type %in% 'checkbox' ~ paste0('0, 0 |', choices_calc)
      )
    )

  return(dat_dict)
}
