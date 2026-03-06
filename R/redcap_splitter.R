#' Split PRISSMM redcap bulk download into instruments.
#'
#' @param redcap_data_path The path to the redcap bulk download (all instruments).
#' @param dict The data dictionary for the redcap project.
#' @param keys_in_all_instr Keys to keep for each split instrument.  Default is "record_id", "redcap_repeat_instrument" and "redcap_repeat_instance".
#' @param forms_missing_in_redcap Forms that exist in the data dictionary but come in as NA for the redcap_repeat_instrument.  Reason unknown.  Default includes "curation_initiation_eligibility", "patient_characteristics", "curation_completion", "quality_assurance".
#'
#' @returns A tibble with list columns for the data dictionary subset and data instruments.
#' @export
#'
#' @examples
redcap_splitter <- function(
  redcap_data_path,
  dict,
  keys_in_all_instr = NULL,
  forms_missing_in_redcap = NULL
) {
  redcap_data <- readr::read_csv(
    redcap_data_path,
    col_types = cols(.default = col_character())
  )

  keys_in_all_instr <- keys_in_all_instr %||%
    c(
      'record_id',
      'redcap_repeat_instrument',
      'redcap_repeat_instance'
    )

  forms_missing_in_redcap <- forms_missing_in_redcap %||%
    c(
      'curation_initiation_eligibility',
      'patient_characteristics',
      'curation_completion',
      'quality_assurance'
    )

  dict <- dict |>
    dplyr::mutate(
      form_in_extract = dplyr::case_when(
        form %in% forms_missing_in_redcap ~ NA_character_,
        T ~ form
      )
    )

  nested_dd <- dict %>%
    tidyr::nest(.by = 'form_in_extract', .key = 'dat_dict_sub')

  nested_dd <- nested_dd |>
    dplyr::mutate(
      var_list = purrr::map(
        dat_dict_sub,
        .f = \(x) x$field_name
      )
    )

  nested_dd <- nested_dd %>%
    dplyr::mutate(
      tab = purrr::map2(
        .x = form_in_extract,
        .y = var_list,
        .f = \(f, v) {
          redcap_data %>%
            dplyr::select(
              tidyselect::all_of(keys_in_all_instr),
              tidyselect::any_of(v)
            ) %>%
            dplyr::filter(redcap_repeat_instrument %in% f)
        }
      )
    )

  nested_dd <- nested_dd %>%
    dplyr::mutate(
      form_in_extract = dplyr::case_when(
        is.na(form_in_extract) ~ "patient",
        T ~ form_in_extract
      )
    )

  nested_dd
}
