#' Add missing (undefined) variables to the data dictionary.
#'
#' @param dat_dict data dictionary as read in with `dd_readr()`.
#' @param undefined_vars The list of variables to add (which presumably aren't
#'   in the data dictionary.
#' @param undefined_note The note to add for undefined variables.  Defaults to
#'   "Not defined - this row was added to the data dictionary as a processing
#'   step".
#'
#' @returns `dat_dict` with the undefined variables added in.
#' @export
#'
#' @examples
add_undefined_vars <- function(
  dat_dict,
  undefined_vars = NULL,
  undefined_note = NULL
) {
  if (is.null(undefined_vars)) {
    undefined_vars <- c(
      'redcap_data_access_group', # sage added.
      'redcap_repeat_instrument',
      'redcap_repeat_instance',
      'curation_initiation_eligibility_complete',
      'patient_characteristics_complete',
      'cancer_diagnosis_complete',
      'ca_directed_drugs_complete',
      'ca_directed_radtx_complete',
      'prissmm_imaging_complete',
      'prissmm_pathology_complete',
      'prissmm_med_onc_assessment_complete',
      'prissmm_tumor_marker_complete',
      'cancer_panel_test_complete',
      'curation_completion_complete',
      'quality_assurance_complete'
    )
  }

  undefined_note <- undefined_note %||%
    "Not defined - this row was added to the data dictionary as a processing step"

  undefined_vars_df <- tibble::tibble(
    field_name = undefined_vars
  )

  undefined_vars_df <- undefined_vars_df %>%
    dplyr::mutate(
      field_note = undefined_note,
      field_type = dplyr::case_when(
        # the completeness checks use 0/1/2 encoding so we need
        #   to treat them differently than yesno columns.
        stringr::str_detect(field_name, 'complete$') ~ 'complete_check',
        T ~ 'text'
      )
    )

  dat_dict <- dplyr::bind_rows(
    dat_dict,
    undefined_vars_df
  )

  dat_dict
}
