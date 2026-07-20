#' @title Distill the medical oncologist report data
#'
#' @description
#' Take the base BPC data and turn it into a format that can be more easily merged with imaging, other columns.
#' @param dat_med_onc PRISSMM medical oncology dataset.
#' @param return_minimal If true, returns fewer columns.
#' @return A tibble with the same number of rows as dat_img.
#' @export
med_onc_distill <- function(
  dat_med_onc,
  return_minimal = F
) {
  rtn <- dat_med_onc |>
    dplyr::select(
      cohort,
      record_id,
      md_visit_number,
      md_onc_visit_int,
      md_ca,
      md_ca_status
    ) |>
    # Arrange by record_id and scan date
    dplyr::arrange(record_id, md_onc_visit_int)

  rtn <- rtn |>
    dplyr::mutate(
      evaluated = case_when(
        str_detect(md_ca, "no evidence of cancer") ~ T,
        str_detect(md_ca, "evidence of cancer") &
          # the other option is not stated / indeterminate.
          str_detect(md_ca_status, "Progressing|Improving|Stable|Mixed") ~
          T,
        T ~ F
      ),
      cancer = case_when(
        str_detect(md_ca, "no evidence of cancer") ~ F,
        str_detect(md_ca, "there is evidence of cancer") ~ T,
        T ~ NA
      )
    ) |>
    status_processor(dat = _, col_name = 'md_ca_status')

  rtn %<>%
    dplyr::mutate(
      event_type = 'med_onc'
    ) %>%
    dplyr::rename(dob_event_days = md_onc_visit_int) %>%
    dplyr::relocate(event_type, .after = dob_event_days)

  rtn %<>%
    add_response_letter_codes(.)

  if (return_minimal) {
    # drops the original columns.
    rtn <- rtn |>
      dplyr::select(
        cohort,
        record_id,
        dob_event_days,
        event_type,
        evaluated,
        cancer,
        status_change,
        resp_code_cs
      )
  }

  rtn
}
