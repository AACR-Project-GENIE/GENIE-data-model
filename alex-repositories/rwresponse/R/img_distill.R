#' @title Distill the imaging data
#'
#' @description
#' Take the base BPC data and turn it into a format that can be more easily merged with med onc, other columns.
#' @param dat_img PRISSMM imaging dataset
#' @param return_minimal If true, returns fewer columns.
#' @return A tibble with the same number of rows as dat_img.
#' @export
img_distill <- function(
  dat_img,
  return_minimal = F
) {
  rtn <- dat_img |>
    dplyr::select(
      cohort,
      record_id,
      scan_number,
      image_scan_int,
      image_ca,
      image_overall,
      image_scan_type,
      scan_sites
    ) |>
    # Arrange by record_id and scan date
    dplyr::arrange(record_id, image_scan_int)

  rtn <- rtn |>
    dplyr::mutate(
      # indeterminate is basically not evaluable.
      evaluated = case_when(
        str_detect(image_ca, "no evidence of cancer") ~ T,
        str_detect(image_ca, "evidence of cancer") &
          # the other option is not stated / indeterminate.
          str_detect(image_overall, "Progressing|Improving|Stable|Mixed") ~
          T,
        T ~ F
      ),
      cancer = case_when(
        str_detect(image_ca, "no evidence of cancer") ~ F,
        str_detect(image_ca, "there is evidence of cancer") ~ T,
        T ~ NA
      )
    ) |>
    status_processor(dat = _, col_name = "image_overall")

  # generalizing the time variable so this can be stacked with med onc.
  rtn %<>%
    dplyr::mutate(
      event_type = "img"
    ) %>%
    dplyr::rename(dob_event_days = image_scan_int) %>%
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

  return(rtn)
}
