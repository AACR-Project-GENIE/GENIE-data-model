#' @title Add one letter codes response codes
#'
#' @description Takes a distilled dataset and adds a column with single letter versions of the codes.  This is later useful for regex.
#'
#' @param dat_distill A tibble as output by `img_distill()` or `med_onc_distill()`
#' @export

add_response_letter_codes <- function(
  dat_distill
) {
  rtn <- dat_distill %>%
    mutate(
      # Short codes use RECIST-like naming, pared down to a single letter.
      resp_code_cs = case_when(
        # for longitudinal imputation it will make sense to use evaluated, but for the simple case when we're not doing that we can use assume NA is not evaluable.
        is.na(status_change) ~ "n", # for not evaluable
        # complete response is not possible to detect in prissmm with the cross sectional codes.
        status_change %in% "improving" ~ "r", # for partial response.
        status_change %in% "worsening" ~ "p", # for progressive disease.
        status_change %in% "mixed" ~ "m",
        status_change %in% "stable" ~ "s",
        T ~ "e"
      )
    )

  if (any(rtn$resp_code_cs %in% "e")) {
    cli_abort("Unknown response code case encountered")
  }
  return(rtn)
}
