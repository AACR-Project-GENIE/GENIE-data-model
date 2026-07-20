#' @title Turn response codes (one letter) into the RECIST-style codes everyone is used to.
#' @param resp_strings
#' @export
best_response_recist_style <- function(
  resp_strings
) {
  # r = response, s = stable, etc.
  chk_format <- all(
    str_detect(resp_strings, "^[rsmpn]*$") | is.na(resp_strings)
  )
  if (!chk_format) {
    cli_abort(
      "This function only takes strings composed of one letter codes, r = response, s = stable, etc."
    )
  }

  bor = case_when(
    resp_strings == "" | is.na(resp_strings) ~ "no data",
    # we basically have no ability to distinguish between PR and CR now.
    str_detect(resp_strings, "R") ~ "CR",
    str_detect(resp_strings, "r") ~ "PR",
    str_detect(resp_strings, "s") ~ "SD",
    # This is a departure from the mcelvey paper, which weirdly seems to have rated mixed response lower than progressive disease.
    str_detect(resp_strings, "m") ~ "MR",
    str_detect(resp_strings, "p") ~ "PD",
    str_detect(resp_strings, "n") ~ "NE",
    T ~ "error"
  )

  if (any(bor == "error")) {
    cli_abort("Processing error on best_response_one_letter_codes()")
  }

  return(bor)
}
