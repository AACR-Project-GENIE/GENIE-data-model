#' Find variables with sub names in a list of columns
#'
#' @param dat_cols A vector of column names
#'
#' @returns A tibble with columns stub (the root), min and max (the highest and lowest appended number after the stub).
#' @export
#'
#' @examples
find_stubs_in_data <- function(dat_cols) {
  stub_vars <- tibble::tibble(
    stub = dat_cols %>%
      .[str_detect(., '\\_\\_')]
  ) %>%
    tidyr::separate(stub, into = c('stub', 'num'), sep = '\\_\\_\\_') %>%
    dplyr::mutate(num = as.numeric(num)) %>%
    dplyr::group_by(stub) %>%
    dplyr::summarize(
      min = min(num),
      max = max(num)
    )

  return(stub_vars)
}
