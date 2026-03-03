#' Expand stub variables
#'
#' @description Find stub variables (those following the stub___[num] format)
#' and expand them into rows to be added to the data dictionary.
#'
#'
#' @param dd A PRISSMM REDCap data dictionary loaded with `dd_readr()`.
#' @param dat_cols The data columns.
#'
#' @returns A tibble with a suitable format to be bound to `dd`.
#' @export
#'
#' @examples
expand_stub_variables <- function(dd, dat_cols) {
  stub_vars <- find_stubs_in_data(dat_cols)

  stub_vars <- stub_vars %>%
    dplyr::mutate(
      closest = find_closest_str(stub, dict = dd$field_name),
      exact_match = closest == stub
    )

  # Ok great, now we have to fix the data dictionary to accomodate all these:
  stub_merge <- stub_vars %>%
    dplyr::slice(rep(1:dplyr::n(), times = max)) %>%
    dplyr::group_by(stub) %>%
    dplyr::mutate(var = paste0(stub, '___', row_number())) %>%
    dplyr::ungroup(.) %>%
    dplyr::select(var, stub)

  # You only require the general variable type, not all the iterations of it.
  stub_merge <- stub_merge %>%
    dplyr::group_by(stub) %>%
    dplyr::arrange(var) %>%
    dplyr::mutate(.is_first = dplyr::row_number() %in% 1) %>%
    dplyr::ungroup(.)

  stub_merge <- dplyr::left_join(
    stub_merge,
    dd,
    by = c(stub = 'field_name')
  ) %>%
    dplyr::rename(field_name = var) %>%
    dplyr::mutate(required = required & .is_first) %>%
    dplyr::select(-c(.is_first, stub))

  stub_merge
}
