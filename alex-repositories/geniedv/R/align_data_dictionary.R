#' Align the data dictionary with the data
#'
#' @param path_to_cur_dat Path to the data we're aligning to.
#' @param path_to_dat_dict Path to the data dictionary to be aligned.
#' @param undefined_vars Passed on to `add_undefined_vars()`, default of NULL uses a built-in list.
#' @param required_override Variables to manually set to be required.  Defaults to  'record_id', 'redcap_repeat_instrument' and 'redcap_repeat_instance'.
#' @param dttm_cols Columns to mark as datetime, if NULL uses default in `dd_assign_coltypes()`.
#' @param date_cols Columns to mark as date, if NULL uses default in `dd_assign_coltypes()`.
#' @param num_cols Columns to mark as numeric if NULL uses default in `dd_assign_coltypes()`.
#'
#' @returns Aligned data dictionary (a tibble).
#' @export
#'
#' @examples
align_data_dictionary <- function(
  path_to_cur_dat,
  path_to_dat_dict,
  undefined_vars = NULL,
  required_override = NULL,
  dttm_cols = NULL,
  date_cols = NULL,
  num_cols = NULL
) {
  required_override <- required_override %||%
    c('record_id', 'redcap_repeat_instrument', 'redcap_repeat_instance')

  dat_dict <- dd_readr(path_to_dat_dict)

  dat_cols <- path_to_cur_dat %>%
    readr::read_csv(., n_max = 1, show_col_types = FALSE) %>%
    trim_nameless_cols(.) %>%
    colnames(.)

  # Data dictionary lists variables without the triple underscore + number extension.  This expands it out to include those.
  exp_stubs <- expand_stub_variables(dat_dict, dat_cols = dat_cols)
  stub_names <- find_stubs_in_data(dat_cols = dat_cols)
  dat_dict <- dplyr::bind_rows(
    dat_dict,
    exp_stubs
  ) %>%
    dplyr::filter(!(field_name %in% stub_names$stub))

  dat_dict <- add_undefined_vars(dat_dict, undefined_vars = undefined_vars)

  dat_dict <- dat_dict %>%
    dplyr::mutate(
      required = dplyr::case_when(
        field_name %in% required_override ~ TRUE,
        T ~ required
      )
    )

  if (length(setdiff(dat_cols, dat_dict$field_name)) > 0) {
    stop("Unresolved data dictionary errors - please fix.")
  }

  dat_dict <- dd_assign_coltypes(
    dat_dict,
    dttm_cols = dttm_cols,
    date_cols = date_cols,
    num_cols = num_cols
  )

  dat_dict <- dat_dict %>%
    dplyr::mutate(
      # the choices_calc field is missing some important stuff we'll want to check.
      valid_val_str = dplyr::case_when(
        field_type %in% c('checkbox', 'dropdown', 'radio') ~ choices_calc,
        field_type %in% 'yesno' ~ '0, No|1, Yes',
        field_type %in% 'complete_check' ~ '1, No|2, Yes'
      )
    )

  dat_dict <- dat_dict %>%
    split_valid_values(.)

  dat_dict
}
