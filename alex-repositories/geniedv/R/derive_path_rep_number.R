#' Derive path_rep_number (report number within procedure)
#'
#' Numbers pathology reports within each `record_id` and `path_proc_number`,
#' ordered by `redcap_repeat_instance`. Requires [derive_path_proc_number()]
#' to have been called first.
#'
#' @param dat A pathology-level data frame with columns `record_id`,
#'   `path_proc_number`, and `redcap_repeat_instance`.
#'
#' @returns `dat` with `path_rep_number` added.
#' @export
#'
#' @examples
#' # derive_path_rep_number(path)
derive_path_rep_number <- function(dat) {
  if (!"path_proc_number" %in% names(dat)) {
    cli::cli_abort(
      "{.fn derive_path_proc_number} must be called before {.fn derive_path_rep_number}."
    )
  }

  dat |>
    dplyr::group_by(record_id, path_proc_number) |>
    dplyr::arrange(redcap_repeat_instance) |>
    dplyr::mutate(path_rep_number = dplyr::row_number()) |>
    dplyr::ungroup()
}
