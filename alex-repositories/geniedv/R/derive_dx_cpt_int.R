#' Derive diagnosis-to-CPT-report timing interval on the CPT table
#'
#' Adds `dx_cpt_rep_days` to `lst$cpt`, representing the number of days from
#' cancer diagnosis to the CPT report date. Computed by subtracting
#' `dob_ca_dx_days` from `dob_cpt_report_days`.
#'
#' @param lst A list of PRISSMM tables. Must contain `ca_ind`, `ca_non_ind`,
#'   and `cpt`. The cancer tables must have `record_id`, `redcap_ca_seq`, and
#'   `dob_ca_dx_days`. `cpt` must have `record_id`, `redcap_ca_seq`, and
#'   `dob_cpt_report_days`.
#'
#' @returns `lst` with `dx_cpt_rep_days` added to `lst$cpt`.
#' @export
#'
#' @examples
#' # derive_dx_cpt_int(lst)
derive_dx_cpt_int <- function(lst) {
  dx_lookup <- dplyr::bind_rows(
    lst[["ca_ind"]],
    lst[["ca_non_ind"]]
  ) |>
    dplyr::select(record_id, redcap_ca_seq, dob_ca_dx_days)

  lst[["cpt"]] <- lst[["cpt"]] |>
    dplyr::left_join(dx_lookup, by = c("record_id", "redcap_ca_seq")) |>
    dplyr::mutate(
      dx_cpt_rep_days = dob_cpt_report_days - dob_ca_dx_days
    ) |>
    dplyr::select(-dob_ca_dx_days)

  lst
}
