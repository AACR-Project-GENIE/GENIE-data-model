#' Derive diagnosis-to-drug timing intervals on the regimen table
#'
#' Adds `dx_drug_start_int_<n>` and `dx_drug_end_int_<n>` columns to the regimen
#' (`reg`) table in `lst`, representing the number of days from the cancer
#' diagnosis (for the cancer in that row, identified by `redcap_ca_seq`) to the
#' start and end of each drug in the regimen. Computed as `drugs_startdt_int_<n>
#' - dob_ca_dx_days` and `drugs_enddt_int_<n> - dob_ca_dx_days`. The month
#' versions (`_mos`) are intentionally not produced here.
#'
#' @param lst A list of PRISSMM tables. Must contain `ca_ind`, `ca_non_ind`, and
#'   `reg`. The cancer index tables must have `record_id`, `redcap_ca_seq`, and
#'   `dob_ca_dx_days`. `reg` must have `record_id`, `redcap_ca_seq`, and the
#'   wide `drugs_startdt_int_<n>` / `drugs_enddt_int_<n>` columns.
#'
#' @returns `lst` with the new `dx_drug_start_int_<n>` and `dx_drug_end_int_<n>`
#'   columns added to `lst$reg`.
#' @export
#'
#' @examples
#' # derive_dx_drug_int(lst)
derive_dx_drug_int <- function(lst) {
  dx_lookup <- dplyr::bind_rows(
    lst[["ca_ind"]],
    lst[["ca_non_ind"]]
  ) |>
    dplyr::select(record_id, redcap_ca_seq, dob_ca_dx_days)

  lst[["reg"]] <- lst[["reg"]] |>
    dplyr::left_join(dx_lookup, by = c("record_id", "redcap_ca_seq")) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::matches("^drugs_startdt_int_\\d+$"),
        .fns = \(x) x - dob_ca_dx_days,
        .names = "{stringr::str_replace(.col, '^drugs_startdt_int_', 'dx_drug_start_int_')}"
      ),
      dplyr::across(
        .cols = dplyr::matches("^drugs_enddt_int_\\d+$"),
        .fns = \(x) x - dob_ca_dx_days,
        .names = "{stringr::str_replace(.col, '^drugs_enddt_int_', 'dx_drug_end_int_')}"
      ),
      dplyr::across(
        .cols = dplyr::matches("^drugs_drug_end_or_lastadm_int_\\d+$"),
        .fns = \(x) x - dob_ca_dx_days,
        .names = "{stringr::str_replace(.col, '^drugs_drug_end_or_lastadm_int_', 'dx_drug_end_or_lastadm_int_')}"
      )
    ) |>
    dplyr::select(-dob_ca_dx_days)

  lst
}
