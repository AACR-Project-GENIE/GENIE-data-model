#' Derive diagnosis-to-drug timing intervals on the regimen table
#'
#' Adds `dx_drug_start_int_<n>`, `dx_drug_end_int_<n>`,
#' `dx_drug_end_or_lastadm_int_<n>`, `dx_reg_start_int`, and
#' `dx_reg_end_any_int` columns to the regimen (`reg`) table in `lst`,
#' representing the number of days from the cancer diagnosis (for the
#' cancer in that row, identified by `redcap_ca_seq`) to the start, end, and
#' combined end-or-last-administered date of each drug in the regimen, and
#' to the regimen start and end-of-any-drug overall. Computed by subtracting
#' `dob_ca_dx_days` from the corresponding DOB-indexed
#' `drugs_startdt_int_<n>`, `drugs_enddt_int_<n>`,
#' `drugs_drug_end_or_lastadm_int_<n>`, `dob_reg_start_int`, and
#' `dob_reg_end_any_int` columns (the end-or-lastadm columns produced
#' upstream by [derive_drug_end_or_lastadm_int()], `dob_reg_start_int` by
#' [derive_reg_start_int()], and `dob_reg_end_any_int` by
#' [derive_reg_end_any_int()]). The month versions (`_mos`) are
#' intentionally not produced here.
#'
#' @param lst A list of PRISSMM tables. Must contain `ca_ind`, `ca_non_ind`, and
#'   `reg`. The cancer index tables must have `record_id`, `redcap_ca_seq`, and
#'   `dob_ca_dx_days`. `reg` must have `record_id`, `redcap_ca_seq`, and the
#'   wide `drugs_startdt_int_<n>`, `drugs_enddt_int_<n>`,
#'   `drugs_drug_end_or_lastadm_int_<n>`, `dob_reg_start_int`, and
#'   `dob_reg_end_any_int` columns.
#'
#' @returns `lst` with the new `dx_drug_start_int_<n>`, `dx_drug_end_int_<n>`,
#'   `dx_drug_end_or_lastadm_int_<n>`, `dx_reg_start_int`, and
#'   `dx_reg_end_any_int` columns added to `lst$reg`.
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
      ),
      dx_reg_start_int = dob_reg_start_int - dob_ca_dx_days,
      dx_reg_end_any_int = dob_reg_end_any_int - dob_ca_dx_days
    ) |>
    dplyr::select(-dob_ca_dx_days)

  lst
}
