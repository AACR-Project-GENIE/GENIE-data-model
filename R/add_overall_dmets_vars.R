#' Add Stage I-III distant metastasis flag and overall timing
#'
#' Adds `dmets_stage_i_iii` (0/1 for Stage I-III patients who developed
#' any distant met post-diagnosis) and `dx_to_dmets_days` (days from
#' diagnosis to the earliest distant met across all site groups). Both
#' are `NA` for Stage IV patients.
#'
#' @param dmets_combined Output of [combine_dmets_derivations()].
#' @param ca_ind Index cancer data frame containing `record_id`, `ca_seq`,
#'   and `stage_dx`.
#'
#' @returns `dmets_combined` with two additional columns:
#'   `dmets_stage_i_iii` and `dx_to_dmets_days`.
#' @export
#'
#' @examples
#' # add_overall_dmets_vars(dmets_combined, ca_ind)
add_overall_dmets_vars <- function(dmets_combined, ca_ind) {
  stages_i_iii <- c(
    "Stage 0",
    "Stage I",
    "Stage II",
    "Stage III",
    "Stage I-III NOS"
  )

  dmets_cols <- grep("^dmets_", names(dmets_combined), value = TRUE)
  days_cols <- grep(
    "^dx_to_dmets_.*_days$",
    names(dmets_combined),
    value = TRUE
  )

  dmets_combined |>
    dplyr::left_join(
      ca_ind |> dplyr::distinct(record_id, ca_seq, stage_dx),
      by = c("record_id", "ca_seq")
    ) |>
    dplyr::rowwise() |>
    # suppressWarnings: all-NA rows produce Inf/-Inf, cleaned up by case_when
    dplyr::mutate(
      dmets_any = sum(dplyr::c_across(dplyr::all_of(dmets_cols)), na.rm = TRUE),
      dmets_stage_i_iii = dplyr::case_when(
        stage_dx %in% stages_i_iii & dmets_any > 0 ~ 1L,
        stage_dx %in% stages_i_iii ~ 0L
      ),
      dx_to_dmets_days = dplyr::case_when(
        stage_dx %in% stages_i_iii & dmets_stage_i_iii == 1L ~
          suppressWarnings(min(dplyr::c_across(dplyr::all_of(days_cols)), na.rm = TRUE))
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-dmets_any, -stage_dx)
}
