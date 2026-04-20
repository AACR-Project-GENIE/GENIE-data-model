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
#' @param first_cancer_only If `TRUE`, only populate `dmets_stage_i_iii`
#'   and `dx_to_dmets_days` for the patient's first index cancer (lowest
#'   `ca_seq`). Default `FALSE`.
#'
#' @returns `dmets_combined` with two additional columns:
#'   `dmets_stage_i_iii` and `dx_to_dmets_days`.
#' @export
#'
#' @examples
#' # add_overall_dmets_vars(dmets_combined, ca_ind)
add_overall_dmets_vars <- function(
  dmets_combined,
  ca_ind,
  first_cancer_only = FALSE
) {
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

  ca_info <- ca_ind |> dplyr::distinct(record_id, ca_seq, stage_dx)

  if (first_cancer_only) {
    first_ca <- ca_ind |>
      dplyr::group_by(record_id) |>
      dplyr::slice_min(ca_seq, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::distinct(record_id, ca_seq) |>
      dplyr::mutate(is_first_ca = TRUE)

    ca_info <- ca_info |>
      dplyr::left_join(first_ca, by = c("record_id", "ca_seq")) |>
      dplyr::mutate(is_first_ca = tidyr::replace_na(is_first_ca, FALSE))
  } else {
    ca_info <- ca_info |>
      dplyr::mutate(is_first_ca = TRUE)
  }

  dmets_combined |>
    dplyr::left_join(ca_info, by = c("record_id", "ca_seq")) |>
    dplyr::rowwise() |>
    # suppressWarnings: all-NA rows produce Inf/-Inf, cleaned up by case_when
    dplyr::mutate(
      dmets_any = sum(dplyr::c_across(dplyr::all_of(dmets_cols)), na.rm = TRUE),
      dmets_stage_i_iii = dplyr::case_when(
        is_first_ca & stage_dx %in% stages_i_iii & dmets_any > 0 ~ 1L,
        is_first_ca & stage_dx %in% stages_i_iii ~ 0L
      ),
      dx_to_dmets_days = dplyr::case_when(
        is_first_ca & stage_dx %in% stages_i_iii & dmets_stage_i_iii == 1L ~
          suppressWarnings(min(
            dplyr::c_across(dplyr::all_of(days_cols)),
            na.rm = TRUE
          ))
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-dmets_any, -stage_dx, -is_first_ca)
}
