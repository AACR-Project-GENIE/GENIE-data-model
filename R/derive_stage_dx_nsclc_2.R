#' Derive stage_dx and stage_dx_iv for NSCLC using group stage fallback
#'
#' For NSCLC patients whose `stage_dx` is still `NA` after the universal
#' AJCC-based derivation in [derive_stage_dx()], attempts to fill in stage
#' using clinical and pathologic group staging. Rows that already have a
#' `stage_dx` value are left unchanged.
#'
#' Unlike [derive_stage_dx_nsclc()], this version uses `ca_clin_group_stage`
#' and `ca_path_group_stage` directly rather than reconstructing from
#' individual T/N components.
#'
#' The priority between clinical and pathologic depends on
#' `ca_tx_pre_path_stage`: if `"Yes"` (treatment preceded pathologic
#' staging), clinical is preferred with pathologic as fallback; if
#' `"No"`, pathologic is preferred with clinical as fallback.
#'
#' @param dat A data frame of NSCLC cancer diagnoses that already has
#'   `stage_dx` and `stage_dx_iv` columns (from [derive_stage_dx()]).
#'   Must also contain: `best_ajcc_stage_cd`, `ca_stage_iv`, `ca_stage`,
#'   `ca_clin_group_stage`, `ca_path_group_stage`, and
#'   `ca_tx_pre_path_stage`.
#'
#' @returns `dat` with `stage_dx` and `stage_dx_iv` updated for rows where
#'   the group stage fallback produced a result.
#' @export
#'
#' @examples
#' # derive_stage_dx_nsclc_2(ca_ind_nsclc)
derive_stage_dx_nsclc_2 <- function(dat) {
  required_cols <- c(
    "stage_dx", "stage_dx_iv",
    "best_ajcc_stage_cd", "ca_stage_iv", "ca_stage",
    "ca_clin_group_stage", "ca_path_group_stage",
    "ca_tx_pre_path_stage"
  )
  missing_cols <- setdiff(required_cols, names(dat))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "Missing required column{?s} for NSCLC group staging: {.val {missing_cols}}"
    )
  }

  ajcc_missing <- c("88", "99", "")

  stage_groups <- list(
    "Stage 0" = c("0", "0A", "0is"),
    "Stage I" = c("I", "IA", "IA1", "IA2", "IA3", "IB", "IC"),
    "Stage II" = c("II", "IIA", "IIB", "IIC"),
    "Stage III" = c("III", "IIIA", "IIIA1", "IIIA2", "IIIB", "IIIC"),
    "Stage IV" = c("IV", "IVA", "IVB", "IVC")
  )

  dat |>
    dplyr::mutate(
      .elig_specific_stage = dplyr::case_when(
        is.na(stage_dx) &
          (best_ajcc_stage_cd %in% ajcc_missing | is.na(best_ajcc_stage_cd)) &
          (ca_stage_iv %in% c("No", "Not Applicable", "Unknown") |
            is.na(ca_stage_iv)) &
          (ca_stage %in% c("Not Applicable", "Unknown") |
            is.na(ca_stage)) ~ 1
      ),
      .ca_clin_group = dplyr::case_when(
        ca_clin_group_stage %in% stage_groups[["Stage 0"]] ~ "Stage 0",
        ca_clin_group_stage %in% stage_groups[["Stage I"]] ~ "Stage I",
        ca_clin_group_stage %in% stage_groups[["Stage II"]] ~ "Stage II",
        ca_clin_group_stage %in% stage_groups[["Stage III"]] ~ "Stage III",
        ca_clin_group_stage %in% stage_groups[["Stage IV"]] ~ "Stage IV"
      ),
      .ca_path_group = dplyr::case_when(
        ca_path_group_stage %in% stage_groups[["Stage 0"]] ~ "Stage 0",
        ca_path_group_stage %in% stage_groups[["Stage I"]] ~ "Stage I",
        ca_path_group_stage %in% stage_groups[["Stage II"]] ~ "Stage II",
        ca_path_group_stage %in% stage_groups[["Stage III"]] ~ "Stage III",
        ca_path_group_stage %in% stage_groups[["Stage IV"]] ~ "Stage IV"
      ),
      .filled_in_stage = dplyr::case_when(
        .elig_specific_stage == 1 & ca_tx_pre_path_stage == "Yes" &
          !is.na(.ca_clin_group) ~ .ca_clin_group,
        .elig_specific_stage == 1 & ca_tx_pre_path_stage == "Yes" &
          is.na(.ca_clin_group) &
          !is.na(.ca_path_group) ~ .ca_path_group,
        .elig_specific_stage == 1 & ca_tx_pre_path_stage == "No" &
          !is.na(.ca_path_group) ~ .ca_path_group,
        .elig_specific_stage == 1 & ca_tx_pre_path_stage == "No" &
          is.na(.ca_path_group) &
          !is.na(.ca_clin_group) ~ .ca_clin_group
      ),
      stage_dx = dplyr::case_when(
        !is.na(stage_dx) ~ stage_dx,
        !is.na(.filled_in_stage) ~ .filled_in_stage,
        stage_dx_iv == "Stage I-III" ~ "Stage I-III NOS",
        .default = stage_dx_iv
      ),
      stage_dx_iv = dplyr::case_when(
        stage_dx == "Stage 0" &
          stage_dx_iv == "Stage I-III" ~ "Stage 0",
        .default = stage_dx_iv
      )
    ) |>
    drop_dots()
}
