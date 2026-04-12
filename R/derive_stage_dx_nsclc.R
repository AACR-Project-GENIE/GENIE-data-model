#' Derive stage_dx and stage_dx_iv for NSCLC using TNM fallback
#'
#' For NSCLC patients whose `stage_dx` is still `NA` after the universal
#' AJCC-based derivation in [derive_stage_dx()], attempts to fill in stage
#' using clinical and pathologic TNM group staging (7th edition AJCC rules
#' for lung cancer). Rows that already have a `stage_dx` value are left
#' unchanged.
#'
#' The priority between clinical and pathologic TNM depends on
#' `ca_tx_pre_path_stage`: if `"Yes"` (treatment preceded pathologic
#' staging), clinical TNM is preferred with pathologic as fallback; if
#' `"No"`, pathologic is preferred with clinical as fallback.
#'
#' @param dat A data frame of NSCLC cancer diagnoses that already has
#'   `stage_dx` and `stage_dx_iv` columns (from [derive_stage_dx()]).
#'   Must also contain: `best_ajcc_stage_cd`, `ca_stage_iv`, `ca_stage`,
#'   `ca_clin_t_stage`, `ca_clin_t1_det`, `ca_clin_t2_det`,
#'   `ca_clin_n_stage`, `ca_path_group_stage`, `ca_path_t_stage`,
#'   `ca_path_t1_det`, `ca_path_t2_det`, `ca_path_n_stage`, and
#'   `ca_tx_pre_path_stage`.
#'
#' @returns `dat` with `stage_dx` and `stage_dx_iv` updated for rows where
#'   the TNM fallback produced a result.
#' @export
#'
#' @examples
#' # derive_stage_dx_nsclc(ca_ind_nsclc)
derive_stage_dx_nsclc <- function(dat) {
  required_cols <- c(
    "stage_dx", "stage_dx_iv",
    "best_ajcc_stage_cd", "ca_stage_iv", "ca_stage",
    "ca_clin_t_stage", "ca_clin_t1_det", "ca_clin_t2_det",
    "ca_clin_n_stage",
    "ca_path_group_stage", "ca_path_t_stage",
    "ca_path_t1_det", "ca_path_t2_det", "ca_path_n_stage",
    "ca_tx_pre_path_stage"
  )
  missing_cols <- setdiff(required_cols, names(dat))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "Missing required column{?s} for NSCLC TNM staging: {.val {missing_cols}}"
    )
  }

  ajcc_missing <- c("88", "99", "")

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
      .ca_clin_tnm_group = dplyr::case_when(
        ca_clin_t_stage %in% "TX" &
          ca_clin_n_stage %in% "N0" ~ "Occult Carcinoma",
        ca_clin_t_stage %in% c("TpIS", "pIS") &
          ca_clin_n_stage %in% "N0" ~ "Stage 0",
        (ca_clin_t1_det %in% c("T1a", "T1b") |
          ca_clin_t2_det %in% "T2a") &
          ca_clin_n_stage %in% "N0" ~ "Stage I",
        ((ca_clin_t2_det == "T2b" | ca_clin_t_stage %in% "T3") &
          ca_clin_n_stage %in% "N0") |
          ((ca_clin_t1_det %in% c("T1a", "T1b") |
            ca_clin_t2_det %in% c("T2a", "T2b")) &
            ca_clin_n_stage %in% "N1") ~ "Stage II",
        (ca_clin_t_stage %in% "T4" & ca_clin_n_stage %in% "N0") |
          (ca_clin_t_stage %in% c("T3", "T4") &
            ca_clin_n_stage %in% "N1") |
          ((ca_clin_t_stage %in% c("T3", "T4") |
            ca_clin_t1_det %in% c("T1a", "T1b") |
            ca_clin_t2_det %in% c("T2a", "T2b")) &
            ca_clin_n_stage %in% "N2") |
          ((ca_clin_t_stage %in% c("T3", "T4") |
            ca_clin_t1_det %in% c("T1a", "T1b") |
            ca_clin_t2_det %in% c("T2a", "T2b")) &
            ca_clin_n_stage %in% "N3") ~ "Stage III"
      ),
      .ca_path_tnm_group = dplyr::case_when(
        ca_path_group_stage %in% c("0", "0A", "0is") ~ "Stage 0",
        ca_path_group_stage %in% c("I", "IA", "IA2", "IB", "IC") ~ "Stage I",
        ca_path_group_stage %in% c("II", "IIA", "IIB", "IIC") ~ "Stage II",
        ca_path_group_stage %in% c("III", "IIIA", "IIIB", "IIIC") ~ "Stage III",
        ca_path_group_stage %in% "IV" ~ "Stage IV",
        ca_path_t_stage %in% "TX" &
          ca_path_n_stage %in% "N0" ~ "Occult Carcinoma",
        (ca_path_t1_det %in% c("T1a", "T1b") |
          ca_path_t2_det %in% "T2a") &
          ca_path_n_stage %in% "N0" ~ "Stage I",
        ((ca_path_t2_det == "T2b" | ca_path_t_stage %in% "T3") &
          ca_path_n_stage %in% "N0") |
          ((ca_path_t1_det %in% c("T1a", "T1b") |
            ca_path_t2_det %in% c("T2a", "T2b")) &
            ca_path_n_stage %in% "N1") ~ "Stage II",
        (ca_path_t_stage %in% "T4" & ca_path_n_stage %in% "N0") |
          (ca_path_t_stage %in% c("T3", "T4") &
            ca_path_n_stage %in% "N1") |
          ((ca_path_t_stage %in% c("T3", "T4") |
            ca_path_t1_det %in% c("T1a", "T1b") |
            ca_path_t2_det %in% c("T2a", "T2b")) &
            ca_path_n_stage %in% "N2") |
          ((ca_path_t_stage %in% c("T3", "T4") |
            ca_path_t1_det %in% c("T1a", "T1b") |
            ca_path_t2_det %in% c("T2a", "T2b")) &
            ca_path_n_stage %in% "N3") ~ "Stage III"
      ),
      .filled_in_stage = dplyr::case_when(
        .elig_specific_stage == 1 & ca_tx_pre_path_stage == "Yes" &
          !is.na(.ca_clin_tnm_group) ~ .ca_clin_tnm_group,
        .elig_specific_stage == 1 & ca_tx_pre_path_stage == "Yes" &
          is.na(.ca_clin_tnm_group) &
          !is.na(.ca_path_tnm_group) ~ .ca_path_tnm_group,
        .elig_specific_stage == 1 & ca_tx_pre_path_stage == "No" &
          !is.na(.ca_path_tnm_group) ~ .ca_path_tnm_group,
        .elig_specific_stage == 1 & ca_tx_pre_path_stage == "No" &
          is.na(.ca_path_tnm_group) &
          !is.na(.ca_clin_tnm_group) ~ .ca_clin_tnm_group
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
