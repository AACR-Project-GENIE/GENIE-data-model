#' Derive stage_dx and stage_dx_iv from AJCC staging variables
#'
#' Classifies cancer stage from `best_ajcc_stage_cd`, `ca_stage_iv`, and
#' `ca_stage`. Produces two new columns: `stage_dx` (granular: Stage 0–IV) and
#' `stage_dx_iv` (coarse: Stage 0, Stage I-III, Stage IV).
#'
#' @param dat A data frame containing columns `best_ajcc_stage_cd`,
#'   `ca_stage_iv`, and `ca_stage`.
#'
#' @returns `dat` with `stage_dx_iv` and `stage_dx` columns added.
#' @export
#'
#' @examples
#' # derive_stage_dx(ca_ind)
derive_stage_dx <- function(dat) {
  ajcc_missing <- c("88", "99", "")

  dat |>
    dplyr::mutate(
      stage_dx_iv = dplyr::case_when(
        !is.na(best_ajcc_stage_cd) &
          !(best_ajcc_stage_cd %in% ajcc_missing) &
          stringr::str_to_upper(best_ajcc_stage_cd) %in%
            c("0", "0A", "0IS", "OS") ~ "Stage 0",
        !is.na(best_ajcc_stage_cd) &
          !(best_ajcc_stage_cd %in% ajcc_missing) &
          stringr::str_to_upper(best_ajcc_stage_cd) %in%
            c("4", "4A", "4B", "4C") ~ "Stage IV",
        !is.na(best_ajcc_stage_cd) &
          !(best_ajcc_stage_cd %in% ajcc_missing) &
          !(stringr::str_to_upper(best_ajcc_stage_cd) %in%
            c("4", "4A", "4B", "4C")) ~ "Stage I-III",
        ca_stage_iv == "Yes" ~ "Stage IV",
        ca_stage_iv == "No" ~ "Stage I-III"
      ),
      stage_dx = dplyr::case_when(
        substr(best_ajcc_stage_cd, 1, 1) == "0" ~ "Stage 0",
        substr(best_ajcc_stage_cd, 1, 1) == "1" |
          stringr::str_to_upper(best_ajcc_stage_cd) %in%
            c("I", "IA") ~ "Stage I",
        substr(best_ajcc_stage_cd, 1, 1) == "2" ~ "Stage II",
        substr(best_ajcc_stage_cd, 1, 1) == "3" ~ "Stage III",
        substr(stringr::str_to_upper(best_ajcc_stage_cd), 1, 3) ==
          "III" ~ "Stage III",
        substr(best_ajcc_stage_cd, 1, 1) == "4" ~ "Stage IV",
        ca_stage_iv == "Yes" ~ "Stage IV",
        stringr::str_to_upper(ca_stage) %in%
          c("0", "0A", "0IS") ~ "Stage 0",
        ca_stage %in% c("I", "IA", "IB") ~ "Stage I",
        ca_stage %in% c("II", "IIA", "IIB", "IIC") ~ "Stage II",
        ca_stage %in% c("III", "IIIA", "IIIB", "IIIC") ~ "Stage III"
      )
    )
}
