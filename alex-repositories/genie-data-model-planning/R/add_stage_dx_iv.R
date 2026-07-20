add_stage_dx_iv <- function(dat) {
  dat %>%
    mutate(
      stage_dx_iv = case_when(
        !is.na(best_ajcc_stage_cd) &
          str_sub(best_ajcc_stage_cd, 1, 1) %in% c("0", "O") ~
          "Stage 0",
        !is.na(best_ajcc_stage_cd) &
          str_sub(best_ajcc_stage_cd, 1, 1) %in% "4" ~
          "Stage IV",
        !is.na(best_ajcc_stage_cd) ~ "Stage I-III",
        ca_stage_iv %in% c("0", "No") ~ "Stage I-III",
        ca_stage_iv %in% c("1", "Yes") ~ "Stage IV"
      )
    )
}
