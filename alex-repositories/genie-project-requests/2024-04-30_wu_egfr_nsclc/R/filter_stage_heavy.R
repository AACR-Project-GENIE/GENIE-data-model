#' @title Filter Stage (messy)
#' 
#' @description Filter by only including those with a confirmed stage in the range we want 
#' 
#' @param dat The cancer index dataset, or at least having those variables.
filter_stage_heavy <- function(
    dat,
    included_best_ajcc_stage_cd = c(
      '1B', '2A', '2B', '3A', '3a'
    ),
    included_ca_path_group_stage = c(
      'IB', 'II', 'IIA', 'IIB', 'IIIA'
    ),
    print_messages = T
) {
    
  dat %<>%
    # .e_ = exists
    mutate(
      .e_ca_path_group_stage = case_when(
        is.na(ca_path_group_stage) ~ F,
        ca_path_group_stage %in% c("Not Applicable", "Unknown") ~ F,
        T ~ T
      ),
      .e_best_ajcc_stage_cd = case_when(
        is.na(best_ajcc_stage_cd) ~ F,
        best_ajcc_stage_cd %in% c("88", "99") ~ F, # no clue on these.
        T ~ T
      ),
    )
  
  n_have_neither <- dat %>% 
    filter(!.e_ca_path_group_stage & !.e_best_ajcc_stage_cd) %>%
    nrow(.)
  
  if (print_messages) {
    n_have_neither <- dat %>% 
      filter(!.e_ca_path_group_stage & !.e_best_ajcc_stage_cd) %>%
      nrow(.)
    cli::cli_inform("Out of {nrow(dat)} rows, {n_have_neither} rows have insufficient tumor registry or pathology data to evaluate at all.")
  }
  
  dat %<>%
    mutate(
      in_included_stage = case_when(
        stage_dx %in% "Stage II" ~ T, # special case.
        .e_best_ajcc_stage_cd & best_ajcc_stage_cd %in% included_best_ajcc_stage_cd ~ T,
        .e_best_ajcc_stage_cd ~ F,
        .e_ca_path_group_stage & ca_path_group_stage %in% included_ca_path_group_stage ~ T,
        T ~ F # If we have no information, we can't confirm stage at all.
      )
    )
  
  dat %<>%
    filter(in_included_stage) %>%
    select(-c(
      in_included_stage,
      .e_best_ajcc_stage_cd,
      .e_ca_path_group_stage
    ))
  return(dat)
  
}
