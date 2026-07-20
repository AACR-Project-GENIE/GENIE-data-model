#' @title Filter Stage (messy)
#' 
#' @description Try desperately to make something sensible out of this mess.
#' 
#' @param dat The cancer index dataset, or at least having those variables.
filter_stage_light <- function(
    dat,
    excluded_best_ajcc_stage_cd = c(
      "1A", "1A1", "1A2", "1A3",
      "3A/B", "3B", "3C", "3a", "IIIB"
    ),
    excluded_ca_path_group_stage = c(
      "0", "0A", "0is", "I", "IA", "IA1", "IA2", "IA3", # what's IS?
      "IIIB"
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
    
    n_ajcc_out <- dat %>% 
      filter(best_ajcc_stage_cd %in% excluded_best_ajcc_stage_cd) %>%
      nrow
    cli::cli_inform("There are {n_ajcc_out} people we expect to be filtered out due to tumor registry (best_ajcc_stage_cd) codes.")
    
    n_curated_out <- dat %>% 
      filter(!.e_best_ajcc_stage_cd & ca_path_group_stage %in% excluded_ca_path_group_stage) %>%
      nrow
    cli::cli_inform("There are {n_curated_out} people we expect to be filtered out due to curated (ca_path_group_stage) codes.")
    
  }
  
  dat %<>%
    mutate(
      in_excluded_stage = case_when(
        stage_dx %in% "Stage II" ~ F, # special case.
        .e_best_ajcc_stage_cd & best_ajcc_stage_cd %in% excluded_best_ajcc_stage_cd ~ T,
        .e_best_ajcc_stage_cd ~ F,
        .e_ca_path_group_stage & ca_path_group_stage %in% excluded_ca_path_group_stage ~ T,
        T ~ F # If we have no information, they get labelled false.
      )
    )
  
  dat %<>%
    filter(!in_excluded_stage) %>%
    select(-c(
      in_excluded_stage,
      .e_best_ajcc_stage_cd,
      .e_ca_path_group_stage
    ))
  return(dat)
  
}
