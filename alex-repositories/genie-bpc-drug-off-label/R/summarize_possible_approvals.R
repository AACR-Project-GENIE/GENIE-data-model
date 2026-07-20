
summarize_possible_approvals_2 <- function(
    dat_poss_app,
    group_cols = c('cohort', 'record_id', 'ca_seq', 'regimen_number',
                   'drug_number', 'agent')
) {
  check_test_cols_true_false(dat_poss_app)
  
  test_cols_except_all <- names(dft_poss_app)
  test_cols_except_all <- test_cols_except_all[
    str_detect(test_cols_except_all, "^test") & !str_detect(test_cols_except_all, "^test_all$")
  ]
  
  
  dat_poss_app <- dat_poss_app %>%
    group_by(across(all_of(group_cols))) %>%
    arrange(across(.cols = all_of(test_cols_except_all), .fns = list(desc))) %>%
    mutate(fail_count = length(test_cols_except_all) - rowSums(pick(all_of(test_cols_except_all)))) 
  
  rtn <- dat_poss_app %>% 
    summarize(
      valid_ind_exists = any(fail_count %in% 0),
      .groups = 'drop'
    )
    
  # closest indication meaning that one that passed the most consecutive checks.
  fail_reason_in_closest_ind <- dat_poss_app %>%
    anti_join(., filter(rtn, valid_ind_exists), by = group_cols) %>%
    select(all_of(group_cols), all_of(test_cols_except_all)) %>%
    mutate(.ind_num = row_number()) %>%
    pivot_longer(
      cols = all_of(test_cols_except_all),
      names_to = "test_name",
      values_to = "pass"
    )
  
  fail_reason_in_closest_ind %<>%
    group_by(across(all_of(group_cols)), .ind_num) %>%
    filter(!pass) %>%
    summarize(
      failure_depth = match(first(test_name), test_cols_except_all),
      failure_type_this_ind = first(test_name),
      .groups = "drop"
    )
  
  fail_reason_in_closest_ind %<>%
    group_by(across(all_of(group_cols))) %>%
    arrange(desc(failure_depth)) %>%
    summarize(
      failure_type = first(failure_type_this_ind),
      .groups = "drop"
    )
  
  rtn %<>%
    left_join(
      .,
      fail_reason_in_closest_ind,
      by = group_cols
    )
  
  chk_completeness <- rtn %>%
    filter(!xor(valid_ind_exists, !is.na(failure_type))) 
  
  if (nrow(chk_completeness) > 0) {
    chk_completeness %>%
      slice(1:min(n(),3)) %>%
      glimpse
    cli_abort("Rows exist with both a valid indication and failure reason.")
  }
  
  return(rtn)
  
} 


# Saving until this issue is dead and buried:
# 
# summarize_possible_approvals <- function(
#     dat_poss_app,
#     group_cols = c('cohort', 'record_id', 'ca_seq', 'regimen_number',
#                    'drug_number', 'agent')
# ) {
#   check_test_cols_true_false(dat_poss_app)
#   
#   rtn <- dat_poss_app %>%
#     group_by(across(all_of(group_cols))) %>%
#     summarize(
#       valid_ind_exists = sum(test_all, na.rm = T) >= 1,
#       # currently needs to be manually updated, which I hate.
#       failure_type = case_when(
#         !any(test_ind_exists) ~ "test_ind_exists",
#         !any(test_ind_exists & test_met) ~ "test_met",
#         !any(test_ind_exists & test_met & test_monotherapy) ~ "test_monotherapy",
#         !any(test_ind_exists & test_met & test_monotherapy & 
#                test_date_definite) ~ "test_date_definite",
#         T ~ NA_character_
#       ),
#       .groups = "drop"
#     )
#   
#   # Check that failure type is always categorized if indication does not exist.
#   chk_failure_type <- rtn %>%
#     filter(is.na(failure_type) & !valid_ind_exists)
#   if (nrow(chk_failure_type) > 0) {
#     cli_abort("Some failure types failed to be categorized when indications did not exist - needs to be fixed.  Returning failed cases.")
#   }
#   
#   return(rtn)
#   
# }

summarize_possible_approvals_no_fail_type <- function(
    dat_poss_app,
    group_cols = c('cohort', 'record_id', 'ca_seq', 'regimen_number',
                   'drug_number', 'agent')
) {
  check_test_cols_true_false(dat_poss_app)
  
  rtn <- dat_poss_app %>%
    group_by(across(all_of(group_cols))) %>%
    summarize(
      valid_ind_exists = sum(test_all, na.rm = T) >= 1,
      .groups = "drop"
    )
  
  return(rtn)
  
}
