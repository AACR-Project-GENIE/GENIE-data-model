#' @title Join the two tables together to form a list of possible indications
#.    using the cancer cohort name as the cancer type for indication.
make_possible_indication_cohort <- function(
    dat_hdrug,
    dat_ind
) {
  dat_hdrug %<>% 
    select(
      cohort, record_id, ca_seq, regimen_number, drug_number, agent,
      dmet_at_drug_start, 
      drug_start_date_min, drug_start_date_max,
      dx_drug_start_int,
      dx_drug_end_or_lastadm_int,
      drug_overlaps,
      num_overlaps,
      matches("^biom_")
    )
  
  dat_ind %<>%
    select(
      cohort = mapped_cohort,
      agent = mapped_agent,
      stage_or_status,
      ind_with = with,
      ind_date = date,
      ind_biomarker = biomarker
    ) %>%
    mutate(test_ind_exists = T)
  
  dat_poss_app <- left_join(
    dat_hdrug,
    dat_ind,
    by = c('cohort', 'agent'),
    relationship = "many-to-many"
  )
  
  dat_poss_app %<>% 
    mutate(
      test_ind_exists = if_else(
        is.na(test_ind_exists), 
        F, 
        test_ind_exists
      )
    )
  
  return(dat_poss_app)
}
