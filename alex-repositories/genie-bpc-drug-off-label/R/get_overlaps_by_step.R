# This is a helper used in analysis to go back and look at exposures which
#   were declared off label after adding a certain test ("step"), and pull
#   the drug overlaps that drug DID have.
get_overlaps_by_step <- function(
    dat_stepwise_new_olu,
    dat_hdrug_conmed,
    fail_step,
    cohort_limit = NULL,
    agent_limit = NULL,
    rtn_type = "count",
    replace_blank_with = "(none)"
) {
  rtn <- dat_stepwise_new_olu %>%
    filter(step %in% fail_step) %>% 
    pull(new_off_label) %>% 
    `[[`(.,1)
  
  if (!is.null(cohort_limit)) {
    rtn %<>% filter(cohort %in% cohort_limit)
  }
  
  if (!is.null(agent_limit)) {
    rtn %<>% filter(agent %in% agent_limit)
  }
  
  
  rtn <- left_join(
    rtn,
    dat_hdrug_conmed, 
    # a bit excessive here - but fine:
    by = c('cohort', 'record_id', 'ca_seq', 'regimen_number',
           'drug_number', 'agent')
  )
  
  rtn %<>%
    mutate(
      drug_overlaps_vec = if_else(
        drug_overlaps_vec %in% "",
        replace_blank_with,
        drug_overlaps_vec
      )
    )
  
  if (rtn_type %in% "full") {
    return(rtn)
  } else if (rtn_type %in% "count") {
    return(
      count(rtn, cohort, agent, drug_overlaps_vec, sort = T, name = "exposures") 
    )
  } else {
    cli_abort("Invalid return type")
  }
}