#' @param dat_poss_app A 'possible approvals' dataset, made with
#'   make_possible_indication_cohort().
#' @param with_req The 'with' entry required to merit this check.
#' @param drug_req The 'agent' entry required to satisfy this check.
add_check_with_simple <- function(
    dat_poss_app,
    with_req,
    agent_req = NULL,
    test_name = NULL,
    verbose_results = T
) {
  if (!any(dat_poss_app$ind_with %in% with_req)) {
    cli_warn("There are no rows with ind_with = {with_req} - typo?")
  }
  
  if (is.null(agent_req)) {
    agent_req = with_req
  }
  
  if (is.null(test_name)) {
    # default:  use the agent_req name.
    test_name = paste0(
      "test_with_", 
      tolower(
        str_replace_all(
          paste(
            agent_req, collapse = "_and_"
          ),
          " ", "_"
        )
      )
    )
  }
  
  
  dat_poss_app %<>%
    mutate(
      .has_drug = map_lgl(
        .x = drug_overlaps,
        # all() only has real meaning when length(agent_req) > 1.
        .f = \(z) all(agent_req %in% z)
      ),
      .test_temp = case_when(
        is.na(ind_with) ~ T, 
        !(ind_with %in% with_req) ~ T,
        .has_drug ~ T,
        T ~ F
      )
    )
  
  if (verbose_results) {
    # it would be odd (not impossible) if a drug we're check had NO overlapping uses in the data.
    verbose_dat <- dat_poss_app %>%
      filter(
        !is.na(ind_with),
        ind_with %in% with_req
      )
    nrow_tot <- nrow(verbose_dat)
    nrow_passed <- nrow(filter(verbose_dat, .test_temp))
    nrow_failed <- nrow(filter(verbose_dat, !.test_temp))
    cli_inform("Of the {nrow_tot} rows with ind_with = {with_req}, {nrow_passed} had {agent_req} and {nrow_failed} did not.")
  }
  
  dat_poss_app %<>%
    select(-.has_drug) %>%
    rename({{test_name}} := .test_temp)
  
  return(dat_poss_app)
  
}


