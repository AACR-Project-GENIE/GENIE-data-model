
#' @param dat_poss_app A 'possible approvals' dataset, made with
#'   make_possible_indication_cohort().
#' @param with_req The 'with' entry required to merit this check.
#' @param drug_req The 'agent' entry required to satisfy this check.
add_check_biomarker_simple <- function(
    dat_poss_app,
    ind_sheet_bio_req,
    biom_col,
    test_name = NULL,
    verbose_results = T
) {
  if (!any(dat_poss_app$ind_biomarker %in% ind_sheet_bio_req)) {
    cli_warn("There are no rows with biomarker = {ind_sheet_bio_req} - typo?")
  }
  
  dat_poss_app %<>%
    mutate(
      .test_temp = case_when(
        is.na(ind_biomarker) ~ T, 
        is.na(.data[[biom_col]]) ~ T, # means the biomarker wasn't tested.
        !(ind_biomarker %in% ind_sheet_bio_req) ~ T, # indication doesn't require this biomarker.
        !(.data[[biom_col]]) ~ F, # biomarker tested and negative.
        T ~ T
      )
    )
  
  # This will give a pretty bad result, but it will at least run.
  if (is.null(test_name)) {
    test_name <- paste0("test_", biom_col)
  }
  
  if (verbose_results) {
    # it would be odd (not impossible) if a drug we're check had NO overlapping uses in the data.
    verbose_dat <- dat_poss_app %>%
      filter(
        !is.na(ind_biomarker),
        ind_biomarker %in% ind_sheet_bio_req 
      )
    nrow_tot <- nrow(verbose_dat)
    nrow_passed <- nrow(filter(verbose_dat, .test_temp))
    nrow_failed <- nrow(filter(verbose_dat, !.test_temp))
    cli_inform("Of the {nrow_tot} rows with ind_biomarker = {ind_sheet_bio_req}, {nrow_failed} were definitely not {biom_col} (failing) and {nrow_passed} were (passing).")
  }
  
  dat_poss_app %<>%
    rename({{test_name}} := .test_temp)
  
  return(dat_poss_app)
  
}
