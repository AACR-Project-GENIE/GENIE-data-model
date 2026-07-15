#' @title Create a binary pathway feature dataframe.
#' 
#' @description Operates on any column in the alterations dataset.
#' 
#' @param dat_alt A dataframe with one row per alteration.
#' @param dat_cpt A dataframe of the format "cancer panel test" from BPC PRISSMM.  The sample IDs here define the samples in the output.
#' @param path_col The column with the pathway data.  Note:  This will technically work on any column in the dat_alt dataset, with the caveat that it hasn't been tested.
create_pathway_binary <- function(
    dat_alt, 
    dat_cpt,
    path_col = "pathway"
) {
  
  dat_path_bin <- dat_alt %>%
    group_by(sample_id, .data[[path_col]]) %>%
    summarize(exists = n() >= 1, .groups = "drop") %>%
    pivot_wider(
      names_from = all_of(path_col),
      values_from = exists
    )
  
  # dat_cpt defines the cohort.  
  # we leave behind any dat_alt rows without a row in dat_cpt here:
  dat_path_bin <- dat_cpt %>%
    select(sample_id = cpt_genie_sample_id) %>%
    left_join(., dat_path_bin, by = "sample_id")
  
  dat_path_bin %<>%
    mutate(
      across(
        .cols = -sample_id,
        .fns = (function(x) {
          x <- as.integer(x)
          x <- if_else(is.na(x), 0L, x)
        })
      )
    ) %>%
    # just ordering:
    select(sample_id, (levels(dat_alt[[path_col]])))
  
  return(dat_path_bin)
  
}