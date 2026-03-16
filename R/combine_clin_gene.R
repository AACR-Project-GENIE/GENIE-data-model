
#' @description Joins the datasets and derives a handful of commonly useful variables.
#' @param dat_gene_comb The output of combine_cpt_gene_feat.  This gives the primary key set for the join (in other words, dat_clin is left joined onto this).
#' @param dat_clin A dataset of clinical characteristics with primary key {record_id, ca_seq} which will be joined to dat_gene_comb.
combine_clin_gene <- function(dat_gene_comb, dat_clin) {
  rtn <- left_join(
    dat_gene_comb,
    dat_clin,
    by = c("record_id", "ca_seq"),
    multiple = "error"
  )
  
  # Add some variables for the sake of interpretability:
  rtn <- rtn %>%
    mutate(
      stage_dx_iv_num = if_else(stage_dx_iv %in% "Stage IV", 1, 0),
      age_dx_c = age_dx - 40, # approximately centered.
      birth_year_c = birth_year - 1970, # approximately centered.
    )
  return(rtn)
}