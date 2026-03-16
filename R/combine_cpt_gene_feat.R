
#' #' @description Combines the data from the clinical curation (CPT dataset) with
#' the gene features from main GENIE. 
#' @param dat_gene_feat a "long" gene feature dataframe with columns cpt_genie_sample_id, feature, value.  Example features are "PTEN_mut or TP53_cna", values should be 0 or 1.
#' @param dat_cpt a subset of the cancer panel test dataset which includes samples that meet the inclusion criteria for this analysis (varies).
#' @param keep_only_first keeps only the first sample if a participant has more than one eligible test.
combine_cpt_gene_feat <- function(dat_cpt, dat_gene_feat, keep_only_first = T) {
  if (max(pull(count(dat_cpt, cpt_genie_sample_id, sort = T), n)) > 1) {
    cli::cli_abort("Duplicate NGS rows in dat_cpt.")
  }
  
  # Edit: we will do this later on - makes no sense to do it here.
  # vec_samples <- dat_cpt$cpt_genie_sample_id %>% unique
  # 
  # rtn <- filter_gene_features(
  #   dat_gene_feat = dat_gene_feat,
  #   vec_samples = vec_samples
  # )
  
  rtn <- dat_gene_feat %>%
    pivot_wider(
      names_from = "feature",
      values_from = "value",
      values_fill = NA
    )
  
  # that the pivot worked to get one-row-per-sample.
  if (max(pull(count(rtn, cpt_genie_sample_id, sort = T), n)) > 1) {
    cli::cli_abort("Duplicate gene features after wide pivot")
  }
  
  rtn <- left_join(
    dat_cpt,
    rtn,
    by = c("cpt_genie_sample_id")
  )
  
  if (keep_only_first) {
    rtn %<>%
      group_by(record_id, ca_seq) %>%
      arrange(dx_cpt_rep_yrs) %>%
      slice(1) %>%
      ungroup(.)
  }
  return(rtn)
}
