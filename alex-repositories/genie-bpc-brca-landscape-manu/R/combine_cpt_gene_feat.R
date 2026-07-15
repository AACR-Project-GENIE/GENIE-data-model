
#' #' @description Combines the data from the clinical curation (CPT dataset)
#' with the gene features from main GENIE.
#' @param dat_gene_feat a "long" gene feature dataframe with columns
#'   cpt_genie_sample_id, feature, value.  Example features are "PTEN_mut" or
#'   "TP53_cna", values should be 0 or 1.
#' @param dat_cpt a subset of the cancer panel test dataset which includes
#'   samples that meet the inclusion criteria for this analysis (varies).
#' @param keep_only_first keeps only the first sample if a participant has more
#'   than one eligible test.
#' @param impute_zero_if_not_in_gene_feat (when true) For any row that exists in
#'   dat_cpt but does not exist in dat_gene_feat, this assumes that no
#'   alterations were found, and therefore a row with all zeroes is filled in.
combine_cpt_gene_feat <- function(
    dat_cpt, 
    dat_gene_feat, 
    keep_only_first = T,
    impute_zero_if_not_in_gene_feat = T) {
  if (max(pull(count(dat_cpt, cpt_genie_sample_id, sort = T), n)) > 1) {
    cli::cli_abort("Duplicate NGS rows in dat_cpt.")
  }
  
  if (impute_zero_if_not_in_gene_feat) {
    impute_samp <- setdiff(
      unique(dat_cpt$cpt_genie_sample_id),
      unique(dat_gene_feat$cpt_genie_sample_id)
    )
    
    impute_feat <- unique(dat_gene_feat$feature)
    
    imputed_rows <- tidyr::expand_grid(
      cpt_genie_sample_id = impute_samp,
      feature = impute_feat
    ) %>%
      mutate(value = 0L)
    
    dat_gene_feat <- bind_rows(
      dat_gene_feat,
      imputed_rows
    )
  }
  
  gene_feat_wide <- dat_gene_feat %>%
    pivot_wider(
      names_from = "feature",
      values_from = "value",
      values_fill = NA
    )
  
  # that the pivot worked to get one-row-per-sample.
  if (max(pull(count(gene_feat_wide, cpt_genie_sample_id, sort = T), n)) > 1) {
    cli::cli_abort("Duplicate gene features after wide pivot")
  }
  
  rtn <- left_join(
    dat_cpt,
    gene_feat_wide,
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
