
#' #' @description Combines the data from the clinical curation (CPT dataset)
#' with the gene features from main GENIE.  This version operates on the 'wide'
#' version of the gene features dataset, which is more natural from the
#' bioinformatics data conventions anyway.
#' @param dat_gene_feat a "wide" gene feature dataframe with columns
#'   cpt_genie_sample_id, and any gene features such as "PTEN_mut" or
#'   "TP53_cna".
#' @param dat_cpt a subset of the cancer panel test dataset which includes
#'   samples that meet the inclusion criteria for this analysis (varies).
#' @param keep_only_first keeps only the first sample if a participant has more
#'   than one eligible test.
#' @param impute_zero_if_not_in_gene_feat (when true) For any row that exists in
#'   dat_cpt but does not exist in dat_gene_feat, this assumes that no
#'   alterations were found, and therefore a row with all zeroes is filled in.
combine_cpt_gene_feat_wide <- function(
    dat_cpt, 
    dat_gene_feat_wide, 
    keep_only_first = T,
    impute_zero_if_not_in_gene_feat = T) {
  if (max(pull(count(dat_cpt, cpt_genie_sample_id, sort = T), n)) > 1) {
    cli::cli_abort("Duplicate NGS rows in dat_cpt.")
  }

  # that the pivot worked to get one-row-per-sample.
  if (max(pull(count(dat_gene_feat_wide, cpt_genie_sample_id, sort = T), n)) > 1) {
    cli::cli_abort("Duplicate gene features in dat_gene_feat_wide.")
  }
  
  rtn <- left_join(
    dat_cpt,
    dat_gene_feat_wide,
    by = c("cpt_genie_sample_id"),
    relationship = "one-to-one"
  )
  
  if (keep_only_first) {
    rtn %<>%
      group_by(record_id, ca_seq) %>%
      arrange(dx_cpt_rep_yrs) %>%
      slice(1) %>%
      ungroup(.)
  }
  
  if (impute_zero_if_not_in_gene_feat) {
    gene_feat_names <- dat_gene_feat_wide %>%
      select(-cpt_genie_sample_id) %>%
      colnames(.)
    
    rtn %<>%
      mutate(
        across(
          .cols = all_of(gene_feat_names),
          .fns = (function(x) if_else(is.na(x), 0L, x))
        )
      )
  }
    
  return(rtn)
}
