summarize_release_wu_brca <- function(
  id_mut, 
  id_clin_comb,
  id_clin_pt,
  id_clin_sample, 
  dat_onco = dft_oncotree) {
  
  dat_mut <- get_synapse_entity_txt(id_mut, skip = 0, cols_to_lower = T)
  
  if (is.na(id_clin_comb)) {
    dat_pt <- get_synapse_entity_txt(id_clin_pt, skip = 4)
    dat_sample <- get_synapse_entity_txt(id_clin_sample, skip = 4)
    dat_clin <- left_join(
      dat_sample,
      dat_pt,
      by = "patient_id"
    )
  } else {
    dat_clin <- get_synapse_entity_txt(id_clin_comb, skip = 0)
  }
  
  dat_mut %<>% 
    select(
      all_of(
        c('tumor_sample_barcode', 
          'hugo_symbol', 
          'mutation_status')
      )
    ) %>%
    mutate(mutation_status = tolower(mutation_status))
  
  dat_clin %<>%
    select(
      all_of(c('sample_id', 'patient_id', 'center', 'oncotree_code')),
      any_of(c('oncotree_primary_node'))
    ) 
  
  # Needed for some older releases:
  if (!("oncotree_primary_node" %in% names(dat_clin))) {
    dat_clin %<>%
      left_join(., dft_oncotree, by = "oncotree_code") %>%
      mutate(tissue = if_else(is.na(tissue), "UNKNOWN", tissue)) %>%
      rename(oncotree_primary_node = tissue)
  }
  
  dat_mut %<>%
    filter(hugo_symbol %in% c("BRCA1", "BRCA2")) %>%
    filter(mutation_status %in% "somatic") %>%
    mutate(variant = 1) %>%
    select(-c(hugo_symbol, mutation_status))
  
  dat_rtn <- left_join(
    dat_clin,
    dat_mut,
    by = c(sample_id = "tumor_sample_barcode")
  )
  
  dat_rtn %<>%
    mutate(variant = if_else(is.na(variant), 0, variant)) %>%
    group_by(center, oncotree_primary_node, patient_id) %>%
    summarize(variant = max(variant, na.rm = T), .groups = "drop") # %>%
  # I decided to drop this chunk so we can use the patient IDs later on,
  #   particularly to fulfill the request to look at which ones are in BPC.
  # group_by(center, oncotree_primary_node) %>%
  # summarize(
  #   n_total = n(),
  #   n_variant = sum(variant),
  #   .groups = "drop"
  # )
  
  
  # Ready for a hideous hack?
  # Every iteration I clear out the synapseCache so I don't run out of memory on
  #   the AWS instance.  This deletes all the cache for other projects, which
  #   I don't have a problem with because I usually save whatever data I need.
  fs::dir_delete("~/.synapseCache")

  
  # Check that oncotree_primary_node does not exist when oncotree code is missing.
  return(dat_rtn)
  
}