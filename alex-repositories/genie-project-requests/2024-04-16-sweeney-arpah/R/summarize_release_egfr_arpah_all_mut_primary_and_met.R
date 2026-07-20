
summarize_release_egfr_arpah_all_mut_primary_and_met <- function(
        id_mut, 
        id_clin_sample,
        samp_oncotree_codes) {
    
    dat_mut <- get_synapse_entity_txt(id_mut, skip = 0, cols_to_lower = T)
    
    if (is.na(id_clin_sample)) {
        # dat_pt <- get_synapse_entity_txt(id_clin_pt, skip = 4)
        # dat_sample <- get_synapse_entity_txt(id_clin_sample, skip = 4)
        # dat_clin <- left_join(
        #     dat_sample,
        #     dat_pt,
        #     by = "patient_id"
        # )
    } else {
        # dat_clin <- get_synapse_entity_txt(id_clin_comb, skip = 0)
        dat_sample <- get_synapse_entity_txt(id_clin_sample, skip = 4)
        
    }
    
    dat_mut %<>%
        select(
            all_of(
                c('tumor_sample_barcode',
                  'consequence',
                  'exon_number',
                  'hugo_symbol',
                  'hgvsp_short',
                  'hgvsc',
                  'mutation_status')
            )
        ) %>%
        mutate(mutation_status = tolower(mutation_status))
    
    dat_mut %<>%
        filter(hugo_symbol %in% "EGFR") %>%
        # select only non-synonymous variants:
        filter(!(consequence %in% "synonymous_variant"))
    
    dat_sample %<>%
        filter(oncotree_code %in% samp_oncotree_codes) %>%
        select(patient_id, sample_id, oncotree_code, sample_type, cancer_type_detailed)
    
    dat_rtn <- dat_mut %>%
        rename(sample_id = tumor_sample_barcode) %>%
        filter(sample_id %in% dat_sample$sample_id) 
    
    dat_rtn <- left_join(
        dat_rtn,
        dat_sample,
        by = "sample_id"
    )
    
    dat_rtn %<>%
        mutate(
            institution = map_chr(
                .x = str_split(sample_id, pattern = "-"), 
                .f = (\(x) x[2])
            )
        ) 
    
    dat_rtn %<>%
        select(
            # primary keys of return:
            sample_id, hgvsp_short,
            patient_id, institution,
            everything()
        )
        
    # Ready for a hideous hack?
    # Every iteration I clear out the synapseCache so I don't run out of memory on
    #   the AWS instance.  This deletes all the cache for other projects, which
    #   I don't have a problem with because I usually save whatever data I need.
    fs::dir_delete("~/.synapseCache")
    
    return(dat_rtn)



}
