
# oncotree = NULL returns all the samples with no oncotree filter.
get_samples_by_oncotree_code <- function(
        id_clin_samp,
        id_clin_pt,
        oncotree = NULL
) {
    dat_sample <- get_synapse_entity_txt(id_clin_samp, skip = 4)
    dat_pt <- get_synapse_entity_txt(id_clin_pt, skip = 4)
    
    rtn <- left_join(
        dat_sample,
        dat_pt,
        by = "patient_id"
    ) %>%
        # makes no sense to have the primary key second.
        select(sample_id, everything())
    
    if (is.null(oncotree)) {
        return(as_tibble(rtn))
    } else {
        rtn %>%
            filter(oncotree_code %in% oncotree) %>%
            as_tibble(.) %>%
            return(.)
        
    }
}