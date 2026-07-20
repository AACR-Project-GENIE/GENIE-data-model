# Description:  See https://sagebionetworks.jira.com/browse/GEN-768
#   Note:  This file is very inefficient computationally.  Lots of data frame
#   lists, lots of room for optimization.
# Author: Alex Paynter

library(here); library(purrr); library(fs)
purrr::walk(fs::dir_ls(here('R')), .f = source)

synid_releases <- "syn7492881"


synLogin()


tic()


dft_releases <- get_syn_children_df(synid_releases)

# Recurse to subfolders:
dft_releases %<>%
    rename(major = name) %>%
    mutate(
        child_df = purrr::map(.x = id, .f = get_syn_children_df)
    ) %>%
    select(-c(id, createdOn)) %>%
    unnest(child_df) %>%
    rename(
        minor = name,
        minor_id = id,
        minor_createdOn = createdOn
    )

# Limit to releases in the request.
# The public releases requested:
dft_releases_p1 <- dft_releases %>%
    filter(str_detect(minor, "1[1-5].1-public"))

# The consort releases that may help projecting:
dft_releases_p2 <- dft_releases %>%
    filter(str_detect(major, "Release 1[6]")) %>%
    group_by(major) %>%
    slice(n()) %>%
    ungroup(.)

dft_releases <- bind_rows(dft_releases_p1, dft_releases_p2)
    
dft_releases <- dft_releases %>%
    mutate(
        df_file_ids = purrr::map(.x = minor_id, .f = add_select_file_ids)
    ) %>%
    unnest(df_file_ids)




# exact matches:
nsclc_oncotree_codes <- mskcc.oncotree::get_tumor_types() %>%
    filter(oncotree_main_type %in% "Non-Small Cell Lung Cancer") %>% 
    pull(oncotree_code)

# Example of doing one:
# sum_rtn <- summarize_release_egfr_arpah(
#     id_mut = pull(slice(dft_releases, 11), data_mutations_extended.txt),
#     #id_clin_comb = NA,
#     id_clin_sample =  pull(slice(dft_releases, 11), data_clinical_sample.txt),
# #    id_clin_pt = pull(slice(dft_releases, 11), data_clinical_patient.txt),
#     protein_symbols = mut_prot_symbols,
#     samp_oncotree_codes = nsclc_oncotree_codes
# )

get_clin_helper <- function(synid, skip = 0) {
    dat_clin <- get_synapse_entity_txt(synid, skip = skip)
    return(dat_clin)
} 

# Doing them all:
dft_rel_sums <- dft_releases %>%
    mutate(
        release_pt = purrr::map(
            .x = data_clinical_patient.txt,
            .f = \(x) get_clin_helper(x, skip = 0)
        ),
        release_samp = purrr::map(
            .x = data_clinical_sample.txt,
            .f = \(x) get_clin_helper(x, skip = 4)
        )
    )

dft_rel_sums %<>%
    mutate(
        n_total = purrr::map_dbl(
            .x = release_samp,
            .f = \(x) {
                x %>% count(patient_id) %>% nrow
            }
        ),
        n_nsclc = purrr::map_dbl(
            .x = release_samp,
            .f = \(x) {
                x %>%
                    filter(oncotree_code %in% nsclc_oncotree_codes) %>%
                    count(patient_id) %>%
                    nrow
            }
        )
    )
        
dft_rel_sums %>%
    select(minor, matches("n_"))
        