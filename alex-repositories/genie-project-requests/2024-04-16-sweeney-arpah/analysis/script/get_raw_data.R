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
    filter(str_detect(major, "Release 1[1-6]")) %>%
    group_by(major) %>%
    slice(n()) %>%
    ungroup(.)

dft_releases <- bind_rows(dft_releases_p1, dft_releases_p2)
    
dft_releases <- dft_releases %>%
    mutate(
        df_file_ids = purrr::map(.x = minor_id, .f = add_select_file_ids)
    ) %>%
    unnest(df_file_ids)




# not found: A763_Y764insFQEA, S768I, G719, 729_761del\
# regex style:
mut_prot_symbols <- c("729_761del",
    "G719.*",
    "L858R",
    "L861Q",
    "S768I",
    "T790M",
    "A763_Y764insFQEA")

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

# Doing them all:
dft_rel_sums <- dft_releases %>%
    # slice(c(1,5,6)) %>% # temporary.
    mutate(
        release_sum = purrr::map2(
            .x = data_mutations_extended.txt,
            .y = data_clinical_sample.txt,
            .f = \(x,y) {
                summarize_release_egfr_arpah(
                    id_mut = x,
                    id_clin_sample = y,
                    protein_symbols = mut_prot_symbols,
                    samp_oncotree_codes = nsclc_oncotree_codes
                )
            }
        )
    )



# Just checking to see if this is more efficient (probably)
readr::write_rds(
    x = dft_rel_sums,
    file = here('data', 'release_sum_nested.rds')
)

toc()

# This would be a reasonable next step, but it's way less efficient to store this way:
# dft_rel_sums %<>% unnest(release_sum)




