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
    filter(str_detect(minor, "1[1-9].1-public"))

# Just the last consortium release available too:
dft_releases_p2 <- dft_releases %>%
    group_by(major) %>%
    slice(n()) %>%
    ungroup(.) %>%
    slice(n())

dft_releases <- bind_rows(dft_releases_p1, dft_releases_p2)

dft_releases <- dft_releases %>%
    mutate(
        df_file_ids = purrr::map(.x = minor_id, .f = add_select_file_ids)
    ) %>%
    unnest(df_file_ids)

# These codes are just pulled from the BPC data guide:
breast_included_oncotree <- c(
    'BA', 'BRCA', 'BRCANOS', 'BRCNOS', 'IDC', 'ILC', 'MBC', 'MDLC',
    'MMBC', 'MPT', 'MSCC'
)

breast_excluded_oncotree <- c(
    'BRAME', 'DCIS', 'PD', 'BFN', 'FA', 'PT', 'BPT', 'BLPT', 'LCIS',
    'BNNOS', 'BBNOS', 'PBS', 'IBC', 'ACBC', 'BRSRCC', 'CSNOS', 'IMMC',
    'SPC', 'JSCB'
)

if (length(intersect(breast_included_oncotree, 
                     breast_excluded_oncotree)) > 0) {
    cli_abort("Overlap between the included and excluded oncotree_codes")
}

readr::write_rds(
    list(incl = breast_included_oncotree, excl = breast_excluded_oncotree),
    here('data', 'incl_excl_oncotree.rds')
)

# Example of doing one:
# get_samples_by_oncotree_code(
#     id_clin_samp =  pull(slice(dft_releases, 1), data_clinical_sample.txt),
#     id_clin_pt = pull(slice(dft_releases, 1), data_clinical_patient.txt)
# ) %>%
#     glimpse

    
# Doing them all:
dft_rel_sums <- dft_releases %>%
    # slice(c(1,5,6)) %>% # temporary.
    mutate(
        dat = purrr::map2(
            .x = data_clinical_sample.txt,
            .y = data_clinical_patient.txt,
            .f = \(x,y) {
                get_samples_by_oncotree_code(
                    id_clin_samp = x,
                    id_clin_pt = y,
                    oncotree = breast_included_oncotree
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






