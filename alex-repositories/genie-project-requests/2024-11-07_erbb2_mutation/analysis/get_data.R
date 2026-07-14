library(fs); library(purrr); library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

synLogin()

get_synapse_entity_data_in_tsv <- function(synapse_id, 
                                           version = NA) {
    if (is.na(version)) {
        entity <- synGet(synapse_id)
    } else {
        entity <- synGet(synapse_id, version = version)
    }
    
    data <- readr::read_tsv(entity$path,
                            show_col_types = F,
                            progress = F)
    return(data)
}

mut <- get_synapse_entity_data_in_tsv(
    synapse_id = 'syn5571527'
)

clin <- get_synapse_entity_data_in_tsv(
    
    synapse_id = 'syn7392892',
)

readr::write_rds(mut, here('data-raw', 'mut.rds'))
readr::write_rds(clin, here('data-raw', 'clin.rds'))


# Get the BPC data next.

# This code is copied from a multi-cohort project, so it's a bit overkill but
#   functional.
dft_datasets_to_get <- tribble(
    ~synapse_name, ~save_name,
    "cancer_level_dataset_index.csv", "ca_ind",
    "cancer_level_dataset_non_index.csv", "ca_non_ind",
    "cancer_panel_test_level_dataset.csv", "cpt",
    "patient_level_dataset.csv", "pt",
    "regimen_cancer_level_dataset.csv", "reg",
    "imaging_level_dataset.csv", "img'",
    "med_onc_note_level_dataset.csv", "med_onc",
    "pathology_report_level_dataset.csv", "path",
    'ca_radtx_dataset.csv', 'rad'
)

dft_folders <- tibble::tribble(
    ~cohort, ~synid,
    "NSCLC", "syn54107384" # 3.1 consortium
)

dc_help <- function(cohort_name) {
    fs::dir_create(here("data-raw", paste0(cohort_name)))
}
purrr::walk(.x = dft_folders$cohort, .f = dc_help)

dft_datasets <- dft_folders %>%
    mutate(
        children = purrr::map(
            .x = synid,
            .f = (function(id) {
                dat <- get_syn_children_df(id) %>%
                    # only need limited info for this project
                    select(
                        dat_name = name,
                        dat_synid = id
                    ) %>%
                    filter(dat_name %in% dft_datasets_to_get$synapse_name)
                
                dat %<>% left_join(., dft_datasets_to_get,
                                   by = c(dat_name = "synapse_name"))
                return(dat)
            })
        )
    ) %>%
    unnest(children)

get_and_save_dataset <- function(
        synid,
        subfolder) {
    
    synGet(
        entity = synid, 
        downloadLocation = here(
            "data-raw", 
            subfolder
        ),
        ifcollision = "overwrite.local"
    )
}

purrr::pwalk(
    .l = with(
        dft_datasets, 
        list(
            synid = dat_synid, 
            subfolder = cohort
        )
    ),
    .f = get_and_save_dataset
)


    
