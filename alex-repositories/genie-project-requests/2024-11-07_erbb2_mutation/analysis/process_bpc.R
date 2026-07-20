library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

ca_ind <- readr::read_csv(
    here('data-raw', 'NSCLC', 'cancer_level_dataset_index.csv')
)

ca_non_ind <- readr::read_csv(
    here('data-raw', 'NSCLC', 'cancer_level_dataset_non_index.csv')
)

# this list is inductive - we just classified the codes which existed.
tr_stage_map <- tribble(
    ~best_ajcc_stage_cd, ~tr_stage,
    '1A', '1A',
    '1A1', '1A',
    '1A2', '1A',
    '1A3', '1A',
    '1B', '1B',
    '2A', '2A',
    '2B', '2B',
    '3A', '3A',
    '3A/B', '3A', # one person.
    '3B', '3B',
    'IIIB', '3B',
    '3C', '3C',
    '4', '4A', # classify as lowest possible stage.
    '4A', '4A',
    '4B', '4B',
    '88', NA_character_,
    '99', NA_character_
)
    
ca_ind %<>%
    left_join(
        ., tr_stage_map, by = 'best_ajcc_stage_cd'
    )

ca_ind %<>% 
    mutate(
        stage_3b_dx = case_when(
            stage_dx %in% "Stage IV" ~ T,
            stage_dx %in% paste0("Stage ", c("I", "II")) ~ F,
            # remaining cases are stage 3, 1-3 NOS, and missing.
            tr_stage %in% c("3B", "3C", "4A", "4B") ~ T,
            T ~ F
        )
    )

# For the summary of 3b+, we'll look at just the first cancer for each person.
# I can't imagine they would be terribly interested in later ones.
bpc_sum <- ca_ind %>%
    group_by(record_id) %>%
    arrange(ca_seq) %>%
    summarize(
        only_one_cancer = all(ca_seq %in% 0),
        any_cancer_3b_dx = any(stage_3b_dx),
        first_cancer_3b_dx = first(stage_3b_dx),
        .groups = 'drop'
    )

met_sum <- get_dmet_time(ca_ind) %>%
    mutate(ever_met = T) %>%
    group_by(record_id) %>%
    # I check that that this works - people don't have one met cancer and another
    #   not in BPC.
    summarize(
        ever_met = first(ever_met),
        dx_dmet_yrs_first_cancer = min(dx_dmet_yrs, na.rm = T),
        .groups = 'drop'
    ) 

bpc_sum <- left_join(
    bpc_sum,
    met_sum,
    by = 'record_id'
) %>%
    replace_na(list(ever_met = F))

bpc_sum %<>%
    mutate(
        stage_3b_dx_or_met = first_cancer_3b_dx | ever_met
    )

index_ca_types <- unique(pull(ca_ind, ca_type))
# Note: At the time of this writing this means that small cell lung cancer
#   counts as an index cancer.  I don't know why that happened but that's
#   what we've got in the data.

ca_types_sum <- bind_rows(
    select(ca_ind, record_id, ca_seq, ca_type),
    select(ca_non_ind, record_id, ca_seq, ca_type)
) %>%
    group_by(record_id) %>%
    summarize(
        only_lung_prostate_cancers = all(
            ca_type %in% c(index_ca_types, "Prostate Cancer")
        ),
        only_lung_cancers = all(
            ca_type %in% index_ca_types
        )
    )
    
    
bpc_sum <- left_join(
    bpc_sum,
    ca_types_sum,
    by = 'record_id'
)

bpc_sum %<>%
    relocate(
        only_one_cancer, .after = only_lung_cancers
    )

readr::write_rds(
    bpc_sum,
    here('data', 'bpc_summary.rds')
)





        
    



            
            
