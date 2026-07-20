library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

bpc_sum <- readr::read_rds(
    here('data', 'bpc_summary.rds')
)

main_sum <- readr::read_rds(
    here('data', 'nsclc_main_genie.rds')
)

# less confusing to me:
main_sum %<>% rename(record_id = patient_id)

# Need the center for each person in the bpc summary - could pull from either place.
bpc_sum <- main_sum %>%
    select(record_id, center) %>%
    distinct %>%
    left_join(
        bpc_sum,
        .,
        by = 'record_id'
    )

step_track <- tibble(
    step = character(0), 
    n_bpc = integer(0), 
    n_main = integer(0)
)

step_center_track <- tibble(
    step = character(0),
    center = character(0),
    n_bpc = integer(0), 
    n_main = integer(0)
)

if (any(!(bpc_sum$record_id %in% main_sum))) {
    cli_alert_warning("There are people in BPC but not main GENIE - investigate.")
}


# Two functions to help me track each step:
add_step_row <- function(step_df, step_name, bpc, main) {
    step_df %>%
        add_row(
            step = step_name,
            n_bpc = length(unique(pull(bpc, record_id))),
            n_main = length(unique(pull(main, record_id)))
        )
}
center_step_counter <- function(step_center_df, step_name, bpc, main) {
    new_step <- full_join(
        (bpc %>% group_by(center) %>% 
             summarize(n_bpc = length(unique(record_id)))),
        (main %>% group_by(center) %>% 
             summarize(n_main = length(unique(record_id)))),
        by = 'center'
    )
    new_step %<>% mutate(step = step_name)
    
    rtn <- bind_rows(step_center_df, new_step)
    
        
}

step_track <- add_step_row(step_track, bpc = bpc_sum, main = main_sum,
                           step_name = "NSCLC")
step_center_track <- center_step_counter(
    step_center_track, bpc = bpc_sum, main = main_sum,
    step_name = "NSCLC")


main_sum %<>% filter(any_erbb2)
bpc_sum %<>% filter(record_id %in% main_sum$record_id)
step_track <- add_step_row(step_track, bpc = bpc_sum, main = main_sum,
                           step_name = "Any ERBB2 mut.")
step_center_track <- center_step_counter(
    step_center_track, bpc = bpc_sum, main = main_sum,
    step_name = "Any ERBB2 mut.")




main_sum %<>% filter(act_erbb2)
bpc_sum %<>% filter(record_id %in% main_sum$record_id)
step_track <- add_step_row(step_track, bpc = bpc_sum, main = main_sum,
                           step_name = "Activating ERBB2")
step_center_track <- center_step_counter(
    step_center_track, bpc = bpc_sum, main = main_sum,
    step_name = "Activating ERBB2")





# At this point we can trim down to one record per patient, we don't need
#   the sample-level info from main genie anymore.
main_sum %<>% group_by(record_id) %>% arrange(seq_year) %>% slice(1) %>% ungroup(.)
# Now we'll use our fancy center projection function to randomly select
#   main GENIE cases based on the BPC numbers:
main_sum <- center_projection(
    bpc_sum, 
    main_sum, 
    var = 'stage_3b_dx_or_met'
)
bpc_sum %<>% filter(stage_3b_dx_or_met)
step_track <- add_step_row(
    step_track, bpc = bpc_sum, main = main_sum,
    step_name = "3B+ at dx or met anytime"
)
step_center_track <- center_step_counter(
    step_center_track, bpc = bpc_sum, main = main_sum,
    step_name = "3B+ at dx or met anytime"
)


main_sum <- center_projection(
    bpc_sum, 
    main_sum, 
    var = 'only_lung_prostate_cancers'
)
bpc_sum %<>% filter(only_lung_prostate_cancers)
step_track <- add_step_row(
    step_track, bpc = bpc_sum, main = main_sum,
    step_name = "Only lung/prostate cancers"
)
step_center_track <- center_step_counter(
    step_center_track, bpc = bpc_sum, main = main_sum,
    step_name = "Only lung/prostate cancers"
)



main_sum <- center_projection(
    bpc_sum, 
    main_sum, 
    var = 'only_lung_cancers'
)
bpc_sum %<>% filter(only_lung_cancers)
step_track <- add_step_row(
    step_track, bpc = bpc_sum, main = main_sum,
    step_name = "Only lung cancers"
)
step_center_track <- center_step_counter(
    step_center_track, bpc = bpc_sum, main = main_sum,
    step_name = "Only lung cancers"
)

main_sum <- center_projection(
    bpc_sum, 
    main_sum, 
    var = 'only_one_cancer'
)
bpc_sum %<>% filter(only_one_cancer)
step_track <- add_step_row(
    step_track, bpc = bpc_sum, main = main_sum,
    step_name = "Only one lung cancer"
)
step_center_track <- center_step_counter(
    step_center_track, bpc = bpc_sum, main = main_sum,
    step_name = "Only one lung cancer"
)


# Cleanup
step_track %<>% mutate(step = fct_inorder(step))
step_center_track %<>%
    mutate(
        step = factor(step, levels = levels(step_track$step)),
        center = factor(center)
    ) %>%
    complete(step, center) %>%
    replace_na(list(n_bpc = 0, n_main = 0))

step_track %<>% replace_na(list(n_bpc = 0, n_main = 0))

step_track %<>% mutate(n_curate = n_main - n_bpc)
step_center_track %<>% mutate(n_curate = n_main - n_bpc) 


# Save
readr::write_rds(
    step_track,
    here('data', 'attrition.rds')
)
readr::write_rds(
    step_center_track,
    here('data', 'attrition_center.rds')
)   

