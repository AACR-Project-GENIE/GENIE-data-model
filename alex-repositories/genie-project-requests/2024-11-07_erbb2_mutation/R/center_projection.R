
center_projection <- function(
        dat_bpc,
        dat_main,
        var,
        print_props = T
) {
    # stack the bpc data in three chunks:  DFCI, MSK and everyone together (including the DFCI and MSK people).
    
    dat_bpc_stack <- bind_rows(
        (dat_bpc %>% filter(center %in% c("DFCI", "MSK"))),
        (dat_bpc %>% mutate(center = "All")) 
    ) %>%
        # .center is DFCI/MSK/All version.
        rename(.center = center)
    
    props <- dat_bpc_stack %>%
        group_by(.center) %>%
        summarize(.prop = mean(.data[[var]]))
    
    if (print_props) {
        props %>% print(n = 500)
    }
    
    
    dat_main %<>%
        mutate(.center = case_when(
            center %in% c("DFCI", "MSK") ~ center,
            T ~ "All"
        )) %>%
        left_join(
            ., props, by = '.center'
        ) 
    
    dat_main %<>%
        group_by(.center) %>%
        mutate(
            # The number who should remain after sampling
            .n_remain = round(n()*.prop),
            .remain_draw = sample(
                x = c(rep(1, first(.n_remain)), rep(0, n()-first(.n_remain))),
                size = n()
            ),
            .remain_draw = as.logical(.remain_draw)
        ) %>%
        ungroup(.) %>%
        filter(.remain_draw)
    
    dat_main %<>% select(-matches("^\\."))
    
    return(dat_main)
    
    # randomly pick .prop portion of people to stay.
    
}