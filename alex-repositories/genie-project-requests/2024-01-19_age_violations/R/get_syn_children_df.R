get_syn_children_df <- function(synid) {
    synGetChildren(synid) %>%
        as.list %>%
        purrr::map_dfr(.x = .,
                       .f = as_tibble)
}
