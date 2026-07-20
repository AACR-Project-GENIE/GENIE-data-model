get_syn_children_df <- function(sid) {
    synGetChildren(sid) %>%
        as.list %>%
        purrr::map_dfr(
            .x = .,
            .f = as_tibble
        ) %>%
        select(name, id, createdOn)
}