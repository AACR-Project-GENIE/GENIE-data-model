
find_violations <- function(
        bpc_dat,
        vio_dat,
        var_to_check,
        bpc_key = "record_id",
        return_type = "violations"
) {
    bpc_sub <- bpc_dat %>%
        select(
            all_of(bpc_key), 
            .var_value = all_of(var_to_check)
        ) %>%
        mutate(var = var_to_check)
    
    vio_sub <- vio_dat %>%
        filter(var %in% var_to_check)
    
    potential_vio <- left_join(
        bpc_sub,
        vio_sub,
        by = "var",
        relationship = 'many-to-many'
    )
    
    potential_vio %<>%
        mutate(
            violation = case_when(
                is.na(.var_value) ~ F,
                .var_value >= lb & .var_value < ub ~ T,
                T ~ F
            )
        )
    
    if (return_type %in% "all") {
        return(potential_vio)
    } else if (return_type %in% "violations") {
        return(filter(potential_vio, violation)) 
    } else {
        cli_abort("Invalid return type")
    }
}