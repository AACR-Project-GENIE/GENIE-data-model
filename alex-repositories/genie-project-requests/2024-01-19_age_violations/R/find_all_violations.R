find_all_violations <- function(
        bpc_dat,
        vio_dat, 
        bpc_key = "record_id",
        return_type = "violations"
) {
    vec_var <- intersect(
        vio_dat$var,
        names(bpc_dat)
    )
    
    dat_vio <- purrr::map_dfr(
        .x = vec_var,
        .f = \(x) {
            find_violations(
                var_to_check = x,
                bpc_dat = bpc_dat,
                vio_dat = vio_dat,
                bpc_key = bpc_key,
                return_type = return_type
            )
        }
    )
    
    return(dat_vio)
    
    
}
