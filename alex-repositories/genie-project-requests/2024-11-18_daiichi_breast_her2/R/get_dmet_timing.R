
#' @title Get the timing of distant metastases
#'
#' @param ca_ind_df The cancer index PRISSMM dataset.
get_dmet_timing <- function(ca_ind_df) {
    stage_iv_df <- ca_ind_df %>%
        filter(stage_dx_iv %in% "Stage IV") %>%
        # Note: There are some people with stage IV but no dmets.
        filter(ca_dmets_yn %in% "Yes") %>%
        select(record_id, 
               ca_seq) %>%
        mutate(tt_y = 0)
    
    early_stage_df <- ca_ind_df %>%
        filter(!(stage_dx_iv %in% c("Stage IV"))) %>%
        filter(dmets_stage_i_iii %in% 1) %>%
        select(record_id,
               ca_seq,
               tt_y = dx_to_dmets_yrs)
    
    if (any(is.na(early_stage_df$tt_y))) {
        cli::cli_inform("Some missing times in Stage 0-3 group.")
    }
    
    rtn_df <- bind_rows(
        stage_iv_df,
        early_stage_df
    ) %>%
        mutate(
            # both of these seem to be empirically true in PRISSMM:
            tt_m = tt_y * 12.0148,
            tt_d = tt_y * 365.25
        )
    
    return(rtn_df)
    
}
