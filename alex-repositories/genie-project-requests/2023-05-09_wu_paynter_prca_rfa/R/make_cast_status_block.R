# Create blocks of time where participants were identified as CRPC, HRPC,
#   or when no determination had yet been made.
make_cast_status_block <- function(med_onc_dat, 
                                   ca_ind_dat,
                                   remove_hspc_after_crpc = T) {
  dft_cast_blocks <- med_onc_dat %>%
    select(record_id, dx_md_visit_yrs, md_pca_status) %>%
    filter(str_detect(md_pca_status, "(CRPC)|(HSPC)")) %>%
    # keep the first row or any row where the designation flips.
    group_by(record_id) %>%
    filter(1:n() == 1 | lag(md_pca_status) != md_pca_status) %>%
    ungroup() %>%
    left_join(
      ., 
      select(ca_ind_dat, record_id, os_dx_status, tt_os_dx_yrs),
      by = "record_id"
    )
  
  
  # If you want to ignore declarations of hormonse sensitive prostate cancer
  #   which occur after a declaration of castrate-resistant prostate cancer:
  if (remove_hspc_after_crpc) {
    dft_cast_blocks %<>%
      group_by(record_id) %>%
      mutate(
        cast_resist = str_detect(md_pca_status, "Castrate-Resistant"),
        cast_resist_sum = cumsum(cast_resist)
      ) %>%
      # Stop once you hit the first castration resistant block.
      filter(cast_resist_sum <= 1 & (1:n() == 1 | lag(cast_resist_sum) <= 0)) %>%
      select(-cast_resist_sum, -cast_resist) %>%
      ungroup()
  }
  
  
  dft_cast_blocks %<>%
    group_by(record_id) %>%
    mutate(dx_block_end = case_when(
      # if it's the last row, the block ends at date of death.
      1:n() == n() ~ tt_os_dx_yrs,
      T ~ lead(dx_md_visit_yrs) # otherwise ends at next block.
    )) %>%
    ungroup()
  
  first_block <- dft_cast_blocks %>%
    group_by(record_id) %>%
    arrange(dx_md_visit_yrs) %>%
    slice(1) %>%
    select(record_id, dx_block_end = dx_md_visit_yrs) %>%
    left_join(
      (ca_ind_dat %>% select(record_id, os_dx_status, tt_os_dx_yrs)),
      ., 
      by = "record_id"
    ) %>%
    # for those who had no declare prostate cancer blocks, 
    # they're always unknown
    mutate(
      dx_md_visit_yrs = 0,
      dx_block_end = if_else(is.na(dx_block_end),
                             tt_os_dx_yrs,
                             dx_block_end),
      md_pca_status = "Not yet declared"
    ) 
  
  dft_cast_blocks <- bind_rows(
    first_block,
    dft_cast_blocks
  ) %>%
    arrange(record_id, dx_md_visit_yrs)
  
  
  # Just touch ups and style prefrences from here on:
  dft_cast_blocks %<>%
    # This variable is altered, so we should change the name:
    rename(md_cast_status = md_pca_status) %>%
    mutate(
      md_cast_status = str_replace_all(
        md_cast_status,
        " Prostate Cancer.*",
        ""
      )
    ) %>%
    arrange(desc(2*str_detect(md_cast_status,"Not yet") +
                   1*str_detect(md_cast_status, "Hormone Sens"))) %>%
    mutate(md_cast_status_f = forcats::fct_inorder(md_cast_status)) %>%
    mutate(os_dx_status_f = factor(if_else(os_dx_status %in% 1,
                                           "Death",
                                           "Censored",
                                           NA_character_)))
  
  dft_cast_blocks %<>%
    select(
      record_id,
      md_cast_status_f,
      dx_block_start = dx_md_visit_yrs,
      dx_block_end,
      os_dx_status_f,
      tt_os_dx_yrs,
      md_cast_status,
      os_dx_status
    )
  
  return(dft_cast_blocks)
}
