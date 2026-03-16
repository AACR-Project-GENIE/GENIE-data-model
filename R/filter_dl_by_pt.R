#' @title Filter Data List by Progression Timing
filter_dl_by_pt <- function(d_list,
                            prog_timing) {
  # filter down to only the data you need to create plots of drug use.
  cohort_name <- names(d_list) 
  
  if (length(cohort_name) > 1) abort("Only designed for one cohort.")
  
  cohort_list <- list()
  
  # filter
  data_list_exposed <- d_list[[cohort_name]]
  data_list_exposed <- data_list_exposed[c("pt_char",
                                           "ca_dx_index",
                                           "ca_drugs",
                                           "cpt")]
  cohort_list[["cohort_pt_char"]] <- data_list_exposed[["pt_char"]] %>%
    filter(record_id %in% prog_timing$record_id) %>%
    as_tibble
  
  cohort_list[["cohort_ca_dx"]] <- data_list_exposed[["ca_dx_index"]] %>%
    filter(record_id %in% prog_timing$record_id) %>%
    as_tibble
  
  
  
  cohort_list[["cohort_ca_drugs"]] <- left_join(
    data_list_exposed[["ca_drugs"]],
    select(prog_timing, all_of(c("record_id", "tt_d"))),
    by = "record_id"
  ) %>%
    as_tibble %>%
    filter(record_id %in% prog_timing$record_id) %>%
    # only regimens that started after the time specified.
    filter(dx_reg_start_int >= tt_d) %>%
    # Important:  This has to be turned into a character vector
    #   to get the legends to work appropriately on the sunburst plot.
    mutate(regimen_drugs = as.character(regimen_drugs)) %>%
    select(-any_of("tt_d")) 
  
  
  
  
  
  
  cohort_list[["cohort_ngs"]] <- left_join(
    data_list_exposed[["cpt"]],
    select(prog_timing, all_of(c("record_id", "tt_d"))),
    by = "record_id"
  ) %>%
    filter(record_id %in% prog_timing$record_id) %>%
    # only cancer panel tests that started after the time specified.
    filter(dx_cpt_rep_days >= tt_d) %>%
    select(-any_of("tt_d")) %>%
    as_tibble
  
  return(cohort_list)
  
}

# filter_dl_by_pt(data_list,
#                 pt_pfs_i_or_m) %>%
#   lobstr::tree(., max_depth = 2)