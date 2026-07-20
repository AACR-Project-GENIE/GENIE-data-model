#' @title Filter Data List by Progression Timing
rename_dl_for_sunburst <- function(d_list) {
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
    as_tibble
  
  cohort_list[["cohort_ca_dx"]] <- data_list_exposed[["ca_dx_index"]] %>%
    as_tibble(.)
  
  cohort_list[["cohort_ca_drugs"]] <- 
    data_list_exposed[["ca_drugs"]] %>%
    as_tibble(.)
  
  cohort_list[["cohort_ngs"]] <- 
    data_list_exposed[["cpt"]] %>%
    as_tibble(.)
  
  return(cohort_list)
  
}

# filter_dl_by_pt(data_list,
#                 pt_pfs_i_or_m) %>%
#   lobstr::tree(., max_depth = 2)