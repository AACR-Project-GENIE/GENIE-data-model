add_select_file_ids <- function(id) {
  child_df <- get_syn_children_df(id) 
  
  valid_file_names <- c(
    "data_clinical.txt",
    "data_clinical_patient.txt",
    "data_clinical_sample.txt",
    "data_mutations_extended.txt"
  )
  
  child_df %>% 
    filter(name %in% valid_file_names) %>%
    select(name, id) %>%
    tidyr::pivot_wider(
      names_from = name,
      values_from = id
    ) 
  
}