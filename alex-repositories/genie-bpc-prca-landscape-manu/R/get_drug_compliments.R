

# A function that finds the top k drugs in regimens that use drug_name.
get_drug_compliments <- function(drug_dat, drug_name) {
  reg_with_drug <- drug_dat %>%
    filter(drug %in% drug_name) %>%
    select(record_id, regimen_number) %>%
    distinct(.)
  
  n_reg <- nrow(reg_with_drug)
  
  top_drug_compliments <- left_join(
    reg_with_drug, 
    drug_dat,
    by = c("record_id", "regimen_number")
  ) %>%
    filter(!(drug %in% drug_name)) %>%
    group_by(drug) %>%
    summarize(n = n(), .groups = "drop")
  
  top_drug_compliments %<>%
    arrange(desc(n)) %>%
    mutate(n_reg = n_reg,
           prop = n/n_reg)
  
  return(top_drug_compliments)
  
} 
