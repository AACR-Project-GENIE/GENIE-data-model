
# cdl = cohort data list.  This is a very specific format with the columns
#   {cohort, reg} where reg is a list column of regimen datasets.
# The only point of this function is tracking drugs as we process data.
tracking_hdrug_helper <- function(cdl, dat_name = "hreg") {
  cdl %<>%
    select(.data[[dat_name]]) %>%
    unnest(.data[[dat_name]]) %>%
    select(
      cohort, record_id, ca_seq, regimen_number, 
      matches("^drugs_drug_[1-5]$")
    ) 
  
  # basically we do a quicker (lighter) version of create_drug_dat here.
  cdl %<>%
    pivot_longer(
      cols = matches("^drugs_drug_[1-5]$"),
      names_to = "drug_number",
      values_to = "agent"
    ) %>%
    mutate(
      drug_number = as.numeric(str_sub(drug_number, -1)),
      agent = str_replace(agent, "\\(.*", "")
    ) %>%
    filter(!is.na(agent))
}
  

# This is for use when you already have a built hdrug column.  All we have to 
#   do then is subset the columns and unnest.
tracking_hdrug_subset <- function(cdl) {
  cdl %>%
    select(cohort, hdrug) %>%
    unnest(hdrug) %>%
    select(cohort, record_id, ca_seq, regimen_number, drug_number, agent)
} 
