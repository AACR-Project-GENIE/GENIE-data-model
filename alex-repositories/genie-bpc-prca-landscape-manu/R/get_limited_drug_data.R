get_limited_drug_data <- function(drug_dat, drug_str, warn = T, factorize = T) {
  drug_dat %<>%
    mutate(
      drug = str_replace(drug, "\\(.*\\)", ""),
      # some drugs have whitespace like Radium.  Inconsistency abounds.
      drug = str_trim(drug)
    )
  
  if (any(!(drug_str %in% unique(drug_dat$drug))) & warn) {
    unobserved_drugs <- drug_str[!(drug_str %in% unique(drug_dat$drug))]
    cli::cli_alert_danger(
      glue(
        "Some inputs not found in drug_dat: {paste(unobserved_drugs, collapse = ', ')}"
      )
    )
  }
  
  drug_dat %<>%
    filter(drug %in% drug_str) %>%
    group_by(record_id, ca_seq, regimen_number) %>%
    summarize(
      regimen_drugs = paste0(sort(drug), collapse = ", "),
      .groups = "drop"
    ) 
  
  if (factorize) {
    drug_dat %<>% mutate(regimen_drugs = factor(regimen_drugs)) 
  }
  
  drug_dat %<>% arrange(record_id, ca_seq, regimen_number) 
  
  return(drug_dat)
  
}
