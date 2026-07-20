#' @title Create a drug dataframe from a (harmonized) regimen dataframe
#' @param dat_reg A regimen dataframe using the most recent GENIE BPC encoding (Bladder)
create_drug_dat <- function(dat_reg) {
  
  dat_drug <- dat_reg %>%
    select(
      record_id, ca_seq, regimen_number,
      matches("_[1-5]$"),
      matches("tt_os_d[1-5]_days$")
    ) %>%
    mutate(
      across(
        .cols = -c(record_id, ca_seq, regimen_number),
        .fn = as.character # for now
      ) 
    )
  
  dat_drug %<>%
    pivot_longer(
      cols = -c(record_id, ca_seq, regimen_number)
    ) %>%
    mutate(
      drug_number = readr::parse_number(name),
      # pattern for most variables:
      name = str_replace(name, "_[1-5]$", ""),
      # pattern for the tt_os_d[#]_days variables:
      name = str_replace(name, "^tt_os_d[1-5]_days$", "tt_os_days")
    ) %>%
    pivot_wider(
      names_from = name,
      values_from = value
    )
  
  dat_drug %<>% 
    rename(agent = drugs_drug) 
  
  dat_drug %<>%
    filter(!is.na(agent)) %>%
    mutate(
      # agent = str_replace(agent, "\\(.*\\)$", "")
      # The above is the correct regex, but there's one truncated synonym list
      #   which is too long, so we never see the ')' character.  Instead:
      agent = str_replace(agent, "\\(.*", "")
    )
  
  dat_drug %<>%
    mutate(
      across(
        .cols = matches("_int|_days$"),
        .fns = as.numeric
      )
    )
  
  return(dat_drug)
}