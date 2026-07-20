library(dplyr)
library(glue)
library(lubridate)
library(magrittr)
library(here)
library(readr)

unclear_cases <- tibble(
  date_birth = "1929-07-20", 
  date_represented = "2015-01-20",
  date_curation = "2020-01-01", 
  date_process = "2023-08-01", 
  date_release = "2023-11-20",
  any_redact_required = F,
  reasoning = glue(
    "There is no observation in the data that is indicative of a person living to 
  age 90 or above.  In other words, it is not relevant that they were curated,
  processed and released after their 90th birthday"
  )
)

unclear_cases %<>%
  add_row(
    date_birth = "1924-07-20", 
    date_represented = "2015-02-28",
    date_curation = "2020-01-07", 
    date_process = "2023-08-01", 
    date_release = "2023-11-20",
    any_redact_required = F,
    reasoning = glue(
      "There is an observed interval after the participant's 90th birthday which needs to be masked."
    )
  )

unclear_cases %<>%
  add_row(
    date_birth = "2005-08-10", 
    date_represented = "2022-07-01",
    date_curation = "2023-09-01", 
    date_process = "2024-11-01", 
    date_release = "2025-06-01",
    any_redact_required = T,
    reasoning = glue(
    "The last observation for the participant was before their 18th birthday which we are required to redact by GENIE standards. Being 
    processed, curated or released after their 18th birthday has no relevance."
    )
  )

unclear_cases %<>%
  add_row(
    date_birth = "2002-08-10", 
    date_represented = "2020-09-01",
    date_curation = "2023-09-15", 
    date_process = "2024-10-10", 
    date_release = "2025-05-01",
    any_redact_required = T,
    reasoning = glue(
      "The last observation for this participant was after their 18th birthday, so all of their data can be released in GENIE (even that occuring before their 18th birthday)."
    )
  )

unclear_cases %<>%
  mutate(
    across(
      .cols = matches("^date"),
      .fns = lubridate::ymd
    )
  ) 

augment_public <- function(dat) {
  dat %<>%
    mutate(
      int_represented_day = time_length(date_represented-date_birth, unit = "day"),
      int_represented_year = time_length(date_represented-date_birth, unit = "year"),
      
      int_curation_day = time_length(date_curation-date_birth, unit = "day"),
      int_curation_year = time_length(date_curation-date_birth, unit = "year"),
      
      int_process_day = time_length(date_process-date_birth, unit = "day"),
      int_process_year = time_length(date_process-date_birth, unit = "year"),
      
      int_release_day = time_length(date_release-date_birth, unit = "day"),
      int_release_year = time_length(date_release-date_birth, unit = "year")
    )
  
  return(dat)
}

unclear_cases %<>%
  augment_public(.)

print_case_bullet_points <- function(dat_row) {
  dat_row %<>%
    mutate(
      across(
        .cols = matches("^date_"),
        .fns = ~ format(., '%b %d, %Y')
      )
    ) %>%
    mutate(
      any_redact_required = if_else(
        any_redact_required,
        "REDACTION REQUIRED",
        "NO REDACTION REQUIRED"
      )
    ) %>%
    mutate(
      across(
        .cols = matches("_year$"),
        .fns = (function(x) {
        formatC(x, digits = 2, format = 'f')
        })
      )
    ) %>%
    mutate(
      across(
        .cols = matches("_day$"),
        .fns = (function(x) {
          formatC(x, digits = 0, format = 'f', big.mark = ',')
        })
      )
    ) %>%
    mutate(across(everything(), as.character)) 
    
  cli::cli_alert_danger("Bleh")

  vec <- tidyr::pivot_longer(dat_row, everything()) %>%
    pull(value, name)
  
  cat(glue(
    "Case: {vec['any_redact_required']}
    Birth date: {vec['date_birth']}
    DATE_REPRESENTED: {vec['date_represented']} ({vec['int_represented_year']} years or {vec['int_represented_day']} days after birth).
    Curation date: {vec['date_curation']} ({vec['int_curation_year']} years or {vec['int_curation_day']} days after birth)
    Processing date:  {vec['date_process']} ({vec['int_process_year']} years or {vec['int_process_day']} days after birth)
    Release date:  {vec['date_release']} ({vec['int_release_year']} years or {vec['int_release_day']} days after birth)
    Reasoning: {vec['reasoning']}"
  ))
  # cat(glue("   - DATE_REPRESENTED: {vec['date_represented']}"))
  
  
  
        
  
}

unclear_cases %>%
  slice(1) %>%
  print_case_bullet_points(.) 
unclear_cases %>%
  slice(2) %>%
  print_case_bullet_points(.) 

unclear_cases %>%
  slice(3) %>%
  print_case_bullet_points(.) 

unclear_cases %>%
  slice(4) %>%
  print_case_bullet_points(.) 






  


