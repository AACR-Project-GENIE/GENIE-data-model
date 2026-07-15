library(fs); library(purrr); library(here);
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)




contains_vars <- function(
    dat,
    vars,
    return_dat = T
) {
  vec <- vars %in% names(dat)
  rtn = tibble(var = vars, included = vec)
  
  if (return_dat) {
    return(rtn)
  } else {
    return(all(vec, na.rm = T))
  }
  
}





dft_cohort_cases <- readr::read_rds(
  here('data', 'cohort', 'clin_dat_wide.rds')
)

dft_cohort_ex_row <- dft_cohort_cases %>% slice(6)

ex_pt <- dft_cohort_ex_row %>% pull(pt) %>% `[[`(.,1)
ex_ca_ind <- dft_cohort_ex_row %>% pull(ca_ind) %>% `[[`(.,1)
ex_hdrug <- dft_cohort_ex_row %>% pull(hdrug) %>% `[[`(.,1)

# Combination option 1:
contains_vars(
  ex_pt, 
  c('birth_date_min')
)
contains_vars(
  ex_hdrug,
  c('drugs_startdt_int')
)
# Ok great, that works.

add_drug_start_range <- function(
    dat_pt,
    dat_drug
) {
  if (!(all(c('birth_date_min', 'birth_date_max') %in% names(dat_pt)))) {
    cli::cli_abort("'birth_date_min' and 'birth_date_max' are required in dat_pt.")
  }
  
  if (!('drugs_startdt_int' %in% names(dat_drug))) {
    cli::cli_abort("'drugs_startdt_int' is required in dat_drug.")
  }
  
  dat_pt_lim <- dat_pt %>%
    select(record_id, birth_date_min, birth_date_max)
  
  rtn <- left_join(
    dat_drug,
    dat_pt_lim,
    by = 'record_id'
  )
  
  rtn %<>%
    mutate(
      drug_start_date_min = birth_date_min + days(drugs_startdt_int),
      drug_start_date_max = birth_date_max + days(drugs_startdt_int)
    )
  
  # Sanity checks
  if (any(rtn$drug_start_date_min >= rtn$drug_start_date_max, na.rm = T)) {
    cli_abort("Error: Drug start min is not less than drug start max.")
  }
  
  return(rtn)
}

add_drug_start_range(
  dat_pt = ex_pt,
  dat_drug = ex_hdrug
)
    
    

contains_vars <- function(
    dat,
    vars,
    return_dat = T
) {
  vec <- vars %in% names(dat)
  rtn = tibble(var = vars, included = vec)
  
  if (return_dat) {
    return(rtn)
  } else {
    return(all(vec, na.rm = T))
  }
  
}
