

load_all_data_of_type <- function(saved_name) {
  cohort_names <- dir(here('data', 'cohort'))
  vec_dir <- here('data', 'cohort', cohort_names)
  
  rtn <- tibble(
    cohort = cohort_names,
    {{saved_name}} := purrr::map(
      .x = vec_dir,
      .f = (function(p) readr::read_rds(here(p, paste0(saved_name,".rds"))))
    )
  )
  
  return(rtn)
  
}
