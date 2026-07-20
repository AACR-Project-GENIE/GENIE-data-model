load_all_raw_data_of_type <- function(saved_name) {
  cohort_names <- dir(here('data-raw'))
  vec_dir <- here('data-raw', cohort_names)
  
  rtn <- tibble(
    cohort = cohort_names,
    {{saved_name}} := purrr::map(
      .x = vec_dir,
      .f = (function(p) {
        readr::read_csv(
          file = here(p, paste0(saved_name,".csv")),
          show_col_types = F
          )
        })
    )
  )
  
  return(rtn)
  
}
