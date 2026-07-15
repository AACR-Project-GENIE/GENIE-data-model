
folder_load_helper <- function(fold, dn) {
  # put the data names we want to keep into a big or regular expression.
  dn_regex <- paste(dn$synapse_name, collapse = "|")
  
  vec_paths <- fs::dir_ls(fold)
  vec_paths <- vec_paths[str_detect(vec_paths, dn_regex)]
  
  dat_names <- dir(fold)
  dat_names <- dat_names[str_detect(dat_names, dn_regex)]
  
  rtn <- tibble(
    path = vec_paths,
    syn_name = dat_names
  )
  
  rtn %<>%
    mutate(
      dat = purrr::map(
        .x = path,
        .f = (function(p) {
          read_csv(file = p, show_col_types = F) 
        })
      )
    )
  
  rtn %<>% select(syn_name, dat) %>%
    left_join(., dn, by = c(syn_name = "synapse_name"))
  
  return(rtn)
  
}
