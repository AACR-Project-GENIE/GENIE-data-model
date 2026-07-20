mesh_list_to_df <- function(str, split_semicolon_terms = T, pull_major_terms = T) {
  vec <- strsplit(str, split = '\\.') %>%
    unlist(.) %>%
    stringr::str_trim(.)
  
  if (split_semicolon_terms) {
    vec <- map(
      .x = vec,
      .f = mesh_semicolon_split
    ) %>% unlist
  }
  
  tib = tibble(mesh = vec)
  
  if (pull_major_terms) {
    tib <- tib %>%
      mutate(major = str_detect(mesh, '\\*')) %>%
      mutate(mesh = str_replace_all(mesh, '\\*', '')) %>%
      select(mesh, major)
  }
  
  return(tib)
  
  
}

mesh_semicolon_split <- function(str) {
  
    last_block <- str_extract(str, "\\/.*$") %>%
      str_sub(., 2) # one for the space, one for the slash.
    
    all_but_last_block <- str_replace_all(str, "\\/.*$", "") %>%
      paste0(., "/") # put the slash back
    
    rtn <- str_split(last_block, ';') %>% unlist
    rtn <- paste0(all_but_last_block, rtn)
    rtn <- str_replace_all(rtn, "\\/NA", "") # when there are no slashes this happens.
    return(rtn)
    
}

# mesh_semicolon_split("STOMACH NEOPLASMS / DRUG THERAPY; GENETICS; IMMUNOLOGY; PATHOLOGY") # real example
# mesh_semicolon_split("STOMACH NEOPLASMS / DRUG THERAPY") # 
# mesh_semicolon_split("STOMACH NEOPLASMS / DRUG THERAPY / GENETICS / IMMUNOLOGY; PATHOLOGY")


map(
  .x = c("STOMACH NEOPLASMS",
         "STOMACH NEOPLASMS / DRUG THERAPY; GENETICS; IMMUNOLOGY; PATHOLOGY"),
  .f = mesh_semicolon_split
) %>% unlist
