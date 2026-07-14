# This function just prints a bunch of information about each release, returns nothing.  It's a mess, an intentional mess.

explore_release_data <- function(
  id_mut, 
  id_clin_comb,
  id_clin_pt,
  id_clin_sample, 
  dat_onco = dft_oncotree) {

  dat_mut <- get_synapse_entity_txt(id_mut, skip = 0, cols_to_lower = T)
  
  if (is.na(id_clin_comb)) {
    dat_pt <- get_synapse_entity_txt(id_clin_pt, skip = 4)
    dat_sample <- get_synapse_entity_txt(id_clin_sample, skip = 4)
    dat_clin <- left_join(
      dat_sample,
      dat_pt,
      by = "patient_id"
    )
  } else {
    dat_clin <- get_synapse_entity_txt(id_clin_comb, skip = 0)
  }
  
  cli::cli_alert_info("JHU panels used")
  dat_clin %>% 
    names %>%
    paste(., collapse = ", ") %>%
    print
  dat_clin %>%
    filter(str_detect(sample_id, "GENIE-JHU-")) %>%
    tabyl(., seq_assay_id) %>%
    print
  
  print(glue("Number of rows in clinical data: {nrow(dat_clin)}."))
  print(glue("Number of rows in mutation data: {nrow(dat_mut)}."))
  
  dat_mut %<>% 
    select(
      all_of(
        c('tumor_sample_barcode', 
          'hugo_symbol', 
          'mutation_status')
      )
    ) %>%
    mutate(mutation_status = tolower(mutation_status))
  
  dat_clin %<>%
    select(
      all_of(c('sample_id', 'patient_id', 'center', 'oncotree_code')),
      any_of(c('oncotree_primary_node'))
    ) 
  
  #print(glimpse(dat_clin))
  #print(glimpse(dat_mut))
  
  print(tabyl(dat_mut, mutation_status))
  
  cli::cli_alert_info(glue(
    "Number of unique hugo symbols: {nrow(tabyl(dat_mut,hugo_symbol))}"
  ))
  cli::cli_alert_info("Top 10 hugo symbols")
  tabyl(dat_mut, hugo_symbol) %>%
    arrange(desc(n)) %>%
    head(10) %>%
    print
  
  cli::cli_alert_info("Top 10 hugo symbols with the string 'BRCA'")
  tabyl(dat_mut, hugo_symbol) %>%
    filter(stringr::str_detect(hugo_symbol, "BRCA")) %>%
    arrange(desc(n)) %>%
    head(10) %>%
    print
  
  cli::cli_alert_info("JHU hugo mutations")
  dat_mut %>%
    filter(str_detect(tumor_sample_barcode, "GENIE-JHU-")) %>%
    tabyl(., hugo_symbol) %>%
    print
    
  
  
  # Needed for some older releases:
  if (!("oncotree_primary_node" %in% names(dat_clin))) {
    dat_clin %<>%
      left_join(., dft_oncotree, by = "oncotree_code") %>%
      mutate(tissue = if_else(is.na(tissue), "UNKNOWN", tissue)) %>%
      rename(oncotree_primary_node = tissue)
  }
  
  dat_mut %<>%
    filter(hugo_symbol %in% c("BRCA1", "BRCA2")) %>%
    # filter(mutation_status %in% "somatic") %>%
    mutate(variant = 1) %>%
    select(-c(hugo_symbol, mutation_status))
  
  dat_rtn <- left_join(
    dat_clin,
    dat_mut,
    by = c(sample_id = "tumor_sample_barcode")
  )
  
  # cli::cli_alert_info("JHU hugo symbols:")
  # dat_rtn %>%
  #   filter(center %in% "JHU") %>%
  #   tabyl(., hugo_symbol) %>%
  #   print
  # 
  # cli::cli_abort("Nah")
  
  cli::cli_alert_info("BRCA1/2 symbols by site (somatic filtering off):")
  
  tabyl(dat_rtn, center, variant) %>%
    print
  
  cli::cli_alert_info("mutation status symbols by site:")
  tabyl(dat_rtn, center, variant) %>%
    print

  
  
  
  # Ready for a hideous hack?
  # Every iteration I clear out the synapseCache so I don't run out of memory on
  #   the AWS instance.  This deletes all the cache for other projects, which
  #   I don't have a problem with because I usually save whatever data I need.
  fs::dir_delete("~/.synapseCache")
  
  
  # Check that oncotree_primary_node does not exist when oncotree code is missing.
  return(NULL)
  
}
