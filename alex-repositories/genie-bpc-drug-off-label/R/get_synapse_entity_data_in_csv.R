get_synapse_entity_data_in_csv <- function(synapse_id, 
                                           version = NA) {
  if (is.na(version)) {
    entity <- synGet(synapse_id)
  } else {
    entity <- synGet(synapse_id, version = version)
  }
  
  data <- readr::read_csv(entity$path,
                   show_col_types = F,
                   progress = F)
  return(data)
}
