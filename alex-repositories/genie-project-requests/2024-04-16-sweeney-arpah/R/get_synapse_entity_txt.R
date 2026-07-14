# slightly altered to get tibbles. 
get_synapse_entity_txt <- function(
        synapse_id, 
        version = NA,
        skip = 0,
        cols_to_lower = T
) {
    if (is.na(version)) {
        entity <- synGet(synapse_id, followLink = T)
    } else {
        entity <- synGet(synapse_id, followLink = T, version = version)
    }
    # 
    # data <- read_tsv(
    #     file = entity$path,
    #     show_col_types = F,
    #     progress = F,
    #     skip = skip,
    #     comment = "#"
    # )
    data <- data.table::fread(
        file = entity$path,
        skip = skip
    )
    
    if (cols_to_lower) {
        data <- rename_all(data, tolower)
    }
    
    
    return(data)
}
