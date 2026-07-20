get_synapse_entity_data_in_csv <- function(
  synapse_id,
  version = NA,
  na.strings = c("", "NA"),
  comment.char = "#"
) {
  if (is.na(version)) {
    entity <- synGet(synapse_id)
  } else {
    entity <- synGet(synapse_id, version = version)
  }

  data <- readr::read_csv(
    entity$path,
    na = na.strings,
    comment = comment.char
  )

  data
}
