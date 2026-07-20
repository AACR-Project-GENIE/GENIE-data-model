read_config_file <- function(file_loc, update_with_date = T) {
  conf <- read_yaml(file_loc)
  if (update_with_date) {
    conf <- folder_date_update(conf)
  }
  conf
}
