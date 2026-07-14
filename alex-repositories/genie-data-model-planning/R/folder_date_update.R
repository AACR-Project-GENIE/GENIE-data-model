folder_date_update <- function(
  loaded_config
) {
  if (loaded_config$add_date_to_folder) {
    loaded_config$storage_root <- paste0(
      loaded_config$storage_root,
      '-',
      today()
    )
  }
  loaded_config
}
