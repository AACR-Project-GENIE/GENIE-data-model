# Sets up the standard storage structure.
# It's assumed that config is loaded before running this.
storage_setup <- function(
  store_dir
) {
  fs::dir_create(fs::path(store_dir, 'dict'))
  fs::dir_create(fs::path(store_dir, 'data'))
  fs::dir_create(fs::path(store_dir, 'result'))
  fs::dir_create(fs::path(store_dir, 'output'))
}
