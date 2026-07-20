release_saver <- function(
  cohort,
  release,
  synid
) {
  subfold <- get_syn_children_df(synid)

  subfold <- subfold |>
    filter(str_detect(name, 'clinical_data'))

  if (nrow(subfold) > 1) {
    cli_abort("Multiple clinical data folders found.")
  } else if (nrow(subfold) < 1) {
    cli_abort("No clinical data folders found.")
  }

  clin_dat_dir <- get_syn_children_df(subfold$id)

  release_helper <- function(
    synid
  ) {
    release_dir <- here(
      "data-raw",
      'bpc',
      cohort,
      release
    )

    fs::dir_create(release_dir)
    synGet(
      entity = synid,
      downloadLocation = release_dir,
      ifcollision = "overwrite.local"
    )
  }

  purrr::walk(
    clin_dat_dir$id,
    release_helper
  )
}
