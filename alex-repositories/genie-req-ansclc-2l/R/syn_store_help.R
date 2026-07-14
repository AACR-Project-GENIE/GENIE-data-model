syn_store_help <- function(
  sid,
  loc = here('data-raw'),
  v = NULL
) {
  fs::dir_create(loc)
  # not sure how to do this with synGet, so we'll do a conditional for the version.
  if (is.null(v)) {
    print('fire')
    print(sid)
    synGet(
      entity = sid,
      downloadLocation = loc,
      ifcollision = 'overwrite.local'
    )
  } else {
    synGet(
      entity = sid,
      downloadLocation = loc,
      ifcollision = 'overwrite.local',
      version = v
    )
  }
}
