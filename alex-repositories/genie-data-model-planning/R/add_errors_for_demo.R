add_errors_for_demo <- function(x) {
  set.seed(8327)
  x %<>% select(-drugs_stop_time) # remove one col
  x <- bind_rows(x, slice(x, 2039)) # one dupe row
  x[500, "ca_directed_radtx_complete"] <- "1" # one marked incomplete

  ca_ind_index <- which(
    x$redcap_repeat_instrument %in%
      "cancer_diagnosis" &
      x$redcap_ca_index %in% '1'
  )
  # add a random non-nsclc cancer in:
  x[sample(ca_ind_index, 1), 'ca_type'] <- '5'

  # if we pick 10 random index cases then this should return at least one person with no index cases:
  x[sample(ca_ind_index, 10), 'redcap_ca_index'] <- '0'

  # Add 5/7 missing cancer seq and index flags
  x[sample(ca_ind_index, 5), 'redcap_ca_seq'] <- NA
  x[sample(ca_ind_index, 7), 'redcap_ca_index'] <- NA

  img_index <- which(
    x$redcap_repeat_instrument %in%
      "prissmm_imaging"
  )
  # Add some missing dob to image intervals.
  x[sample(img_index, 13), 'image_scan_int'] <- NA

  x %<>% drop_dots

  x
}
