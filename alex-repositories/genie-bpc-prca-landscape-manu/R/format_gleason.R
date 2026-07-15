
format_gleason <- function(vec) {
  vec_lev <- c(
    "<8",
    ">=8",
    "No biopsy or TURP done",
    "Not available"
  )
  
  form_vec <- case_when(
    vec %in% paste0(c("Gleason score "), 2:7) ~ vec_lev[1],
    vec %in% paste0(c("Gleason score "), 8:10) ~ vec_lev[2],
    str_detect(vec, "No needle core") ~ vec_lev[3],
    str_detect(vec, "(Not applicable)|(Not documented)") ~ vec_lev[4],
    T ~ "error"
  )
  
  if (any(form_vec %in% "error")) {
    cli::cli_abort("Problem with format_gleason()")
  }
  
  form_vec <- factor(form_vec, vec_lev)
}