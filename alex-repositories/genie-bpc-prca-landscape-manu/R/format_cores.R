
format_cores <- function(vec) {
  vec_lev <- c(
    # based on the empirical distribution - not meaningful.
    "0-8 cores",
    "9-15 cores",
    ">15 cores",
    "Biopsy cores examined, number unknown",
    "No needle core biopsy performed",
    "Not applicable information not collected for this case",
    "Not documented in medical record"
  )
  vec_num <- if_else(str_detect(vec, "^[0-9]+$"),
                     as.double(vec),
                     NA_real_,
                     NA_real_)
  
  form_vec <- case_when(
    vec %in% vec_lev[4:7] ~ vec,
    vec_num >= 0 & vec_num <= 8 ~ vec_lev[1],
    vec_num >= 9 & vec_num <= 15 ~ vec_lev[2],
    vec_num >= 16 ~ vec_lev[3]
  )
  
  form_vec <- factor(form_vec, levels = vec_lev)
  
  return(form_vec)
  
}
