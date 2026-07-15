
any_time_overlaps <- function(t_start, t_end, tol = 0.5) {
  purrr::map_lgl(
    .x = seq_along(t_start),
    .f = \(idx) {
      this_start <- t_start[idx]
      this_end <- t_end[idx]
      o_start <- t_start[-idx] # o = other
      o_end <- t_end[-idx]
      
      cond_1 <- (this_end - o_start > tol) & (this_start - o_start < tol)
      cond_2 <- (this_start - o_end < -tol) & (this_start - o_start > tol)
      
      return(any(cond_1 | cond_2, na.rm = T))
    }
  )
}