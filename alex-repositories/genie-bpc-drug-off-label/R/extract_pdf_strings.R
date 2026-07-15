extract_pdf_strings <- function(
    v_pdf, 
    str_to_match
) {
  res <- map(
    .x = v_pdf,
    .f = \(x) {
      str_extract_all(x, pattern = str_to_match)
    }
  )
  
  res <- unlist(res)
  
  return(res)
  
}