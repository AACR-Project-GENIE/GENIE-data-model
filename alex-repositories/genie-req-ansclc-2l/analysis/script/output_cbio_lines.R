output_cbio_lines <- function(
  ids,
  file = NULL,
  return_vec = F,
  cohort = 'panc_genie_bpc'
) {
  stub1 <- "https://genie-private.cbioportal.org/patient?studyId="
  stub2 <- "&caseId="

  out_lines <- paste0(
    stub1,
    cohort,
    stub2,
    ids
  )

  if (return_vec) {
    return(out_lines)
  } else if (is.null(file)) {
    cat(paste0(out_lines, '\n', sep = ''))
  } else {
    writeLines(out_lines, file)
  }
}
