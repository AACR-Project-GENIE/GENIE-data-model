cohort_release_order <- function(
  coh_vec
) {
  factor(
    coh_vec,
    levels = c(
      "NSCLC",
      'CRC',
      'Breast',
      'Pancreas',
      'Prostate',
      'Bladder'
    )
  )
}
