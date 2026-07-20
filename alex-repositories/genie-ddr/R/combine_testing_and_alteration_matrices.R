combine_testing_and_alteration_matrices <- function(
  dat_test,
  dat_alt
) {
  if (length(setdiff(rownames(dat_alt), rownames(dat_test))) > 0) {
    cli::cli_abort(
      "Samples exist in the alteration data not listed in the testing data - this is impossible."
    )
  }
  if (length(setdiff(colnames(dat_alt), colnames(dat_test))) > 0) {
    cli::cli_abort(
      "Genes exist in the alteration data not listed in the testing data - this is impossible."
    )
  }
  if (any(is.na(dat_test))) {
    cli::cli_abort("No NA values allowed in dat_test.")
  }
  if (any(is.na(dat_alt))) {
    cli::cli_abort("No NA values allowed in dat_alt.")
  }

  samples_not_in_alt <- setdiff(rownames(dat_test), rownames(dat_alt))

  dat_alt_row_expand <- matrix(
    FALSE,
    nrow = length(samples_not_in_alt),
    ncol = ncol(dat_alt)
  )
  rownames(dat_alt_row_expand) <- samples_not_in_alt
  colnames(dat_alt_row_expand) <- colnames(dat_alt)
  dat_alt <- rbind(dat_alt, dat_alt_row_expand)
  dat_alt <- dat_alt[rownames(dat_test), ]

  genes_not_in_alt <- setdiff(colnames(dat_test), colnames(dat_alt))
  dat_alt_col_expand <- matrix(
    FALSE,
    nrow = nrow(dat_alt),
    ncol = length(genes_not_in_alt)
  )
  rownames(dat_alt_col_expand) <- rownames(dat_alt)
  colnames(dat_alt_col_expand) <- genes_not_in_alt
  dat_alt <- cbind(dat_alt, dat_alt_col_expand)

  dat_alt <- dat_alt[, colnames(dat_test)]

  # check that everything lines up:
  if (any(dim(dat_alt) != dim(dat_test))) {
    cli::cli_abort("matrix merge failed - different dimensions")
  }
  if (any(rownames(dat_alt) != rownames(dat_test))) {
    cli::cli_abort("matrix merge failed - rownames don't match")
  }
  if (any(colnames(dat_alt) != colnames(dat_test))) {
    cli::cli_abort("matrix merge failed - colnames don't match")
  }

  # NA = not tested, F = tested but not positive, T = tested and positive (altered)
  # The way this is written a person with a positive will note that regardless
  #   of whether the test file says they're tested for the gene.
  dat_rtn <- matrix(NA, nrow = nrow(dat_test), ncol = ncol(dat_test))
  dat_rtn[dat_test] <- F
  dat_rtn[dat_alt] <- T
  rownames(dat_rtn) <- rownames(dat_test)
  colnames(dat_rtn) <- colnames(dat_test)

  return(dat_rtn)
}
