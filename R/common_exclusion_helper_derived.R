# just a list of columns that we want stripped out before saving a derived dataset.  Very manual for now.

#' Column exclusion helper for derived variables.
#'
#' @param dat Any dataset with PRISSMM variable names (otherwise it won't do much.
#'
#' @returns A version of `dat` with column names removed that we typically don't share.
#' @export
#'
#' @examples
column_exclusion_helper_derived <- function(
  dat
) {
  # columns to be excluded.  Will also exclude some based on patterns
  excl_col <- c(
    'cpt_seq_date',
    'ca_qacurator',
    'ca_qa',
    'ca_qaissues',
    'ca_qaresolve',
    'curation_dt',
    # these follow a pattern but 'partial' seems like too common of a word so it scares me to do bulk exclusions.
    'qa_partial',
    'ca_partial',
    'pt_partial',
    'cdrug_partial',
    'rt_partial',
    'path_partial',
    'image_partial',
    'md_partial',
    'cpt_partial'
  )

  dat <- dat %>%
    dplyr::select(
      -tidyselect::any_of(excl_col),
      -tidyselect::matches('^vstat_'),
      -tidyselect::matches('start_time$'),
      -tidyselect::matches('stop_time$'),
      -tidyselect::matches('qamajor'),
      -tidyselect::matches('qaminor'),
      -tidyselect::matches('qaother'),
      -tidyselect::matches('qatype'),
      -tidyselect::matches('^qa_'),
      -tidyselect::matches('_complete$'),
      -tidyselect::matches('^completion_'),
      -tidyselect::matches('_qacurator$'),
      -tidyselect::matches('_qa$'),
      -tidyselect::matches('_qaissues$'),
      -tidyselect::matches('_qaresolve$'),
    )

  return(dat)
}
