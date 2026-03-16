#' @title Filter dataframe by record ID.
#' 
#' @description Filters a dataframe by rid_vec.  If the dataframe does not
#'  have a column called record_id, it does nothing.
#'  
#' @param dat Data frame to be filtered.
#' @param rid_vec Vector of valid `record_id`s.
filter_df_by_rid <- function(dat, rid_vec) {
  if (!('data.frame' %in% class(dat))) {
    cli::cli_abort("Non-dataframe given.")
  }
  if (!("record_id" %in% names(dat))) {
    return(dat)
  } else {
    dat %>% filter(record_id %in% rid_vec) %>% return(.)
  }
}