#' @title Filter img/med_onc events down to a range.
#'
#' @description A simple join and subset, just a convenience function.
#'
#' @param df_time Dataframe with record_id, dob_event_min (minimum event time) and dob_event_max (maximum event time).
#' @param df_event An event dataframe of the style output by img_distill() or med_onc_distill().
#' @param tol_low Tolerance (days) surrounding dob_event_min.  The default of +0.5 will exclude events on dob_event_min.
#' @param tol_high Tolerance (days) surrounding dob_event_max.  The default of 0.5 will include events on dob_event_max.
#' @export
filter_events <- function(
  df_time,
  df_time_key_cols,
  df_event,
  tol_low = 0.5,
  tol_high = 0.5
) {
  if (
    !all(
      c(df_time_key_cols, 'dob_event_min', 'dob_event_max') %in%
        colnames(df_time)
    )
  ) {
    cli::cli_abort(
      "'df_time' must have columns in df_time_key_cols, along with dob_event_min, dob_event_max"
    )
  }

  included_events <- dplyr::left_join(
    df_event,
    dplyr::select(
      df_time,
      all_of(df_time_key_cols),
      dob_event_min,
      dob_event_max
    ),
    by = 'record_id'
  )

  included_events <- included_events |>
    dplyr::filter(
      dob_event_min + tol_low < dob_event_days,
      dob_event_max + tol_high > dob_event_days
    )

  included_events
}
