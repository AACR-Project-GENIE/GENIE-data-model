#' @title Summarize the response to med onc OR imaging using only the cross sectional flags.
#'
#' @description Cross sectional flags are those in status_change, or equivalently the one letter codes in resp_code_cs

#' @details
#'  Design note: This is a bulky function.  Time filtering has to be done in this function in addition to the summarization.  The reason is that the longitudinal versions (if ever written) will require baseline images to be fed in too, to establish where cancer has been found so far.
#' Confirmed response breakdown (the first one is a subset):
#' - not evaluable or stable disease, 0 or more times.
#' - response.
#' - not evaluable, 0 or more times.
#' - stable or response, 1 or more times.

#' @param df_time A row of events to calculate responses to.  Must have columns record_id, dob_event_min, dob_event_max.
#' @param df_time_key_cols A character vector specifying the primary keys for this dataset (must be unique).
#' @param df_event An event dataframe of the style output by img_distill() or med_onc_distill().
#' @export
response_sum_cs <- function(
  df_time,
  df_time_key_cols,
  df_event,
  time_tol_lb = 0.5,
  time_tol_ub = 0.5,
  event_type_override = NULL
) {
  check_unique_rows(df_time, df_time_key_cols)
  nr_input <- nrow(df_time)

  if (length(unique(df_event$event_type)) > 1) {
    cli::cli_abort(
      "response_sum_cs() only works on one event type (med onc or imaging, you combine them later on"
    )
  }

  if (is.null(event_type_override)) {
    event_title <- dplyr::first(dplyr::pull(df_event, event_type))
  } else {
    event_title <- event_type_overrride
  }

  df_resp_lim <- filter_events(
    df_time = df_time,
    df_event = df_event,
    df_time_key_cols = df_time_key_cols,
    tol_low = time_tol_lb,
    tol_high = time_tol_ub
  )

  df_resp_lim <- df_resp_lim %>%
    group_by(across(all_of(df_time_key_cols))) %>%
    summarize(
      "{event_title}_n" := n(),
      "{event_title}_n_eval" := sum(!(resp_code_cs %in% 'n')),
      "{event_title}_resp_codes" := paste(resp_code_cs, collapse = ""),
      .groups = "drop"
    )

  # Need to put those who had no scans in range back in:
  no_scan_group <- anti_join(
    df_time,
    df_resp_lim,
    by = df_time_key_cols
  ) %>%
    select(all_of(df_time_key_cols))

  df_resp_lim <- bind_rows(
    df_resp_lim,
    no_scan_group
  )

  df_resp_lim %<>%
    mutate(
      "{event_title}_resp_cat" := stringr::str_detect(
        .data[[glue::glue('{event_title}_resp_codes')]],
        "[ns]*r"
      ),
      "{event_title}_resp_cat_conf" := stringr::str_detect(
        .data[[paste0(event_title, "_resp_codes")]],
        "[ns]*r[n]*[sr]+"
      ),
      # I'll go ahead and write these out as RECIST-speak:
      "{event_title}_bor_recist" := best_response_recist_style(
        .data[[paste0(event_title, "_resp_codes")]]
      )
    )

  # Check the output:
  check_unique_rows(df_resp_lim, df_time_key_cols)
  if (!(nr_input == nrow(df_resp_lim))) {
    cli_abort("Output has different number of rows than input.")
  }

  df_resp_lim
}
