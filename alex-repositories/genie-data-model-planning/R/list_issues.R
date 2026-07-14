# for now all datasets have the same key.  This may need to change if this is not the case in the future.

list_issues <- function(
  val_sub,
  extracts,
  report,
  report_observed = TRUE,
  key_cols = c(
    "record_id",
    "redcap_repeat_instrument",
    "redcap_repeat_instance"
  )
) {
  if (length(extracts) > 0) {
    info_on_the_checks <- full_join(
      val_sub,
      select(
        report,
        i,
        type,
        columns,
        values,
        precon
      ),
      by = 'i',
      relationship = 'one-to-one'
    )

    failed_cases <- bind_rows(
      extracts,
      .id = 'i'
    ) %>%
      mutate(i = as.integer(i))

    rtn <- inner_join(
      failed_cases,
      info_on_the_checks,
      by = 'i',
      relationship = 'many-to-one'
    )

    if (report_observed) {
      # There's definitely a faster way to do this.
      # Nesting and unnesting becuase we want to pull from different
      #   columns in each row.
      rtn <- rtn %>%
        mutate(.temp_row_id = row_number()) %>%
        nest(., .by = .temp_row_id, .key = 'one_row_dat') %>%
        mutate(
          one_row_dat = map(
            one_row_dat,
            ~ mutate(., .obs_val = .[[columns]])
          )
        ) %>%
        unnest(one_row_dat)

      rtn <- rtn %>%
        mutate(
          observed_value = case_when(
            is.na(.obs_val) ~ "(not_applicable)",
            T ~ glue('{columns}:{.obs_val}')
          )
        ) %>%
        drop_dots() # redundant but more clear to me.
    }
    rtn <- rtn %>%
      select(
        all_of(colnames(info_on_the_checks)),
        i,
        all_of(key_cols),
        any_of('observed_value')
      )
  } else {
    rtn <- NULL
  }

  rtn
}
