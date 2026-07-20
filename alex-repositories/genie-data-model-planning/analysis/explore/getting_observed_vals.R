# This is not a reproducible script.  Starting from the q0 script data on a set with known issues (providence)

info_dat <- basil_sum %>%
  pull(validation_subset) %>%
  .[[1]]
# data rows where the problems occured:
ext_dat <- basil_sum %>%
  pull(extracts) %>%
  .[[1]]

rep_dat <- basil_sum %>%
  pull(report) %>%
  .[[1]]

info_dat_2 <- full_join(
  info_dat,
  select(
    rep_dat,
    i,
    type,
    columns,
    values,
    precon
  ),
  by = 'i',
  relationship = 'one-to-one'
)

failed_cases_2 <- bind_rows(
  ext_dat,
  .id = 'i'
) %>%
  mutate(i = as.integer(i))

# Making up a few things so I can see that it's working OK:
failed_cases_2[4, 'quality_assurance_complete'] <- "-1"

rtn_dat <- inner_join(
  failed_cases_2,
  #select(failed_cases, i, all_of(key_cols)),
  info_dat_2,
  by = 'i',
  relationship = 'many-to-one'
)

# Second made up part:
rtn_dat[3, 'columns'] <- 'naaccr_race_code_primary'
rtn_dat[3, 'naaccr_race_code_primary'] <- 'alien'

rtn_dat[2, 'columns'] <- 'something_not_in_data'

rtn_dat <- rtn_dat %>%
  mutate(.temp_row_id = row_number()) %>%
  nest(., .by = .temp_row_id, .key = 'one_row_dat') %>%
  mutate(
    one_row_dat = map(
      one_row_dat,
      ~ mutate(., .obs_val = .[[columns]])
    )
  ) %>%
  unnest(one_row_dat)

rtn_dat <- rtn_dat %>%
  mutate(
    observed_value = case_when(
      is.na(.obs_val) ~ "(not_applicable)",
      T ~ glue('{columns}:{.obs_val}')
    )
  ) %>%
  pull(observed_value)

rtn_dat %>%
  select(all_of(colnames(info_dat)), i, all_of(key_cols))
