library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

# Load the indications data (limited to FDA) and crosswalks:
dft_ind_lim <- readr::read_rds(
  here('data', 'warner_materials', 'indications_limited.rds')
)
dft_cw_condition <- readr::read_rds(
  here('data', 'warner_materials', 'cw_condition.rds')
)
dft_cw_drug <- readr::read_rds(
  here('data', 'warner_materials', 'cw_drug.rds')
)

# Load and create the hdrug cohort data:
dft_cohort_cases <- readr::read_rds(
  here('data', 'cohort', 'clin_dat_wide.rds')
)
dft_hdrug_cohort <- dft_cohort_cases %>%
  select(cohort, hdrug) %>%
  unnest(hdrug)




# Map the columns we need in the indications over to the PRISSMM names:
dft_ind_converted <- left_join(
  dft_ind_lim,
  dft_cw_drug,
  by = "component"
) %>%
  rename(mapped_agent = agent)

dft_ind_converted <- left_join(
  dft_ind_converted,
  dft_cw_condition,
  by = 'condition'
) %>%
  rename(mapped_cohort = cohort)

dft_ind_converted %<>%
  filter(!is.na(mapped_cohort) & !is.na(mapped_agent))

possible_approvals <- left_join(
  (dft_hdrug_cohort %>% 
     select(cohort, record_id, ca_seq, regimen_number, drug_number, agent)
  ),
  (dft_ind_converted %>%
     select(
       cohort = mapped_cohort,
       agent = mapped_agent,
       date,
       context, 
       stage_or_status,
       condition,
       agent_map_type
     ) %>%
     mutate(approval_exists = T)
  ),
  by = c('cohort', 'agent'),
  relationship = "many-to-many"
)

possible_approvals %<>% 
  mutate(
    approval_exists = if_else(
      is.na(approval_exists), 
      F, 
      approval_exists
    )
  )

readr::write_rds(
  possible_approvals,
  here('data', 'linked_approvals', 'possible_approvals.rds')
)


dft_hdrug_app <- possible_approvals %>%
  # This is excessive to get uniqueness - I just want the drug and cohort for
  #   the sake of documentation.
  group_by(
    cohort, record_id, ca_seq, regimen_number, drug_number, agent
  ) %>%
  summarize(
    # renaming here because I didn't like "approval"
    ind_exists = sum(approval_exists, na.rm = T) >= 1,
    .groups = "drop"
  )

readr::write_rds(
  dft_hdrug_app,
  here('data', 'linked_approvals', 'hdrug_ind.rds')
)



dft_hdrug_app %>%
  group_by(cohort, agent) %>%
  summarize(
    n_uses = n(),
    n_indicated = sum(ind_exists, na.rm = T),
    prop_approved = n_indicated / n_uses,
    .groups = "drop"
  )







