library(tidyverse)
library(magrittr)

smoke_clin <- read_rds(
  here('data', 'smoke', 'smoke_clin.rds')
)

# I should do all the modifications and then come back to main GENIE.  I'm in a hurry now so I won't.

# just the stuff we aspire to get in main GENIE.
smoke_clin_mg <- smoke_clin %>%
  select(
    ca_cig_ever,
    stage_dx, # aspire to get.
    dob_ca_dx_yrs, # aspire to get.
    institution,
    birth_year,
    ethnicity = naaccr_ethnicity_code,
    race = naaccr_race_code_primary,
    sex = naaccr_sex_code,
    dx_cpt_rep_days,
    sample_type
  )

readr::write_csv(
  smoke_clin_mg,
  here('data', 'smoke', 'smoke_mod_mg_strings.csv')
)

# ivc = indicator variable coding.
smoke_clin_mg_ivc <- smoke_clin_mg %>%
  fastDummies::dummy_cols(
    remove_most_frequent_dummy = T,
    remove_selected_columns = T,
    select_columns = c(
      'sample_type',
      'stage_dx',
      'institution',
      'ethnicity',
      'race',
      'sex'
    )
  ) %>%
  rename_with(\(x) str_sub(x, 1, 25))

smoke_clin_mg_ivc %<>%
  mutate(ca_cig_ever = as.numeric(ca_cig_ever))

readr::write_csv(
  smoke_clin_mg,
  here('data', 'smoke', 'smoke_mod_mg_ivc.csv')
)
