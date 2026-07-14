# library(synapser)
library(tibble)
library(dplyr)
library(lubridate)
library(magrittr)
library(here)
# synLogin()

# synGetWiki("syn3380222") # mmmmk nope.

dft_releases <- readr::read_rds(file = here('data', 'releases_by_pt_nested.rds'))

# The point of this script is adding the release date column.  The join step
#   below will not work if we load data that already has it (say, during testing).
if ("release_date" %in% names(dft_releases)) {
  dft_releases %<>% select(-release_date)
}

# manual input with Vim magic.  Lacking reproducibility but it only took 5 minutes,
#   ... which is less than it would take me to learn pulling tables from wikis.
previous_consortium_releases <- tribble(
  ~release_version, ~upload_deadline, ~data_sequenced_before, ~release_date,
  '5.2-consortium', '9/19/2018 5:00 PM', 'Jan-2018','9/28/2018 5:00 PM',
  '5.3-consortium', '10/17/2018 5:00 PM', 'Jan-2018', '10/26/2018 5:00 PM',
  '5.4-consortium', '11/21/2018 5:00 PM', 'Jan-2018', '11/30/2018 5:00 PM',
  '5.5-consortium', '12/14/2018 5:00 PM', 'Jan-2018', '12/21/2018 5:00 PM',
  '6.1-consortium', '1/18/2019 5:00 PM', 'Jul-2018', '1/31/2019 5:00 PM',
  '6.2-consortium', '2/21/2019 5:00 PM', 'Jul-2018', '3/5/2019 5:00 PM',
  '6.3-consortium', '3/20/2019 5:00 PM', 'Jul-2018', '3/29/2019 5:00 PM',
  '6.4-consortium', '4/17/2019 5:00 PM', 'Jul-2018', '4/26/2019 5:00 PM',
  '6.5-consortium', '5/21/2019 5:00 PM', 'Jul-2018', '5/30/2019 5:00 PM',
  '6.6-consortium', '6/12/2019 5:00 PM', 'Jul-2018', '6/21/2019 5:00 PM',
  '7.1-consortium', '7/22/2019 5:00 PM', 'Jan-2019', '7/31/2019 5:00 PM',
  '7.2-consortium', '8/21/2019 5:00 PM', 'Jan-2019', '8/30/2019 5:00 PM',
  '7.3-consortium', '9/18/2019 5:00 PM', 'Jan-2019', '9/27/2019 5:00 PM',
  '7.4-consortium', '10/16/2019 5:00 PM', 'Jan-2019', '10/25/2019 5:00 PM',
  '7.5-consortium', '11/20/2019 5:00 PM', 'Jan-2019', '11/29/2019 5:00 PM',
  '7.6-consortium', '12/11/2019 5:00 PM', 'Jan-2019', '12/20/2019 5:00 PM',
  '8.1-consortium', '2/7/2020 5:00 PM', 'Jul-2019', '2/17/2020 5:00 PM',
  '8.2-consortium', '3/9/2020 5:00 PM', 'Jul-2019', '3/18/2020 5:00 PM',
  '8.3-consortium', '4/6/2020 5:00 PM', 'Jul-2019', '4/15/2020 5:00 PM',
  '8.4-consortium', '5/4/2020 5:00 PM', 'Jul-2019', '5/13/2020 5:00 PM',
  '8.5-consortium', '6/8/2020 2:07 PM', 'Jul-2019', '6/17/2020 5:00 PM',
  '9.1-consortium', '7/20/2020 5:00 PM', 'Jan-2020', '7/29/2020 5:00 PM',
  '9.2-consortium', '8/17/2020 5:00 PM', 'Jan-2020', '8/26/2020 5:00 PM',
  '9.3-consortium', '9/21/2020 5:00 PM', 'Jan-2020', '9/30/2020 5:00 PM',
  '9.4-consortium', '10/19/2020 5:00 PM', 'Jan-2020', '10/28/2020 5:00 PM',
  '9.5-consortium', '11/16/2020 5:00 PM', 'Jan-2020', '11/25/2020 5:00 PM',
  '9.6-consortium', '1/4/2021 5:00 PM', 'Jan-2020', '1/14/2021 5:00 PM',
  '10.1-consortium', '1/26/2021 5:00 PM', 'Jul-2020', '2/4/2021 5:00 PM',
  '10.2-consortium', '2/22/2021 5:00 PM', 'Jul-2020', '3/3/2021 5:00 PM',
  '10.3-consortium', '3/29/2021 5:00 PM', 'Jul-2020', '4/7/2021 5:00 PM',
  '10.4-consortium', '4/26/2021 5:00 PM', 'Jul-2020', '5/5/2021 5:00 PM',
  '10.5-consortium', '5/24/2021 5:00 PM', 'Jul-2020', '6/2/2021 5:00 PM',
  '10.6-consortium', '6/14/2021 5:00 PM', 'Jul-2020', '6/23/2021 5:00 PM',
  '11.1-consortium', '7/14/2021 5:00 PM', 'Jan-2021', '7/21/2021 5:00 PM',
  '11.2-consortium', '8/16/2021 5:00 PM', 'Jan-2021', '8/25/2021 5:00 PM',
  '11.3-consortium', '9/20/2021 5:00 PM', 'Jan-2021', '9/29/2021 5:00 PM',
  '11.4-consortium', '10/18/2021 5:00 PM', 'Jan-2021', '10/27/2021 5:00 PM',
  '11.5-consortium', '11/15/2021 4:00 PM', 'Jan-2021', '11/24/2021 4:00 PM',
  '11.6-consortium', '12/6/2021 4:00 PM', 'Jan-2021', '12/22/2021 4:00 PM',
  '12.1-consortium', '1/11/2022 4:00 PM', 'Jul-2021', '1/20/2022 4:00 PM',
  '12.2-consortium', '2/8/2022 4:00 PM', 'Jul-2021', '2/17/2022 4:00 PM',
  '12.3-consortium', '3/8/2022 5:00 PM', 'Jul-2021', '3/17/2022 5:00 PM',
  '12.4-consortium', '4/12/2022 5:00 PM', 'Jul-2021', '4/21/2022 5:00 PM',
  '12.5-consortium', '5/10/2022 5:00 PM', 'Jul-2021', '5/19/2022 5:00 PM',
  '12.6-consortium', '6/14/2022 5:00 PM', 'Jul-2021', '6/23/2022 5:00 PM',
  '13.1-consortium', '7/12/2022 5:00 PM', 'Jan-2022', '7/21/2022 5:00 PM',
  '13.2-consortium', '8/9/2022 5:00 PM', 'Jan-2022', '8/18/2022 5:00 PM',
  '13.3-consortium', '9/13/2022 5:00 PM', 'Jan-2022', '9/22/2022 5:00 PM',
  '13.4-consortium', '10/11/2022 5:00 PM', 'Jan-2022', '10/20/2022 5:00 PM',
  '13.5-consortium', '11/8/2022 5:00 PM', 'Jan-2022', '11/17/2022 5:00 PM',
  '13.6-consortium', '12/13/2022 5:00 PM', 'Jan-2022', '12/22/2022 5:00 PM',
  '14.1-consortium', '1/10/2023 5:00 PM', 'Jul-2022', '1/19/2023 5:00 PM',
  '14.2-consortium', '2/7/2023 5:00 PM', 'Jul-2022', '2/16/2023 5:00 PM',
  '14.3-consortium', '3/7/2023 5:00 PM', 'Jul-2022', '3/16/2023 5:00 PM',
  '14.4-consortium', '4/11/2023 5:00 PM', 'Jul-2022', '4/20/2023 2:50 PM',
  '14.5-consortium', '5/9/2023 5:00 PM', 'Jul-2022', '5/18/2023 5:00 PM',
  '14.6-consortium', '6/13/2023 5:00 PM', 'Jul-2022', '6/22/2023 5:00 PM'
)

# To check:
# previous_consortium_releases %>%
#   mutate(rel_date_lubri = lubridate::parse_date_time(release_date, 'm/d/y H:M p')) %>%
#   select(release_date, rel_date_lubri) %>%
#   sample_n(., size = 10)

# To fix:
previous_consortium_releases %<>%
  mutate(
    upload_deadline = lubridate::parse_date_time(upload_deadline, 'm/d/y H:M p'),
    release_date = lubridate::parse_date_time(release_date, 'm/d/y H:M p')
  )

dft_releases <- left_join(
  dft_releases,
  select(previous_consortium_releases, release_version, release_date),
  by = c(minor = "release_version")
) 

dft_releases %<>%
  relocate(release_date, .before = minor_createdOn)

readr::write_rds(
  x = dft_releases,
  file = here('data', 'releases_by_pt_nested.rds')
)

