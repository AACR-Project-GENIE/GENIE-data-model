library(tidyverse)

dat <- tibble(
  name = character(0),
  n = numeric(0),
  lab = character(0),
  color = character(0)
)

# For now I'm just going to input the numbers.  You could obviously add
#   a section to download the data and get them directly if you want.
dat <- dat |>
  add_row(
    name = "GENIE",
    n = 196 * 10^3,
    lab = "Main<br>GENIE",
    color = "#ffecd1"
  ) |>
  add_row(
    name = "BPC consortium",
    n = 3819 + 2299 + 1129 + 1109 + 1116 + 714,
    lab = "BPC<br>consortium",
    color = "#B6E0FF"
  ) |>
  add_row(
    name = "BPC public",
    n = 1846 + 1485, # CRC and lung
    lab = "BPC<br>public",
    color = "#49a2b6"
  ) |>
  add_row(
    name = "Sponsored projects",
    n = 1000,
    lab = "Sponsored<br>projects",
    color = "#ff7d00"
  ) |>
  add_row(
    name = "Additional projects",
    n = 500,
    lab = "Additional<br>Projects",
    color = "#78290f"
  )

readr::write_rds(dat, 'cohort_data.rds')
