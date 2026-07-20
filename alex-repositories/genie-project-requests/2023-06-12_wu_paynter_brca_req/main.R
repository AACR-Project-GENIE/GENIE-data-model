library(here)
library(fs)
library(purrr)
library(rmarkdown)


source(here("scripts", "get_data.R")) # takes a while.
source(here('scripts', 'fix_dates.R'))
source(here("scripts", "get_bpc_data.R"))
rmarkdown::render(
  # this file also creates the CSV files:
  input = here("scripts", "wu-brca-main-genie-request.Rmd"),
  output_file = here("output", "wu-brca-main-genie-request.html")
)
source(here("scripts", "upload_synapse.R"))



