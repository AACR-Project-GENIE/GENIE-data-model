dir_merged <- here('data-raw', 'bpc', 'step2-merged')

test_data <- readr::read_csv(
  here(dir_merged, 'BLADDERBPCIntake_data.csv')
)

test_irr <- readr::read_csv(
  here(dir_merged, 'BLADDERBPCIntake_irr.csv')
)

dim(test_data)
dim(test_irr)

# Lots of records in the data not in the IRR (as expected)
setdiff(unique(test_data$record_id), unique(test_irr$record_id))
setdiff(unique(test_irr$record_id), unique(test_data$record_id))
# The ones in the IRR but not the data seem to just have a 2 suffix.  Let's check:

irr_cases <- test_irr$record_id |>
  unique() |>
  # some use -2 and some use _2.
  stringr::str_replace_all("-2", "") |>
  stringr::str_replace_all("_2", "") |>
  unique()

setdiff(irr_cases, unique(test_data$record_id)) # yay
