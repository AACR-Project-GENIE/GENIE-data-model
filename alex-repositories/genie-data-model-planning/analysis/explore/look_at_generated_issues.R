library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

issues <- readr::read_csv(
  here('data', 'qc', 'DFCI', 'qc_issues', 'DFCI_issues.csv')
)

# Here you can see the brief contains most of the info.
# However, my labels aren't getting surfaced unfortunately.
issues %>%
  View(.)
