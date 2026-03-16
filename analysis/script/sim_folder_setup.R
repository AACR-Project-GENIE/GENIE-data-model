# Description:  Setup simulation structure.

library(fs)
library(here)
library(purrr)

purrr::walk(.x = fs::dir_ls('R'), .f = source)

fs::dir_create(path = here("sim", "gen_data"))
fs::dir_create(path = here("sim", "run_methods"))
fs::dir_create(path = here("sim", "evaled_methods"))

