library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

source(here('analysis', 'get_data.R'))
source(here('analysis', 'process_main_genie.R'))
source(here('analysis', 'process_bpc.R'))
