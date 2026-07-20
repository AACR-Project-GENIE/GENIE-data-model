# Description: Creates any folders needed for project.

library(purrr)
library(here)
library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

fs::dir_create(here('data'))
fs::dir_create(here('data-raw', 'bpc', 'step1-curated'))
fs::dir_create(here('data-raw', 'bpc', 'step2-merged'))
fs::dir_create(here('data-raw', 'bpc', 'step3-premsks'))
fs::dir_create(here('data-raw', 'bpc', 'step4-release'))

fs::dir_create(here('data', 'bpc', 'step1-curated'))
fs::dir_create(here('data', 'bpc', 'step2-merged'))
fs::dir_create(here('data', 'bpc', 'step3-premsks'))
fs::dir_create(here('data', 'bpc', 'step4-release'))
