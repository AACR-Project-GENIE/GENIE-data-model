# Main workflow for the project

library(here); library(purrr); library(fs)
purrr::walk(fs::dir_ls(here('R')), .f = source)

source(here('analysis', 'script', 'get_raw_data.R')) # takes ~30s
source(here('analysis', 'script', 'get_bpc_data.R'))
source(here('analysis', 'script', 'process_samples.R'))

quarto::quarto_render(
    input = here('analysis', 'report', "her2_ultra_low.qmd")
)

file.copy(
    from = here('analysis', 'report', 'her2_ultra_low.html'),
    to = here('output', 'her2_ultra_low.html'),
    overwrite = T
)
    