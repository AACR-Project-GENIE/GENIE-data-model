library(purrr)
library(here)
library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

output_dir <- here('output', 'manu', 'fig5')

dir_biomark <- here('data', 'stories', 'biomarker_driven_use')

gg_tras_mosaic <- readr::read_rds(here(dir_biomark, 'tras_gg_mosaic.rds'))
gg_vemura_mosaic <- readr::read_rds(here(dir_biomark, 'vemura_gg_mosaic.rds'))

# If you need more raw data for annotations you have saved stuff,e.g.:
#dft_vemura_model <- readr::read_rds(here(dir_biomark, 'vemura_model.rds'))
#gg_vemura_tab <- readr::read_rds(here(dir_biomark, 'vemura_gg_tab.rds'))

fig4 <- cowplot::plot_grid(
  gg_vemura_mosaic,
  gg_tras_mosaic,
  labels = 'AUTO'
)

fs::dir_create(output_dir)
ggsave(
  fig4,
  filename = here(output_dir, 'fig5-guideline-tras-vermura.pdf'),
  height = 3,
  width = 8
)
