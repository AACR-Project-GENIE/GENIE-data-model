# Just playing with the YAML loader to see what structure pops up.

library(yaml)

config <- read_yaml(here('analysis', 'explore', 'config.yaml'))

config %>% str

config %>% lobstr::tree(max_depth = 1)
