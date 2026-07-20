# remotes::install_github("ropensci/refsplitr")

library(bibliometrix)
library(here)
library(tidyverse)
library(magrittr)

path_to_medline <- here('data-raw', '752 MEDLINE Full Records.ciw')
dir_output <- here('data', 'parsed_bib_data', 'medline_derived')

medline_bib <- bibliometrix::convert2df(
  file = path_to_medline,
  dbsource = 'wos'
)

readr::write_rds(
  medline_bib,
  file = here(dir_output, 'medline.rds')
)

# basically explains what's going on with the missing mesh terms:
# medline_bib %>%
#   mutate(MH_missing = is.na(MH)) %>%
#   tabyl(SA, MH_missing)
 
medline_bib %<>%
  # I'll give these some names I can actually read.
  # this is a combo of guesswork and reading https://en.wikipedia.org/wiki/RIS_(file_format), even though this isn't technically RIS.
  select(
    journal_number = UT, 
    database = DB, 
    title = TI, 
    year = PY, 
    language = LA, 
    type = DT, 
    doi = DI, 
    author_list = AU, 
    abstract = AB,
    mesh_terms = MH
  )

medline_bib %<>% 
  mutate(
    mesh_df = purrr::map(.x = mesh_terms, .f = mesh_list_to_df)
  )

readr::write_rds(
  medline_bib,
  file = here(dir_output, 'medline_sub.rds')
)

medline_bib %<>% filter(!is.na(mesh_terms))

mesh <- medline_bib %>%
  filter(!is.na(mesh_terms)) %>%
  select(doi, mesh_df) %>%
  unnest(mesh_df)

readr::write_rds(
  mesh, 
  file = here(dir_output, 'mesh_terms.rds')
)


