# remotes::install_github("ropensci/refsplitr")

library(bibliometrix)
library(here)
library(tidyverse)
library(magrittr)
library(janitor)

path_to_medline <- here('data-raw', '752 MEDLINE Full Records.ciw')

# bibliometrix is probably the best available.  The all-upper nonsense is 
#   apparently working as intended by the author:
# https://github.com/massimoaria/bibliometrix/issues/57

# Some that failed:
# medline <- references_read(
#   data = path_to_medline,
#   include_all = T
# )
# 
# medline_rev <- revtools::read_bibliography(
#   filename = path_to_medline
# )
# 
# medline_rismed <- RISmed:::read.ris(
#   file = path_to_medline
# )

medline_bib <- bibliometrix::convert2df(
  file = path_to_medline,
  dbsource = 'wos'
)

# K this is weird:
medline_bib %>%
  mutate(MH_missing = is.na(MH)) %>%
  tabyl(SA, MH_missing)

 
medline_bib %<>%
  # I'll give these some names I can actually read.
  # this is a combo of guesswork and reading https://en.wikipedia.org/wiki/RIS_(file_format), even though this isn't RIS.
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

medline_bib %<>% filter(!is.na(mesh_terms))

medline_bib %<>% 
  mutate(
    mesh_df = purrr::map(.x = mesh_terms, .f = mesh_list_to_df)
  )

medline_bib %>%
  select(doi, mesh_df) %>%
  unnest(mesh_df) %>%
  filter(doi %in% c('10.1186/s40246-024-00615-7',
                    '10.1038/s41467-021-27889-y')) %>%
  View(.)


medline_bib %>%
  select(doi, mesh_df) %>%
  unnest(mesh_df) %>%
  mutate(semicolon_count = str_count(mesh, '\\;')) %>%
  filter(semicolon_count > 2) %>%
  View(.)
  

# medline_2 <- references_read(
#   data = here('data-raw', '752 MEDLINE RIS.ciw'),
#   include_all = T
# )


medline %>%
