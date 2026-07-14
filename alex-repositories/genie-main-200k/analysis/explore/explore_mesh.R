dir_input <- here('data', 'parsed_bib_data', 'medline_derived')

mesh <- readr::read_rds(
  here(dir_input, 'mesh_terms.rds')
)

mesh %<>% distinct(.)

medline_sub <- readr::read_rds(
  here(dir_input, 'medline_sub.rds')
)

# For each duplicate doi, take whichever one has a longer mesh term list.
medline_sub %<>%
  filter(!is.na(doi)) %>% 
  group_by(doi) %>%
  mutate(length_mesh = nchar(mesh_terms)) %>%
  arrange(desc(length_mesh)) %>%
  slice(1) %>%
  ungroup(.)

hf_mesh_terms <- mesh %>%
  count(mesh, sort = T) %>%
  filter(n >= 10) %>%
  pull(mesh)

mesh %<>%
  filter(mesh %in% hf_mesh_terms) %>%
  left_join(
    .,
    select(medline_sub, -c(mesh_df, mesh_terms)),
    by = 'doi',
    relationship = 'many-to-one'
  )

mesh %>%
  count(mesh, year)

mesh %>% group_by(year) %>% summarize(n_works = length(unique(doi)))

# normalize within each year somehow, then look at which ones are on the rise.
