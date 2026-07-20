dict_41 <- readr::read_csv(
  '/Users/apaynter/Downloads/v421_PRISSMM_Dictionary_Bayer_HER2.csv'
)


dict_41 %>% glimpse

dict_41 %>%
  filter(`Variable / Field Name` %in% 'rt_ca') %>%
  glimpse

# fields that are not marked as required but the curation directives say they are:
# - hybrid_death_ind
# - ca_stage_iv (debatedly)

# marked in the data dictionary but not in the curation directives:
# - path_proc_dt (curation directives say leave it blank)
# - md_ca

dict_41 %>%
  filter(`Required Field?` %in% 'y') %>%
  pull(`Variable / Field Name`)
