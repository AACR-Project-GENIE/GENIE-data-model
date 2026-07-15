library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

# Load the indications data (limited to FDA) and crosswalks:
dft_ind_lim <- readr::read_rds(
  here('data', 'warner_materials', 'indications_mapped_limited.rds')
)
dft_cw_condition <- readr::read_rds(
  here('data', 'warner_materials', 'cw_condition.rds')
)
dft_cw_drug <- readr::read_rds(
  here('data', 'warner_materials', 'cw_drug.rds')
)
vec_excl_drugs <- readr::read_rds(
  here('data', 'warner_materials', 'old_drugs_list.rds')
)

# Load the hdrug cohort data:
dft_hdrug_cohort_lim <- readr::read_rds(
  here('data', 'cohort', 'hdrug_with_conmeds.rds')
)

# Add the biomarker lists into the cohort data:
dft_biom_flags <- readr::read_rds(
  here('data', 'cohort', 'biomarker_flags',
       'biomarker_flags_by_drug.rds')
)

dft_hdrug_cohort_lim <- left_join(
  dft_hdrug_cohort_lim,
  dft_biom_flags,
  by = c('cohort', 'record_id', 'ca_seq', 'regimen_number', 'drug_number')
)


# Fix up the date column from the indications sheet:
dft_ind_lim %<>% 
  mutate(
    date = case_when(
      # If the date is uncertain we just assume it was always approved.
      date %in% "Uncertain date" ~ ymd(rep('1900-01-01', times = n())),
      T ~ suppressWarnings(ymd(date))
    )
  )



dft_poss_app <- make_possible_indication_cohort( 
  dat_hdrug = dft_hdrug_cohort_lim,
  dat_ind = dft_ind_lim
)

# Check for metastatic disease:
dft_poss_app <- dft_poss_app %>%
  add_check_met(.) 


simple_biom_tests <- readr::read_rds(
  here('data', 'linked_approvals', 'simple_biom_tests.rds')
)

# Need NULLs here to allow passing to function correctly.
simple_biom_tests %<>%
  mutate(
    test_name = purrr::map(
      .x = test_name,
      .f = \(z) if(is.na(z)) return(NULL) else return(z)
    )
  )

# Working tip: add_check_with_simple() has a verbose_results setting to print
#   some checks.  Turn this on to check typos or off for quieter work. 
for (k in seq_len(nrow(simple_biom_tests))) {
  dft_poss_app <- add_check_biomarker_simple(
    dat_poss_app = dft_poss_app,
    ind_sheet_bio_req = pull(simple_biom_tests, ind_sheet_bio_req)[[k]],
    biom_col = pull(simple_biom_tests, biom_col)[[k]],
    test_name = pull(simple_biom_tests, test_name)[[k]]
  )
}






dft_simple_with_tests <- readr::read_rds(
  here('data', 'linked_approvals', 'simple_with_tests.rds')
)

# To iterate more easily we'll make the other columns lists:
dft_simple_with_tests %<>%
  mutate(
    with_req = as.list(with_req),
    test_name = as.list(test_name),
    test_name = purrr::map(
      .x = test_name,
      .f = \(z) if(is.na(z)) return(NULL) else return(z)
    )
  )

# Working tip: add_check_with_simple() has a verbose_results setting to print
#   some checks.  Turn this on to check typos or off for quieter work. 
for (k in seq_len(nrow(dft_simple_with_tests))) {
  dft_poss_app <- add_check_with_simple(
    dat_poss_app = dft_poss_app,
    with_req = pull(dft_simple_with_tests, with_req)[[k]],
    agent_req = pull(dft_simple_with_tests, agent_req)[[k]],
    test_name = pull(dft_simple_with_tests, test_name)[[k]]
  )
}



dft_poss_app %<>% add_check_monotherapy(.) 
dft_poss_app %<>% add_check_chemo(.)
dft_poss_app %<>% add_check_plat(.)
dft_poss_app %<>% add_check_plat_doublet(.)
dft_poss_app %<>% add_check_ipilum_pd(.)
dft_poss_app %<>% add_check_carbotaxol_nab(.)
dft_poss_app %<>% add_check_pem_plat(.)
dft_poss_app %<>% add_check_tras_chemo(.)
dft_poss_app %<>% add_check_ai(.)
dft_poss_app %<>% add_check_ai_or_tamox(.)
dft_poss_app %<>% add_check_pd_nivo(.)
dft_poss_app %<>% add_check_fluor_iri_or_oxal(.)







  
dft_poss_app %<>% add_check_date_definite(.) 
dft_poss_app %<>% add_check_multiple_tests(dat_poss_app = .) # by default selects all "test" columns


readr::write_rds(
  dft_poss_app,
  here('data', 'linked_approvals', 'possible_approvals.rds')
)

dft_hdrug_determinations <- summarize_possible_approvals_2(dft_poss_app)

# tabyl(dft_hdrug_determinations, cohort, valid_ind_exists)
# tabyl(dft_hdrug_determinations, failure_type, cohort) %>%
#   View(.)

levs_failure_type <- c(
  "No indications found",
  "Specific req. not met",
  "Started before approval"
)

dft_hdrug_determinations %<>%
  mutate(
    failure_type_f = case_when(
      failure_type %in% "test_ind_exists" ~ levs_failure_type[1],
      failure_type %in% "test_date_definite" ~ levs_failure_type[3],
      # All other types are lumped:
      !is.na(failure_type) ~ levs_failure_type[2],
      T ~ NA_character_
    ),
    failure_type_f = factor(failure_type_f, levels = levs_failure_type)
  )

readr::write_rds(
  dft_hdrug_determinations,
  here('data', 'linked_approvals', 'hdrug_determinations.rds')
)










