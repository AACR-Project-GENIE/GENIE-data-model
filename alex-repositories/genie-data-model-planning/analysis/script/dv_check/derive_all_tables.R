# Layer 2 is the derived versions of the split data.
# This includes derived variables, variable typing, etc.
library(geniedv)
library(fs)
library(purrr)
purrr::walk(.x = fs::dir_ls(fs::path("R")), .f = source)

out_dir_dv <- path('data', 'dv', 'layer_2_derived_tables')
fs::dir_create(out_dir_dv)

nested_splits <- readr::read_rds(
  path('data', 'dv', 'layer_1_split_tables', 'nested_splits.rds')
)

nested_splits <- nested_splits %>%
  select(site, nested_split_data) %>%
  unnest(nested_split_data)

# Seems like there might be a nicer way to write this with switch()
dv_code_dispatch <- function(tab, dd_sub, form_name) {
  if (form_name %in% "patient") {
    build_dv_tab_pt(tab, dd_sub)
  } else if (form_name %in% "cancer_diagnosis") {
    build_dv_tab_ca_dx(tab, dd_sub) |>
      # special lung wrapper to integrate TNM backup to stage_dx and stage_dx_iv.
      derive_stage_dx_nsclc()
  } else if (form_name %in% "ca_directed_drugs") {
    build_dv_tab_reg(tab, dd_sub)
  } else if (form_name %in% "ca_directed_radtx") {
    build_dv_tab_rad(tab, dd_sub)
  } else if (form_name %in% "prissmm_pathology") {
    build_dv_tab_path(tab, dd_sub)
  } else if (form_name %in% "prissmm_imaging") {
    build_dv_tab_img(tab, dd_sub)
  } else if (form_name %in% "prissmm_med_onc_assessment") {
    build_dv_tab_med_onc(tab, dd_sub)
  } else if (form_name %in% "cancer_panel_test") {
    build_dv_tab_cpt(tab, dd_sub)
  } else {
    NULL
  }
}

# Example of one use:
# dv_code_dispatch(
#   tab = nested_splits$tab[[2]],
#   dd_sub = nested_splits$dat_dict_sub[[2]],
#   form_name = nested_splits$form_in_extract[[2]]
# )

# Doing them all:
nested_splits <- nested_splits %>%
  mutate(
    dv_tab = purrr::pmap(
      .l = list(tab = tab, dd_sub = dat_dict_sub, form_name = form_in_extract),
      .f = dv_code_dispatch
    )
  )

readr::write_rds(nested_splits, path(out_dir_dv, 'nested_dv.rds'))

multisite_tables <- nested_splits %>%
  # keeping site temporarily just to have unique rows.
  select(site, form_in_extract, dv_tab) %>%
  group_by(form_in_extract) %>%
  summarize(dv_tab = list(bind_rows(dv_tab)))

ca_split <- multisite_tables %>%
  filter(form_in_extract %in% 'cancer_diagnosis') %>%
  pull(dv_tab) %>%
  .[[1]] %>%
  split_ca_all(.)

multisite_tables %<>%
  filter(!(form_in_extract %in% 'cancer_diagnosis')) %>%
  add_row(form_in_extract = 'ca_ind', dv_tab = list(ca_split$ca_ind)) %>%
  add_row(form_in_extract = 'ca_non_ind', dv_tab = list(ca_split$ca_non_ind))

multisite_tables %<>%
  filter(purrr::map_dbl(dv_tab, nrow) > 0) %>%
  mutate(name = dat_name_to_short(form_in_extract)) %>%
  arrange(name)

multisite_tables %<>% select(name, dv_tab)

multisite_list <- pull(multisite_tables, dv_tab)
names(multisite_list) <- pull(multisite_tables, name)

multisite_list <- propagate_ca_seq(multisite_list)

multisite_list <- derive_dx_drug_int(
  multisite_list
)

multisite_list <- derive_dx_cpt_int(
  multisite_list
)

multisite_list <- add_dmets_ca_ind(
  tables = multisite_list,
  cohort_ca_types = c("Non Small Cell Lung Cancer", "Lung Cancer, NOS"),
  only_full_dmet_scans = F, # made it worse to do TRUE, surprisingly they're taking partial dmet scans.
  include_indeterminate = F,
  first_cancer_only = T
)


readr::write_rds(
  multisite_list,
  here(out_dir_dv, 'table_list.rds')
)
