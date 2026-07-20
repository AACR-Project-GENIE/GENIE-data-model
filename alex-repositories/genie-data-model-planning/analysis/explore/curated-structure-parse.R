library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

library(stringdist)


# Just as an example, MSK NSCLC2 data
msk_nsclc2_path <- here('data-raw', 'bpc', 'step1-curated', 'NSCLC2', 'MSK')
redcap_ex <- readr::read_csv(
  here(msk_nsclc2_path, dir(msk_nsclc2_path))
)

dat_dict_non_phi <- readr::read_csv(
  here(
    'data-raw',
    'curated-manual',
    'nonphi_v388_PRISSMM_Dictionary_NSCLC_BPC_P2.csv'
  )
) %>%
  rename(
    field_name = `Variable / Field Name`
    # won't bother with the rest for now.
  )

dat_dict <- readr::read_csv(
  here(
    'data-raw',
    'curated-manual',
    'v388_PRISSMM_Dictionary_NSCLC_BPC_P2.csv'
  )
) %>%
  # jeez these headers suck.
  rename(
    field_name = `Variable / Field Name`,
    form = `Form Name`,
    field_type = `Field Type`,
    # this isn't actually limited to valid values, there's calculations in here for some odd reason:
    valid_vals = `Choices, Calculations, OR Slider Labels`,
    required = `Required Field?`
  ) %>%
  mutate(
    required = if_else(required %in% "y", T, F) # fixing y/NA coding.
  )

# How do the phi and non-phi versions relate?
setdiff(dat_dict_non_phi$field_name, dat_dict$field_name) # yeah, expected.
setdiff(dat_dict$field_name, dat_dict_non_phi$field_name) #
# The phi one seems to be a subset of the main one.  Makes sense.

cols_not_in_dict <- tibble(
  var = setdiff(colnames(redcap_ex), dat_dict$field_name)
)
cols_not_in_dict %<>%
  mutate(closest = find_closest_str(var, dict = dat_dict$field_name))
cols_not_in_dict %>% print(n = 500)

# Lets see if we can find a match for all the double underscore fields.
stub_vars <- tibble(
  stub = colnames(redcap_ex) %>%
    .[str_detect(., '\\_\\_')]
) %>%
  separate(stub, into = c('stub', 'num'), sep = '\\_\\_\\_') %>%
  mutate(num = as.numeric(num)) %>%
  group_by(stub) %>%
  summarize(
    min = min(num),
    max = max(num)
  )

stub_vars %<>%
  mutate(
    closest = find_closest_str(stub, dict = dat_dict$field_name),
    exact_match = closest == stub
  )

# Ok great, now we have to fix the data dictionary to accomodate all these:
stub_merge <- stub_vars %>%
  slice(rep(1:n(), times = max)) %>%
  group_by(stub) %>%
  mutate(var = paste0(stub, '___', row_number())) %>%
  ungroup(.) %>%
  select(var, stub)

stub_merge <- left_join(
  stub_merge,
  dat_dict,
  by = c(stub = 'field_name')
) %>%
  select(-stub) %>%
  rename(field_name = var)

dat_dict <- bind_rows(
  dat_dict,
  stub_merge
) %>%
  # not strictly required, but we can remove the stubs now that we have the numbered versions in there:
  filter(!(field_name %in% stub_vars$stub))

# Then there are a few fields that are just hard manual.  No real explanation why these aren't in the data dictionary, but they're not.
# using tribble here just in case I want to add a second or third field.
undefined_vars <- tribble(
  ~field_name,
  'redcap_repeat_instrument',
  'redcap_repeat_instance',
  'curation_initiation_eligibility_complete',
  'patient_characteristics_complete',
  'cancer_diagnosis_complete',
  'ca_directed_drugs_complete',
  'ca_directed_radtx_complete',
  'prissmm_imaging_complete',
  'prissmm_pathology_complete',
  'prissmm_med_onc_assessment_complete',
  'prissmm_tumor_marker_complete',
  'cancer_panel_test_complete',
  'curation_completion_complete',
  'quality_assurance_complete'
)

undefined_vars %<>%
  mutate(
    field_note = "Not defined - this row was added to the data dictionary before QC."
  )

dat_dict <- bind_rows(
  dat_dict,
  undefined_vars
)

# Between these two types of fixes everything is now in:
setdiff(colnames(redcap_ex), dat_dict$field_name)

# Note that the dictionary is still a superset - that's considered OK:
setdiff(dat_dict$field_name, colnames(redcap_ex))

# curious about a couple things here:
count(dat_dict, field_type)
count(dat_dict, form)
count(dat_dict, valid_vals) %>% print(n = 500)
# you can sort out what's in this weird combo column by the field type:
count(dat_dict, field_type, valid_vals) %>% print(n = 500)
# note that yes/no has valid values too, so you'd porbably wnat to put those in.

count(dat_dict, `Branching Logic (Show field only if...)`) # 730 options
count(dat_dict, `Text Validation Min`)
count(dat_dict, `Text Validation Max`)
count(dat_dict, `Required Field?`)

# Surprising number, even with my version being wrong.
# May look into string matching or something, should also just ask.
setdiff(colnames(redcap_ex), dat_dict$field_name)
setdiff(dat_dict$field_name, colnames(redcap_ex))
setdiff(letters[1:3], letters[1:2])

colnames(redcap_ex)[str_detect(
  colnames(redcap_ex),
  '\\_\\_'
)] %in%
  dat_dict$field_name
# So it's all the double underscores that get left out.  I've seen some of these
#   in the production data though, I'm almost positive.

# what about the merged data - do these fields show up there?

dat_merged <- readr::read_csv(
  here(
    'data-raw',
    'bpc',
    'step2-merged',
    'NSCLC2BPCIntake_data.csv'
  )
)

setdiff(colnames(redcap_ex), dat_dict$field_name) %in% colnames(dat_merged)
# Yes, those still show up in the merged data.
setdiff(colnames(redcap_ex), colnames(dat_merged)) # 0 cols
setdiff(colnames(dat_merged), colnames(redcap_ex)) # one col that basically says the site name
# So the column names for those two are almost identical
