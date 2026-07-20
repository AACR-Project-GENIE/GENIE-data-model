


library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source) # also loads lots of packages.

read_wrap_clin <- function(p) {read_rds(file = here("data", 'clin', p))}
dft_pt <- read_wrap_clin("dft_pt.rds")
dft_ca_ind <- read_wrap_clin("dft_ca_ind.rds")
# we're taking the augmented version, which has TMB columns added.  The existing info is the same.
dft_cpt <- read_wrap_clin("dft_cpt_aug.rds")
dft_img <- read_wrap_clin("dft_img.rds")


dft_first_cpt <- get_first_cpt(
  ca_ind_dat = dft_ca_ind,
  cpt_dat = dft_cpt
)

dft_surv_met <- dft_ca_ind %>%
  filter(!is.na(os_adv_status)) %>%
  left_join(., dft_first_cpt, by = c("record_id", "ca_seq")) %>%
  select(
    record_id, ca_seq, dx_cpt_rep_yrs,
    os_adv_status, tt_os_adv_yrs
  )

dft_met_timing <- make_dmet_status_block(ca_ind_dat = dft_ca_ind) %>% 
  filter(dmet_status %in% "Distant Metastasis") %>%
  select(record_id, ca_seq, dx_met_yrs = dx_block_start)

check_unique_key(dft_surv_met, record_id, ca_seq)
check_unique_key(dft_met_timing, record_id, ca_seq)

# These datasets should have the same cohorts if everything is as expected.
# Let's see:
anti_join(
  dft_met_timing,
  dft_surv_met,
  by = c("record_id", "ca_seq")
)
missing_from_met <- anti_join(
  dft_surv_met,
  dft_met_timing,
  by = c("record_id", "ca_seq")
) %>%
  select(record_id, ca_seq)
missing_from_met

# All these people appear to be Stage IV:
left_join(
  missing_from_met,
  dft_ca_ind,
  by = c("record_id", 'ca_seq')
) %>%
  select(record_id, ca_seq, stage_dx, stage_dx_iv, dmets_stage_i_iii) %>%
  tabyl(stage_dx_iv)

dft_ca_ind %>% 
  filter(stage_dx_iv %in% "Stage IV" & !(ca_dmets_yn %in% "Yes")) %>%
  select(record_id, ca_seq, dmets_stage_i_iii) %>% 
  tabyl(dmets_stage_i_iii)

dft_ca_ind %>% 
  filter(stage_dx_iv %in% "Stage IV" & !(ca_dmets_yn %in% "Yes")) %>%
  tabyl(dx_to_dmets_yrs) # all missing

dft_ca_ind %>% 
  filter(stage_dx_iv %in% "Stage IV" & !(ca_dmets_yn %in% "Yes")) %>%
  tabyl(dx_to_dmets_abdomen_yrs) # weirdly NOT all missing.

# For Stage IV with no mets advanced disease just starts at diagnosis.
dft_ca_ind %>% 
  filter(stage_dx_iv %in% "Stage IV" & !(ca_dmets_yn %in% "Yes")) %>%
  select(record_id, ca_seq, tt_os_adv_yrs, tt_os_dx_yrs) %>%
  mutate(diff = tt_os_adv_yrs - tt_os_dx_yrs) %>%
  tabyl(diff)

# Checking that my classification is collectively exhaustive:
lev_met_code <- c(
  'never_met',
  'met_later',
  'met_at_dx',
  'tricky_ones'
)

dft_met_class <- dft_ca_ind %>%
  mutate(
    met_coding_class = case_when(
      !(stage_dx_iv %in% "Stage IV") & dmets_stage_i_iii %in% 0 ~ lev_met_code[1],
      !(stage_dx_iv %in% "Stage IV") & dmets_stage_i_iii %in% 1 ~ lev_met_code[2],
      stage_dx_iv %in% "Stage IV" & ca_dmets_yn %in% "Yes" ~ lev_met_code[3],
      stage_dx_iv %in% "Stage IV" & !(ca_dmets_yn %in% "Yes") ~ lev_met_code[4],
      T ~ NA_character_
    )
  ) %>%
  select(
    record_id, ca_seq, 
    stage_dx_iv, ca_dmets_yn, dmets_stage_i_iii, 
    met_coding_class
  ) %>%
  mutate(met_coding_class = factor(met_coding_class, levels = lev_met_code))

tabyl(dft_met_class, met_coding_class)

dft_met_class %>% filter(is.na(met_coding_class)) 
# for some reason has no info - OK fine.

subject_glimpse_help <- function(record, dat_ca_ind = dft_ca_ind) {
  dft_ca_ind %>%
    filter(record_id %in% record) %>%
    select(
      record_id, ca_seq, stage_dx_iv, dmets_stage_i_iii, ca_dmets_yn, 
      matches("dx_to_dmet.*_yrs")
    ) %>%
    glimpse
}

subject_glimpse_help('GENIE-DFCI-010669')

      

# Attempt to reconstruct dx_to_dmets_yrs using the site variables:
dft_reconstruct <- dft_ca_ind %>%
  filter(!(stage_dx_iv %in% "Stage IV") & dmets_stage_i_iii %in% 1) %>%
  select(record_id, ca_seq, matches("dx_to_dmet.*_yrs")) %>%
  pivot_longer(
    cols = -c(record_id, ca_seq)
  ) %>%
  filter(!(name %in% 'dx_to_dmets_yrs')) %>%
  group_by(record_id, ca_seq) %>%
  arrange(value) %>%
  slice(1) %>% 
  ungroup(.) %>%
  rename(
    var_for_min = name,
    site_min_yrs = value
  )

dft_reconstruct <- dft_ca_ind %>%
  filter(!(stage_dx_iv %in% "Stage IV") & dmets_stage_i_iii %in% 1) %>%
  select(record_id, ca_seq, dx_to_dmets_yrs) %>%
  full_join(
    .,
    dft_reconstruct,
    by = c("record_id", "ca_seq")
  )

# Cases where the reconstruction fails:
dft_reconstruct %>%
  filter(xor(is.na(dx_to_dmets_yrs), is.na(site_min_yrs))) %>% glimpse
dft_reconstruct %>%
  filter(abs(dx_to_dmets_yrs - site_min_yrs) > 0.5/365.25) %>% glimpse
# So actually 3.

# Here are the relevant rows from those patients:
subject_glimpse_help('GENIE-DFCI-010669')
subject_glimpse_help("GENIE-DFCI-038902")
subject_glimpse_help("GENIE-MSK-P-0002303")

# Do any of the ICD codes used match up to the Distant (Rare and nos) column?
dft_img %>% 
  # filter(record_id %in% 'GENIE-DFCI-010669') %>%
  # filter(record_id %in% 'GENIE-DFCI-038902') %>%
  filter(record_id %in% "GENIE-MSK-P-0002303") %>%
  select(record_id, matches('image_casite')) %>% 
  pivot_longer(
    cols = -record_id
  ) %>%
  filter(!is.na(value)) %>%
  arrange(value) %>%
  print(n = 500)

# Checking:  Do we ever see codes marked "Distant (rare and nos)" in the data guide.
dft_img %>% 
  select(record_id, matches('image_casite')) %>% 
  pivot_longer(
    cols = -record_id
  ) %>%
  filter(!is.na(value)) %>%
  count(value) %>%
  arrange(value) %>%
  print(n = 500)
# Yes, we have 'C15.9 Esophagus NOS'!
# Also 'C44.9 Skin NOS'!




dft_stage_iv_recon <- dft_ca_ind %>%
  filter(stage_dx_iv %in% "Stage IV" & !(ca_dmets_yn %in% "Yes")) %>%
  select(record_id, ca_seq, matches("dx_to_dmet.*_yrs")) %>%
  pivot_longer(
    cols = -c(record_id, ca_seq)
  ) %>%
  filter(!(name %in% 'dx_to_dmets_yrs')) %>%
  group_by(record_id, ca_seq) %>%
  arrange(value) %>%
  slice(1) %>% 
  ungroup(.) %>%
  rename(
    var_for_min = name,
    site_min_yrs = value
  )

  


