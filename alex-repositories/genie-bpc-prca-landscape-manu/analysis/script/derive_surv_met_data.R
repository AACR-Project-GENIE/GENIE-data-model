


library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source) # also loads lots of packages.

read_wrap_clin <- function(p) {read_rds(file = here("data", 'clin', p))}
dft_pt <- read_wrap_clin("dft_pt.rds")
dft_ca_ind <- read_wrap_clin("dft_ca_ind.rds")
# we're taking the augmented version, which has TMB columns added.  The existing info is the same.
dft_cpt <- read_wrap_clin("dft_cpt_aug.rds")
dft_img <- read_wrap_clin("dft_img.rds")

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
lev_met <- 
dft_ca_ind %>%
  mutate(
    met_coding_class = case_when(
      !(stage_dx_iv %in% "Stage IV") & dmets_stage_i_iii %in% 0 ~ "never_met",
      !(stage_dx_iv %in% "Stage IV") & dmets_stage_i_iii %in% 1 ~ "met_later",
      stage_dx_iv %in% "Stage IV" & ca_dmets_yn %in% "Yes" ~ "met_at_dx",
      stage_dx_iv %in% "Stage IV" & !(ca_dmets_yn %in% "Yes") ~ "the_tricky_ones",
      T ~ NA_character_
    )
  )
      
      

met_site_vars <- dft_ca_ind %>% 
  select(record_id, ca_seq, matches("dx_to_dmets.*_yrs")) %>%
  filter(!is.na(dx_to_dmets_yrs)) 

met_site_vars %>% 
  pivot_longer(
    cols = -c(record_id, ca_seq)
  ) %>%
  group_by(record_id, ca_seq) %>%
  filter(!is.na(value)) %>%
  summarize(num_vars_complete = n(), .groups = "drop") %>%
  arrange(num_vars_complete)

dft_ca_ind %>%
  filter(record_id %in% "GENIE-DFCI-010669", ca_seq %in% 0) %>% glimpse


dft_img %>% tabyl(image_ca) # k good.

dft_img %>% select(matches("^image_casite")) %>%
  pivot_longer(
    cols = everything()
  ) %>%
  filter(!is.na(value)) %>%
  count(value, sort = T)


dft_surv_met <- left_join(dft_surv_met,
                          dft_met_timing,
                          by = c('record_id', 'ca_seq'))

dft_surv_met %>% 
  remove_trunc_gte_event(
    trunc_var = 'dx_cpt_rep_yrs',
    event_var = 'tt_os_adv_yrs'
  )

surv_obj_os_dx <- with(
  dft_surv_dx,
  Surv(
    time = dx_cpt_rep_yrs,
    time2 = tt_os_dx_yrs,
    event = os_dx_status
  )
)

gg_os_dx_stage <- plot_one_survfit(
  dat = dft_surv_dx,
  surv_form = surv_obj_os ~ stage_dx_iv,
  plot_title = "OS from diagnosis",
  plot_subtitle = "Adjusted for (independent) delayed entry"
)