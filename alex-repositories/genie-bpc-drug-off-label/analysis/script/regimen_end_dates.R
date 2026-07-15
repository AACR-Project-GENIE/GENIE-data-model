library(fs); library(purrr); library(here);
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

dft_clin_dat <- readr::read_rds(
  here('data-raw', 'clin_dat_untouched.rds')
)

dft_os_dx <- dft_clin_dat %>%
  filter(short_name %in% "ca_ind") %>%
  select(dat) %>%
  mutate(
    dat = purrr::map(
      .x = dat,
      .f = \(z) 
       select(z, cohort, record_id, ca_seq, 
              any_of(c("ca_cadx_int", "dob_ca_dx_days")),
              os_dx_status, tt_os_dx_days)
    )
  ) %>%
  unnest(dat)






# Next we have a big paste to get the hdrug data from regimen data:
dft_clin_dat_wide <- dft_clin_dat %>%
  select(cohort, short_name, dat) %>%
  pivot_wider(
    names_from = short_name,
    values_from = dat
  )

dft_clin_dat_wide %<>%
  mutate(
    # hreg = harmonized regimen data.
    hreg = purrr::map(
      .x = reg,
      .f = \(x) {
        breast_regimen_fix(x) 
      }
    )
  )

# Later on we will need a range of possible values for birth date.  Just add now.
dft_clin_dat_wide %<>%
  mutate(
    pt = purrr::map(
      .x = pt,
      .f = add_birth_date_range
    )
  )

dft_clin_dat_wide %<>%
  mutate(
    # hreg = harmonized regimen data.
    hreg = purrr::map(
      .x = hreg,
      .f = select_hreg_columns
    )
  )

# Add the versions of the regimen variables that include last known administration.
dft_clin_dat_wide %<>%
  mutate(
    hreg = purrr::map(
      .x = hreg,
      .f = \(x) {
        add_regimen_lastadm_vars(x)
      }
    )
  )

# Create a drug-keyed dataset:
dft_clin_dat_wide %<>%
  mutate(
    hdrug = purrr::map(
      .x = hreg,
      .f = create_drug_dat
    )
  )

# Trim whitespace from agent names.  Now a bigger problem in NSCLCv3.1
dft_clin_dat_wide %<>%
  mutate(
    hdrug = purrr::map(
      .x = hdrug,
      .f = \(z) {
        mutate(z, agent = str_trim(agent))
      }
    )
  )

# Change "HCL" to Hydrochloride now that we have round 2 of NSCLC... just why?
dft_clin_dat_wide %<>%
  mutate(
    hdrug = purrr::map(
      .x = hdrug,
      .f = \(z) {
        mutate(z, agent = str_replace(agent, "HCL", "Hydrochloride"))
      }
    )
  )

dft_hdrug <- dft_clin_dat_wide %>%
  select(cohort, hdrug) %>% 
  unnest(hdrug)

# Merge, figure out if any of the regimens end after censoring/death.
purrr::map(dft_hdrug,.f = \(z) sum(is.na(z)))

dft_hdrug %>%
  mutate(na_ct = is.na(dx_drug_end_or_lastadm_int)) %>%
  tabyl(na_ct, cohort)

# Probably can't do much with unknown end dates so:
dft_hdrug %<>%
  filter(!is.na(dx_drug_end_or_lastadm_int)) %>%
  select(-tt_os_days) # from drug start time - not helpful really.

dft_joined <- dft_os_dx %>%
  select(cohort, record_id, ca_seq, os_dx_status, tt_os_dx_days) %>%
  left_join(
    dft_hdrug, .,
    by = c('cohort', 'record_id', 'ca_seq')
  ) 

dft_joined %<>%
  mutate(diff = dx_drug_end_or_lastadm_int - tt_os_dx_days) %>%
  filter(diff > 0) 

ggplot(
  dft_joined,
  aes(x = diff, y = os_dx_status)
) + 
  geom_point()

dft_joined %>% count(os_dx_status)

dft_joined %>% filter(os_dx_status %in% 1) %>% glimpse

  


