library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

# Load and create the hdrug cohort data:
dft_hdrug_cohort <- readr::read_rds(
  here('data', 'cohort', 'hdrug.rds')
)




# The list of all cases hasn't been processed into an hdrug dataset yet, so we 
#   need to repeat some steps (skipping removals):
dft_all_cases <- readr::read_rds(
  here('data', 'no_ca_seq_filter', 'clin_dat_just_before_drug_removal.rds')
)
dft_all_cases %<>%
  mutate(
    # hreg = harmonized regimen data.
    hreg = purrr::map(
      .x = hreg,
      .f = select_hreg_columns
    )
  )
# Add the versions of the regimen variables that include last known administration.
dft_all_cases %<>%
  mutate(
    hreg = purrr::map(
      .x = hreg,
      .f = \(x) {
        add_regimen_lastadm_vars(x)
      }
    )
  )
# Create a drug-keyed dataset:
dft_all_cases %<>%
  mutate(
    hdrug = purrr::map(
      .x = hreg,
      .f = create_drug_dat
    )
  )
# Trim whitespace from agent names.  Now a bigger problem in NSCLCv3.1
dft_all_cases %<>%
  mutate(
    hdrug = purrr::map(
      .x = hdrug,
      .f = \(z) {
        mutate(z, agent = str_trim(agent))
      }
    )
  )
# Change "HCL" to Hydrochloride now that we have round 2 of NSCLC... just why?
dft_all_cases %<>%
  mutate(
    hdrug = purrr::map(
      .x = hdrug,
      .f = \(z) {
        mutate(z, agent = str_replace(agent, "HCL", "Hydrochloride"))
      }
    )
  )
# Fix one drug names which cause particular problems down the line.
# "Etoposide" only has phosphate appended once and it causes some 
#    headaches on linking to the Warner materials to have both.  Easier to
#    change the data in this one case.
dft_all_cases %<>%
  mutate(
    hdrug = purrr::map(
      .x = hdrug,
      .f = \(x) {
        x %>%
          mutate(
            agent = if_else(
              agent %in% "Etoposide Phosphate",
              "Etoposide",
              agent
            )
          )
      }
    )
  )

# Now we can get the drug list:
dft_hdrug_all <- dft_all_cases %>%
  select(cohort, hdrug) %>%
  unnest(hdrug) 

dft_hdrug_all %<>% fix_cohort_names(.)

dft_hdrug_cohort %<>% add_dob_int_hdrug(.)
dft_hdrug_all %<>% add_dob_int_hdrug(.)
  
# One example where we know overlaps exist, and it's beyond the cohort ones.
# get_overlaps_one_row(
#   row_to_check = (
#     dft_hdrug_cohort %>%
#        filter(record_id %in% "GENIE-DFCI-008411") %>%
#        slice(2)
#     ),
#   dat_overlaps = dft_hdrug_all
# )

# This takes quite a while to run.  That's ok, we're not in a huge hurry for
#   something we'll be doing once - it's about one minute on a personal computer.
# A solution where you join then filter is probably going to be faster.
list_overlaps <- dft_hdrug_cohort %>%
  group_by(row_number()) %>%
  group_nest(., .key = "drug_use_row") %>%
  pull("drug_use_row") %>%
  purrr::map(
    .x = .,
    .f = \(z) {
      get_overlaps_one_row(
        row_to_check = z,
        dat_overlaps = dft_hdrug_all
      )
    }
  )

dft_hdrug_cohort %<>%
  mutate(
    drug_overlaps = list_overlaps,
    drug_overlaps_vec = purrr::map_chr(
      .x = drug_overlaps,
      .f = \(z) paste(z, collapse = ", ")
    ),
    num_overlaps = purrr::map_dbl(
      .x = drug_overlaps,
      .f = length
    )
  )

readr::write_rds(
  dft_hdrug_cohort,
  here('data', 'cohort', 'hdrug_with_conmeds.rds')
)


# We will also build an "index" of drugs, which is very useful for 
#   making sure we spell things right.  It get confusing with the indications
#   name and the data ones (conmed ones) being different.
dft_drug_index <- list_overlaps %>% 
  unlist %>% 
  tibble(agent = .) %>% 
  count(agent, name = "n_overlap")

dft_drug_index <- dft_hdrug_cohort %>%
  count(agent, name = "n_exposure") %>%
  full_join(., dft_drug_index, by = 'agent')

dft_drug_index %<>% arrange(agent)

readr::write_rds(
  dft_drug_index,
  here('data', 'cohort', 'drug_index.rds')
)




  




