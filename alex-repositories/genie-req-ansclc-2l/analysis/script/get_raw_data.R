library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

synLogin()

# Get the latest NSCLC release data:

release_not_cohort <- c('Main GENIE cBioPortal Releases')

release_dat <- get_syn_children_df('syn21241322') |>
  select(cohort = name, cohort_id = id) |>
  filter(!(cohort %in% release_not_cohort))

# Go a level deeper
release_dat <- release_dat |>
  mutate(
    children = map(.x = cohort_id, .f = get_syn_children_df)
  ) |>
  unnest(children) |>
  select(contains("cohort"), release = name, release_id = id)

# Just to be safe I'll ignore anything marked sensitive or archived for this.
release_dat <- release_dat |>
  filter(
    !str_detect(
      tolower(release),
      'archived|sensitive'
    )
  )

release_dat %<>%
  filter(cohort %in% "NSCLC" & release %in% "3.1-consortium")

# You could do a single release with the function like this (demo):
# release_saver(
#   cohort = pull(slice(release_dat, 1), cohort),
#   release = pull(slice(release_dat, 1), release),
#   synid = pull(slice(release_dat, 1), release_id)
# )

# We'll do them all at once:
purrr::pwalk(
  .l = list(
    cohort = release_dat$cohort,
    release = release_dat$release,
    synid = release_dat$release_id
  ),
  .f = release_saver
)

##############
# main GENIE #
##############

main_genie_folder <- 'syn70371502' # 19.4 consortium.
df_main_children <- synGetChildren(main_genie_folder) %>%
  as.list %>%
  purrr::map_dfr(.x = ., .f = as_tibble)

# Don't think we need the panel files here.
df_main_children %<>%
  filter(
    name %in%
      c(
        "data_mutations_extended.txt",
        "data_CNA.txt",
        "data_fusions.txt",
        'data_sv.txt',
        'genomic_information.txt',
        'data_clinical_patient.txt',
        'data_clinical_sample.txt'
      )
  )

purrr::walk(
  .x = df_main_children$id,
  .f = \(z) {
    syn_store_help(
      z,
      loc = here("data-raw", 'main_genie')
    )
  }
)
