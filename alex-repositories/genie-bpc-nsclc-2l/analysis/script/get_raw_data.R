# Description: Grabs the raw data from Synapse.
# Author: Alex Paynter

library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

synLogin()

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

# For each release we'll go into the folder
release_saver <- function(
  cohort,
  release,
  synid
) {
  subfold <- get_syn_children_df(synid)

  subfold <- subfold |>
    filter(str_detect(name, 'clinical_data'))

  if (nrow(subfold) > 1) {
    cli_abort("Multiple clinical data folders found.")
  } else if (nrow(subfold) < 1) {
    cli_abort("No clinical data folders found.")
  }

  clin_dat_dir <- get_syn_children_df(subfold$id)

  release_helper <- function(
    synid
  ) {
    release_dir <- here(
      "data-raw",
      # because we're defining this function in the context of a single release, this works even though cohort and release aren't arguments.
      cohort,
      release
    )

    fs::dir_create(release_dir)
    synGet(
      entity = synid,
      downloadLocation = release_dir,
      ifcollision = "overwrite.local"
    )
  }

  purrr::walk(
    clin_dat_dir$id,
    release_helper
  )
}

release_dat %<>%
  filter(cohort %in% "NSCLC") %>%
  filter(release %in% c('3.1-consortium'))

# We'll do them all at once:
purrr::pwalk(
  .l = list(
    cohort = release_dat$cohort,
    release = release_dat$release,
    synid = release_dat$release_id
  ),
  .f = release_saver
)


get_and_save_dataset <- function(
  synid,
  subfolder,
  v = NULL
) {
  if (!is.null(v)) {
    synid = paste0(synid, '.', v)
  }
  synGet(
    entity = synid,
    downloadLocation = here(
      "data-raw",
      subfolder
    ),
    ifcollision = "overwrite.local"
  )
}


# Add in genomic data (all tied to 15.0 public)
synid_maf <- "syn9734426.112"
synid_cna <- "syn9734422.114"
synid_clin_samp <- "syn9735027.121"
# note: the bed file is newer than the maf version.
# Just trying this to see if we can resolve some seq assay id issues.
synid_bed <- 'syn9734427.109'

fs::dir_create('data-raw', 'main_genie')

# Look at this beauty of versions:
get_and_save_dataset(synid_maf, 'main_genie')
get_and_save_dataset(synid_cna, 'main_genie')
get_and_save_dataset(synid_clin_samp, 'main_genie')
get_and_save_dataset(synid_bed, 'main_genie')
