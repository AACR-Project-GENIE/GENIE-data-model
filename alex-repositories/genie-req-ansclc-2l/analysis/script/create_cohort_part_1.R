library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

library(genzplyr)

ca_ind <- readr::read_csv(
  here(
    'data-raw',
    'bpc',
    'NSCLC',
    '3.1-consortium',
    'cancer_level_dataset_index.csv'
  )
)
cpt <- readr::read_csv(
  here(
    'data-raw',
    'bpc',
    'NSCLC',
    '3.1-consortium',
    'cancer_panel_test_level_dataset.csv'
  )
)
fcpt <- get_first_cpt(ca_ind, cpt, include_sample_id = TRUE)


cohort <- ca_ind %>%
  select(
    phase,
    cohort,
    record_id,
    institution,
    ca_seq,
    dob_ca_dx_days
  )

# flow_track monitors attrition at each step for us.
flow_track <- flow_record_helper(cohort, "BPC NSCLC v3.1")

# We'll work with each case's first sample for simplicity:
cohort %<>%
  left_join(
    .,
    fcpt,
    by = c('record_id', 'ca_seq')
  )

sample_gene <- readr::read_rds(
  here('data', 'genomic', 'sample_gene_test_pos.rds')
)

# first we take a flattening approach to multiple samples.
gene_elig <- sample_gene %>%
  yeet(sample_id %in% cohort$cpt_genie_sample_id) %>%
  mutate(
    # The awkward patterns here deal with NAs.
    # Currently we don't care if someone was tested for ALK, EGFR, etc.
    geno_eligible = KRAS_G12D & !(ALK %in% TRUE) & !(EGFR %in% TRUE)
  )

cohort <- gene_elig %>%
  select(
    cpt_genie_sample_id = sample_id,
    geno_eligible,
    KRAS_G12D,
    EGFR,
    ALK
  ) %>%
  left_join(
    cohort,
    .,
    by = 'cpt_genie_sample_id',
    # many-to-one because oddly some samples map to multiple index cancers.
    relationship = 'many-to-one'
  )

cohort %<>% yeet(KRAS_G12D)

flow_track %<>% flow_record_helper(cohort, "KRAS p.G12D", .)

cohort %<>% yeet(geno_eligible)

flow_track %<>% flow_record_helper(cohort, "No EGFR or ALK detected", .)

dx_stage <- ca_ind %>%
  yeet(stage_dx %in% c("Stage III", "Stage IV")) %>%
  mutate(
    stage_detailed_comb = case_when(
      !is.na(best_ajcc_stage_cd) ~ best_ajcc_stage_cd,
      !is.na(ca_path_group_stage) ~ ca_path_group_stage,
      T ~ NA_character_
    )
  ) %>%
  select(record_id, ca_seq, stage_detailed_comb, stage_dx)

cohort <- left_join(
  cohort,
  dx_stage,
  by = c('record_id', 'ca_seq')
)

# This is inductive - I just looked at the available codes first.
cohort %<>%
  yeet(
    stage_dx %in% "Stage IV" | stage_detailed_comb %in% c("3B", "3C", "IIIB")
  )

flow_track %<>% flow_record_helper(cohort, "Stage 3B+ at dx", .)

# Now off to create our lines...

readr::write_rds(
  cohort,
  here('data', 'cohort', 'cohort_part_1.rds')
)
readr::write_rds(
  flow_track,
  here('data', 'cohort', 'flow_track_part_1.rds')
)
