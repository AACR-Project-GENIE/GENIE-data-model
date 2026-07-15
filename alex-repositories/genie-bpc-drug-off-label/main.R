# Description: Top level workflow for the project.
# Author: Alex Paynter

library(purrr)
library(here)
library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

# Get the raw materials:
source(here('analysis', 'script', 'create_folders.R'))
# source(here('analysis', 'script', 'get_raw_data.R'))

# Preprocessing of BPC and indications data:
source(here('analysis', 'script', 'process_clinical_data.R'))
source(here('analysis', 'script', 'derive_first_approvals.R'))
source(here('analysis', 'script', 'remove_old_drugs_save_hdrug.R'))
source(here('analysis', 'script', 'find_concomitant_drugs.R')) # takes a minute.
source(here('analysis', 'script', 'process_genomic_data.R'))
source(here('analysis', 'script', 'crosswalk_indications.R'))

# Extracting biomarker flags, linking to exposures.
source(here('analysis', 'script', 'derive_gene_test_flags.R'))
source(here('analysis', 'script', 'derive_mutation_pos_flags.R'))
source(here('analysis', 'script', 'derive_breast_biomarker_regimen_flags.R'))
source(here('analysis', 'script', 'derive_lung_biomarker_regimen_flags.R'))
source(here('analysis', 'script', 'join_regimen_biomarker_flags.R'))

# Test exposures:
source(here('analysis', 'script', 'declare_with_tests.R'))
source(here('analysis', 'script', 'declare_biom_tests.R'))
source(here('analysis', 'script', 'join_and_test_exposures.R'))

# Analysis of tests and when exposures are knocked out:
source(here('analysis', 'script', 'stepwise_test_analysis.R'))
source(here('analysis', 'script', 'stepwise_tree_explanation.R'))

# Post-hoc analysis of special cases:
source(here('analysis', 'script', 'scrape_guidelines_panc_cape.R'))
source(here('analysis', 'script', 'scrape_guidelines_colon_cape.R'))
source(here('analysis', 'script', 'scrape_guidelines_rectal_cape.R'))
source(here('analysis', 'script', 'biomarker_driven_use.R'))

# no longer an rmarkdown doc:
# rmarkdown::render(
#   input = here('analysis', 'report', '01-genie-bpc-off-label.Rmd'),
#   output_file = here('output', '01-genie-bpc-off-label.html')
# )

# Render for 02 report, 03 report, etc.

source(here('analysis', 'script', 'manu-fig1-restriction-impact.R'))
source(here('analysis', 'script', 'manu-fig2-restriction-impact.R'))
# Figure 3 is still living in the 03 report - needs to be puleld out.
