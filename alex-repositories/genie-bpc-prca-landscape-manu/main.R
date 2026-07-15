# Description: Top level workflow for the project.  Can be converted to a 
#   cleaner workflow later on.
# Author: Alex Paynter

library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

############
# Clinical #
############
# Create a script to manage directories?
source(here('analysis', 'script', 'get_raw_data.R'))
source(here('analysis', 'script', 'process_clinical_data.R'))
source(here('analysis', 'script', 'create_drug_dat.R'))
source(here('analysis', 'script', 'save_sunbursts.R'))

fs::dir_create(here("output"))
rmarkdown::render(
  input = here('analysis', 'report', 'genie-bpc-prca-clinical.Rmd'),
  output_file = '01-genie-bpc-prostate-clinical.html',
  output_dir = here('output')
)


###########
# Genomic #
###########
source(here('analysis', 'script', 'input_sanchez.R')) # manual save of pathways.
source(here('analysis', 'script', 'create_ddr_pathway_mapping.R')) # manual save of pathways.
# Need to do once:
source(here('analysis', 'script', 'reshape_cna.R'))
# # run annotate_oncokb.sh from the command line.  See comments on enviro vars.
source(here('analysis', 'script', 'save_oncokb_annotated_data.R'))
# After that the files can be obtained from synid in save_oncokb_annotated_data.R

source(here('analysis', 'script', 'create_gene_panel_dat.R'))
source(here('analysis', 'script', 'process_oncokb_output.R'))
source(here('analysis', 'script', 'add_tmb_to_cpt.R'))
# The above file calls some bioconductor packages that might interfere with 
#  dplyr calls.  Possibly need to reload R after running it.
rmarkdown::render(
  input = here('analysis', 'report', 'genie-bpc-prca-genomic.Rmd'),
  output_file = '02-genie-bpc-prostate-genomic.html',
  output_dir = here('output')
)


############
# Survival #
############
# quarto::quarto_render(
#   input = here('analysis', 'report', 'genie-bpc-prca-survival.qmd'),
#   output_format = 'html',
#   output_file = '03-genie-bpc-prostate-survival.html',
# )

source(here('analysis', 'script', 'basic_survival_descriptives.R'))
source(here('analysis', 'script', 'create_surv_hrd_dataset.R'))
fs::file_move(
  path = here('analysis', 'report', 'genie-bpc-prca-survival.html'),
  new_path = here('output', '03-genie-bpc-prostate-survival.html')
)




#########
# Other #
#########
# Current version of quarto stunningly does not have output_dir, so we render and move.
# A pre-release version has this, so this can be fixed soon.
# quarto::quarto_render(
#   input = here('analysis', 'report', 'genie-bpc-prca-met-class.qmd'),
#   output_format = 'html',
# )
# Nope, even that doesn't work for self contained.  What a mess.
# Render it and move it.
fs::file_move(
  path = here('analysis', 'report', 'genie-bpc-prca-met-class.html'),
  new_path = here('output', '99-genie-bpc-prostate-met-class.html')
)




