# Description:  Calculate the panel size and TMB for each sample.
#   This code is adapated from Alex Baras's code used in early genie releases.


library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

# There was an error with 2.4 for dbplyr when calling getBM() below. 
# This fixed it:
# devtools::install_version("dbplyr", version = "2.3.4")

library(curl)
library(biomaRt)
library(VariantAnnotation) # bioconductor.

# So far this part is failing to run.  When ready, we can copy from other cohorts.