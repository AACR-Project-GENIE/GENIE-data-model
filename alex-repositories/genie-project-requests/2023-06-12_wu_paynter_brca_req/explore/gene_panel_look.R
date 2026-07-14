# Description:  Pull the clinical data and save it.  Simple.
# Author: Alex Paynter

synid_releases <- "syn7492881"
# output_location_synid <- "syn51317177" # in 'GENIE BioPharma Collaborative Internal' > requests

library(synapser)
library(readxl)
library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)
library(magrittr)
library(glue)
library(fs)
library(here)
library(ggplot2)

purrr::walk(fs::dir_ls(here('R')), .f = source)

synLogin()

dft_releases <- get_syn_children_df(synid_releases)

# Recurse to subfolders:
dft_releases %<>%
  rename(major = name) %>%
  mutate(
    child_df = purrr::map(.x = id, .f = get_syn_children_df)
  ) %>%
  select(-c(id, createdOn)) %>%
  unnest(child_df) %>%
  rename(
    minor = name,
    minor_id = id,
    minor_createdOn = createdOn
  )

incl_releases <- c("14.1-public")
dft_releases %<>%
  filter(minor %in% incl_releases)

dft_gene_panels_in_release <- get_syn_children_df(dft_releases$minor_id) %>%
  filter(str_detect(name, "^data_gene_panel_")) %>%
  rename(filename = name) %>%
  mutate(
    panel_string = str_replace_all(filename, "^data_gene_panel_", ""),
    panel_string = str_replace_all(panel_string, ".txt$", "")
  )

dft_samp <- get_syn_children_df(dft_releases$minor_id) %>%
  filter(name %in% 'data_clinical_sample.txt') %>%
  pull(id) %>%
  get_synapse_entity_txt(.)

dft_assay_in_data <- janitor::tabyl(dft_samp, seq_assay_id)

# Reported as missing by collaborator:
setdiff(
  dft_assay_in_data$seq_assay_id, 
  dft_gene_panels_in_release$panel_string
)

setdiff(
  dft_gene_panels_in_release$panel_string,
  dft_assay_in_data$seq_assay_id 
)


dft_mut <- get_syn_children_df(dft_releases$minor_id) %>%
  filter(str_detect(name, "^data_mutations_extended")) %>%
  pull(id) %>%
  get_synapse_entity_txt(.)

wxs_id <- dft_samp %>%
  filter(seq_assay_id %in% 'PROV-TRISEQ-V2') %>%
  pull(sample_id)

dft_mut %>% 
  filter(tumor_sample_barcode %in% wxs_id)

# Reported as missing by collaborator:
# data_gene_panel_PROV-TRISEQ-V2.txt
# data_gene_panel_VHIO-607.txt
# data_gene_panel_VHIO-COLORECTAL-V01.txt

# I don't see any in the data that are from VHIO-607.
# The colorectal one is in synapse (https://www.synapse.org/#!Synapse:syn19957019)
