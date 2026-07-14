# Description: Create subfolders for cohorts and sites for phase 2 in 'IDs for export request' folder.
# Author: Haley Hunter-Zinck
# Date: 2022-05-20

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_folder_upload <- "syn20781633"

# parameters
cohorts <- c("OVARIAN", "ESOPHAGO", "RENAL", "MELANOMA", "CRC", "NSCLC")
sites <- c("DFCI", "DUKE", "MSK", "PROV", "UCSF", "UHN", "VICC")

# main ----------------------------

for (cohort in cohorts) {
  synid_folder_cohort <- synStore(Folder(name = cohort, parent = synid_folder_upload))
  
  for (site in sites) {
    synid_folder_site <- synStore(Folder(name = site, parent = synid_folder_cohort))
  }
}

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
