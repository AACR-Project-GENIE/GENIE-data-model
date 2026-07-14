# Description: Add annotations to GENIE BPC uploads.
# Author: Haley Hunter-Zinck
# Date: 2022-05-20

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_folder_upload <- "syn20852283"

# parameters
cohorts <- c("OVARIAN", "ESOPHAGO", "RENAL", "MELANOMA", "CRC2", "NSCLC2")
sites <- c("DUKE", "PROV", "UCSF")
file_placeholder <- "placeholder.txt"

# main ----------------------------

# create placeholder files
write(c("this is a placeholder file"), ncolumns = 1, file = file_placeholder)

for (cohort in cohorts) {
  synid_folder_cohort <- synStore(Folder(name = cohort, parent = synid_folder_upload))
  
  for (site in sites) {
    synid_folder_site <- synStore(Folder(name = site, parent = synid_folder_cohort))
    
    if (site == "DFCI") {
      synid_file_header <- synStore(File(path = file_placeholder, 
                                         parent = synid_folder_site, 
                                         name = glue("{site} {cohort} Header"),
                                         annotations = list(cohort = cohort, site = site, file_type = "header", phase = "2")))
    } 
    synid_file_data <- synStore(File(path = file_placeholder, 
                                     parent = synid_folder_site, 
                                     name = glue("{site} {cohort} Data"),
                                     annotations = list(cohort = cohort, site = site, file_type = "data", phase = "2")))
  }
}

# clean up
file.remove(file_placeholder)

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
