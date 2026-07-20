# Description: quick check per email request from Mike
# Author: Haley Hunter-Zinck
# Date: July 19, 2021
# Text of email:
# In the 6/18/2021 QA report, 2 DFCI patients were flagged as being potentially ineligible 
# with diagnoses of small cell lung carcinoma. In the end we agreed to remove one of the 
# patients for the 2.1 release and sub in another for the 2.0-public release. Looking at 
# the QA report, GENIE-DFCI-002270 and GENIE-DFCI-034595were flagged as SCLC. We then 
# compared the 1.1 and 2.1 releases and it looks like -002270 has been removed. Could you 
# please confirm so Jessica can complete the data guide and we can push the release?

library(synapser)
synLogin()
library(glue)

# parameters
synid1_clinical <- 'syn22418979' # NSCLC 1.1: clinical files, patient_level_dataset.csv
synid2_clinical <- 'syn25985884' # NSCLC 2.1: clinical files, patient_level_dataset.csv
synid1_cbio <- 'syn22334129' # NSCLC 1.1: cbio files, data_clinical_patient.txt
synid2_cbio = 'syn25471837'  # NSCLC 2.1: cbio files, data_clinical_patient.txt
ids = c('GENIE-DFCI-002270', 'GENIE-DFCI-034595')

synid_clinical <- c(synid1_clinical, synid2_clinical)
synid_cbio <- c(synid1_cbio, synid2_cbio)

# clinical
res_clinical <- list()
for(synid in synid_clinical) {
  data <- read.csv(synGet(synid)$path, stringsAsFactors = F)
  res_clinical[[synid]] <- data[which(is.element(data$record_id, ids)),]
}

# cbio
res_cbio <- list()
for(synid in synid_cbio) {
  data <-  read.table(synGet(synid)$path, comment.char = "#", sep = "\t", stringsAsFactors = F, header = T)
  res_cbio[[synid]] <- data[which(is.element(data[,"PATIENT_ID"], ids)),]
  
}

for(synid in names(res_clinical)) {
  i = which(names(res_clinical) == synid)
  print(glue("Number of patients present in the {i}.1 clinical files ({synid}): {nrow(res_clinical[[synid]])} / {length(ids)}\n"))
  print(res_clinical[[synid]]$record_id)
}

for(synid in names(res_cbio)) {
  i = which(names(res_cbio) == synid)
  print(glue("Number of patients present in the {i}.1 cbio files ({synid}): {nrow(res_cbio[[synid]])} / {length(ids)}\n"))
  print(res_cbio[[synid]][,"PATIENT_ID"])
}


