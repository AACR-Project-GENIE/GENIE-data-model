# Description: ensure that all retracted samples are removed from cBioPortal files.
# Author: Haley Hunter-Zinck
# Date: July 22, 2021

library(synapser)
synLogin()
library(glue)

# parameters
synid_sa_retraction = 'syn25779833' # BPC Internal, BPC Sample Retraction table
synid1_cbio <- 'syn22334130' # NSCLC 1.1: cbio files, data_clinical_sample.txt
synid2_cbio = 'syn25471838'  # NSCLC 2.1: cbio files, data_clinical_sample.txt

# files
synid_cbio <- c(synid1_cbio, synid2_cbio)

# get retracted sample IDs from table
ids <- unlist(as.data.frame(synTableQuery(glue("SELECT SAMPLE_ID FROM {synid_sa_retraction}"), 
                                          includeRowIdAndRowVersion = F)))

# cbio
res_cbio <- list()
for(synid in synid_cbio) {
  data <-  read.table(synGet(synid)$path, comment.char = "#", sep = "\t", stringsAsFactors = F, header = T)
  res_cbio[[synid]] <- data[which(is.element(data[,"SAMPLE_ID"], ids)),]
  
}

# print cbio results
for(synid in names(res_cbio)) {
  i = which(names(res_cbio) == synid)
  print(glue("Number of samples present in the {i}.1 cbio files ({synid}): {nrow(res_cbio[[synid]])} / {length(ids)}\n"))
  print(res_cbio[[synid]][,"PATIENT_ID"])
}


