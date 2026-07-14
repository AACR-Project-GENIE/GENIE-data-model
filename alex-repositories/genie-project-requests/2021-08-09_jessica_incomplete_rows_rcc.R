# Description: determine source of discrepancy between UHN REDCap and RCC for incomplete or blank records 
#   for 3 record IDs in the PANC cohort
# AUthor: Haley Hunter-Zinck
# Date: August 9, 2021
# Email: 
#   From: Jessica Lavery
#   Date sent: August 5, 2021


# setup ----------------------

library(synapser)
synLogin()

# files
synid_file_uhn_upload <- "syn25472587"
synid_table_cancer_panel <- "syn21446709"
synid_table_imaging <- "syn21446706"

# column_names 
COL_CANCER_PANEL_COMPLETE <- "cancer_panel_test_complete"
COL_IMAGING_COMPLETE <- "prissmm_imaging_complete" 
COL_RECORD_ID <- "record_id"
COL_REPEAT_INSTANCE <- "redcap_repeat_instance"
COL_REPEAT_INSTRUMENT <- "redcap_repeat_instrument"
COL_IMAGING_RESOLVE <- "image_qaresolve"

# values
INSTRUMENT_CANCER_PANEL <- "cancer_panel_test"
INSTRUMENT_IMAGING <- "prissmm_imaging"
map_instrument <- setNames(c(COL_CANCER_PANEL_COMPLETE, COL_IMAGING_COMPLETE),
                           c(INSTRUMENT_CANCER_PANEL, INSTRUMENT_IMAGING))

# data_upload keys
repeats <- c(3, 5, 7, 9, 15, 16, 18)
checks <- cbind(c("GENIE-UHN-OCT251821","GENIE-UHN-OCT251821", rep("GENIE-UHN-OCT420107", length(repeats))),
                c(INSTRUMENT_CANCER_PANEL, INSTRUMENT_IMAGING, rep(INSTRUMENT_IMAGING, length(repeats))),
                c(1, 2, repeats))
colnames(checks) <- c(COL_RECORD_ID, COL_REPEAT_INSTRUMENT, COL_REPEAT_INSTANCE)


# read data_upload ----------------

data_upload <- read.csv(synGet(synid_file_uhn_upload)$path, stringsAsFactors = F, na.strings = "")
column_names <- colnames(data_upload)

# compare ------------------

results <- c()

for (i in 1:nrow(checks)) {
  idx <- which(data_upload[,COL_RECORD_ID] == checks[i,COL_RECORD_ID] & 
                 data_upload[,COL_REPEAT_INSTRUMENT] == checks[i, COL_REPEAT_INSTRUMENT] &
                 data_upload[, COL_REPEAT_INSTANCE] == checks[i, COL_REPEAT_INSTANCE])
  
  
  if (i >= 3) {
    status <- data_upload[idx, COL_IMAGING_RESOLVE]
  } else {
    status <- data_upload[idx, map_instrument[checks[i, COL_REPEAT_INSTRUMENT]]]
  }
  
  
  
  results <- rbind(results, c(checks[i,], status))
}
colnames(results) <- c(colnames(checks), "status")

print(results)

