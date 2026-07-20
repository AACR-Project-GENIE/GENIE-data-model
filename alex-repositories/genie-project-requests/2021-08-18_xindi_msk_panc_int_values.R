# Description: determine if the flagged age_at_seq column
#   contains string values when SOR says it should contain only integers.
# Author: Haley Hunter-Zinck
# Date: August 17, 2021

# setup ----------------------

library(synapser)
synLogin()

synid_file <- "syn25541828"
col_name <- "age_at_seq_report"

# main --------------------

data <- read.csv(synGet(synid_file)$path, na.strings = "", stringsAsFactors = F)

unique(data[,col_name])
