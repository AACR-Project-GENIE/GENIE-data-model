# Description: Experiment with how to silence synapse download output from tables.  
# Author: Haley Hunter-Zinck
# Date: 

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(synapser)
synLogin()

# synapse
synid_table_test <- "syn25944660"

# main ----------------------------

query <- glue("SELECT * FROM {synid_table_test}")
invisible(capture.output(res <- synTableQuery(query)))

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
