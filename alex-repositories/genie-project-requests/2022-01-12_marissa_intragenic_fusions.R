# Description: Look at MSK submitted stuctural variant files to get more information on intragenic "fusions".
# Author: Haley Hunter-Zinck
# Date: 2022-01-12

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(synapser)
synLogin()

# synapse
synid_file_fusion <- "syn7224465"

# main ----------------------------

data <- read.csv(synGet(synid_file_fusion)$path, sep = "\t")
data %>% filter(Fusion == 'NF1-intragenic') %>% select(Comments)


# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
