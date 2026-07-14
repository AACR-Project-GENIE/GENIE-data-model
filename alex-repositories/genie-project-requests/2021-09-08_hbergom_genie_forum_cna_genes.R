# Description: look at CNA genes and sample IDs for cBioPortal and v10.
# Author: Haley Hunter-Zinck
# Date: September 8, 2021
# Request:
#   Source: GENIE discussion forum
#   URL: https://www.synapse.org/#!Synapse:syn7222066/discussion/threadId=8047
#   Date of request: 06/23/2021
#   Requestor: https://www.synapse.org/#!Profile:3428670 (hbergom)

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(synapser)
synLogin()

# files
file_cna <- "~/data/genie_release_10.0/data_CNA.txt"
file_cbio_cna <- "~/data/cbioportal/CNA_Genes.txt"
file_cbio_cli <- "~/data/cbioportal/genie_public_clinical_data.tsv"
file_sample <- "~/data/genie_release_10.0/data_clinical_sample.txt"
file_tmp <- "/tmp/genes.txt"


# functions ----------------------------

# read ----------------------------

# genes from CNA file
system(glue("cut -f 1 {file_cna} > {file_tmp}"))
cna_genes <- scan(file_tmp, skip = 1, what = "character")

# sample ids  from CNA file
raw <- scan(file_cna, nlines = 1, what = "character")
cna_sample_ids <- unique(raw[2:length(raw)])

# sample ids from sample file
data <- read.csv(file_sample, header = T, comment.char = "#", sep = "\t")
cli_sample_ids <- unique(data[,"SAMPLE_ID"])

# CNA genes from cbioportal
cbio_data <- read.csv(file_cbio_cna, header = T, sep = "\t")
cbio_genes <- unique(cbio_data[,"Gene"])

# read sample IDs from cbioportal
cbio_cli <- read.csv(file_cbio_cli,  header = T, sep = "\t")
cbio_sample_ids <- unique(cbio_cli[,"Sample.ID"])

# main ----------------------------

print_summary <- function() {
  print(glue("Number of unique genes in the CNA clinical file: {length(unique(cna_genes))}"))
  print(glue("Number of unique genes in the CNA cBioPortal file: {length(unique(cbio_genes))}"))
  
  print(glue("Number of rows in the CNA clinical file: {length(cna_genes) + 1}"))
  print(glue("Number of rows in the CNA cBioPortal CNA gene file: {nrow(cbio_data)}"))
  
  print(glue("Percentage of sample IDs in the CNA clinical file that are in the clinical sample file: {round(length(intersect(cna_sample_ids, cli_sample_ids)) / length(cna_sample_ids) *100, 2)}%"))
  print(glue("Percentage of cBioPortal sample IDs that are in the clinical sample file: {round(length(intersect(cbio_sample_ids, cli_sample_ids)) / length(cbio_sample_ids) *100, 2)}%"))
}

print_summary()

# close out ----------------------------

file.remove(file_tmp)

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
