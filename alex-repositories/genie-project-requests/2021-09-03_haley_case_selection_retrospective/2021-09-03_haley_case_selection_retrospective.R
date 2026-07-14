# Description: Retrospectively explore case selection (production, pilot, SDV, IRR)
# Author: Haley Hunter-Zinck
# Date: September 3, 2021
# request: I would like to retroactively explore the case selection files to determine
#     how the categorics of cases interact with each other.
#     Eventually, I would like to consolidate and rewrite code so that we can have a
#     single script for case selection.  

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(yaml)
library(synapser)
synLogin()

# synapse
config <- read_yaml("case_selection.yaml")
n_digit <- 2

# functions ----------------------------

get_data_from_file_on_synapse <- function(synapse_id, version = NA, sep = ",", na.strings = c("NA")) {
  
  if (is.na(version)) {
    data <- read.csv(synGet(synapse_id)$path, 
                     stringsAsFactors = F, 
                     sep = sep, 
                     na.strings = na.strings)
  } else {
    data <- read.csv(synGet(synapse_id, version = version)$path, 
                     stringsAsFactors = F, 
                     sep = sep,
                     na.strings = na.strings)
  }
  
  return(data)
}

# main ----------------------------

labels <- c("cohort", "site", "n_total", "n_sdv", "n_irr", "p_sdv", "p_irr")
stats <- matrix(NA, nrow = 0, ncol = length(labels), dimnames = list(c(), labels))

files <- config$files
for (cohort in names(files)) {
  
  for (site in names(files[[cohort]])) {
    
    file <- files[[cohort]][[site]]
    data <- get_data_from_file_on_synapse(synapse_id = file$id, version = file$version, na.strings = c("NA", ""))
    colnames(data) <- tolower(colnames(data))
    
    site_inf <- gsub(pattern = "-", replacement = "", 
                     x = regmatches(data[1,1], regexpr(text = data[1,1], pattern = "-[A-Z]+-")))
    if (!length(site_inf)) {
      site_inf = NA
    }
    
    n_total <- nrow(data)
    n_sdv <- length(which(!is.na(data[,"sdv"])))
    n_irr <- length(which(!is.na(data[,"sdv"])))
    row <- c(cohort, site_inf, n_total, n_sdv, n_irr, round(n_sdv/n_total*100, n_digit), round(n_irr/n_total*100, n_digit))
    stats <- rbind(stats, row)
  }
}

print(stats)

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
