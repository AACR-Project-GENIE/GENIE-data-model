# Description: check oncotree codes for BrCa release with codes used to create eligible cohort
# Author: Haley Hunter-Zinck
# Date: August 18, 2021
# email: 
#    subject: BrCa Oncotree codes
#   from : Brown, Samantha/Epidemiology-Biostatistics (browns7@mskcc.org)
#    date sent: August 18, 2021

# setup ----------------

file_sammi <- "~/requests/2021-08-18_sammi_oncotree_codes/2021-08-18_sammi_oncotree_codes_brca.txt"
file_eligible <- "~/requests/2021-08-18_sammi_oncotree_codes/2021-08-18_eligible_oncotree_codes_brca.txt"

# function -------------------

get_list_element <- function(my_list, index) {
  
  vec <- c()
  
  for (i in 1:length(my_list)) {
    vec[i] <- my_list[[i]][index]
  }
  
  return(vec)
}

# codes -------------------

# sammi
raw_sammi <- scan(file_sammi, sep = "\n", what = "character")
open_paren <- get_list_element(strsplit(x = raw_sammi, split = "(", fixed = T), 2)
code_sammi <- get_list_element(strsplit(open_paren, split = ")", fixed = T), 1)

# eligible
raw_elig <- read.table(file_eligible, sep = "\t", header = T)
code_elig <- raw_elig[,2]

setdiff(code_sammi, code_elig)

setdiff(code_elig, code_sammi)
