# Description: Explore issues with derived variable code.
# Author: Haley Hunter-Zinck
# Date: 2022-01-24

# setup --------------------

library(dplyr)
library(synapser)
synLogin()

# parameters
file <- "derived_data_2022-01-21.rdata"
data_cut <- "2022-01-21"

# functions --------------------

get_phi_cutoff <- function(unit){
  return(switch(unit, 
                "day" = c(89*365, 18*365),
                "month" = c(89*12, 18*12), 
                "year" = c(89,18)))
}

check_data_for_phi <- function(df, interval_field_info){
  interval_list <- intersect(colnames(df), interval_field_info$variable)
  df[,interval_list] <- lapply(interval_list,function(x){
    print(x)
    phi_cutoff <- get_phi_cutoff(interval_field_info$unit[interval_field_info$variable==x])
    dat <- df[[x]]
    dat <- lapply(dat, function(x) {if (is.null(x)) NA else x})
    dat <- ifelse((as.numeric(dat) > phi_cutoff[1] | grepl(">",dat)), #|
                  #as.numeric(dat) < phi_cutoff[2] | grepl("\\<", dat)),
                  NA, dat)
    return(dat)
  })
  return(df)
}

# Store derived variable tables on Synapse
store_synapse <- function(var, filename,dataset) {
  write.csv(apply(var, 2, unlist), filename, row.names = F, quote = T, na = "")
  #synStore(File(path=filename, parent="syn22296812",name=filename,
  #              annotations=list(data_type='derived',dataset=dataset)))
}

# read -------------------
load(file)

# main ---------------------------

### Xindi
### Save datasets with derived variables and queries datasets
### interval fields for redacted patient
redacted_pt_list <- synTableQuery("SELECT record_id FROM syn21446700 WHERE redacted='Yes'", includeRowIdAndRowVersion=FALSE)
redacted_pt_list <- as.data.frame(redacted_pt_list)$record_id
interval_field_info <- synTableQuery("SELECT * FROM syn23281483", includeRowIdAndRowVersion=FALSE)
interval_field_info <- as.data.frame(interval_field_info)

# check intervals and set affected one to NA
ca_dx_derived_redacted <- check_data_for_phi(ca_dx_derived,interval_field_info)
ca_dx_derived_index_redacted <- check_data_for_phi(ca_dx_derived_index,interval_field_info)
ca_dx_derived_non_index_redacted <- check_data_for_phi(ca_dx_derived_non_index,interval_field_info)
pt_derived_redacted <- check_data_for_phi(pt_derived, interval_field_info)
ca_drugs_derived_redacted <- check_data_for_phi(ca_drugs_derived, interval_field_info)
prissmm_image_derived_redacted <- check_data_for_phi(prissmm_image_derived, interval_field_info)
prissmm_path_derived_redacted <- check_data_for_phi(prissmm_path_derived, interval_field_info)
prissmm_md_derived_redacted <- check_data_for_phi(prissmm_md_derived, interval_field_info)
prissmm_tm_derived_redacted <- check_data_for_phi(prissmm_tm_derived, interval_field_info)
cpt_derived_redacted <- check_data_for_phi(cpt_derived, interval_field_info)
ca_radtx_derived_redacted <- check_data_for_phi(ca_radtx_derived, interval_field_info)

# set birth year to NA
redacted_adult_list <- synTableQuery("SELECT record_id FROM syn21446700 WHERE redacted='Yes' and birth_year is null", includeRowIdAndRowVersion=FALSE)
redacted_adult_list <- as.data.frame(redacted_adult_list)$record_id
pt_derived_redacted <- pt_derived_redacted %>%
  mutate(birth_year = ifelse(record_id %in% redacted_adult_list, NA, birth_year))
# save the derived datasets
save(cur_qa, ca_dx_derived_redacted, ca_dx_derived_index_redacted, ca_dx_derived_non_index_redacted, pt_derived_redacted, 
     ca_drugs_derived_redacted, prissmm_image_derived_redacted, prissmm_path_derived_redacted, prissmm_md_derived_redacted, 
     prissmm_tm_derived_redacted, cpt_derived_redacted, ca_radtx_derived_redacted,
     file = paste0("derived_data_redacted_", data_cut, ".rdata")
)
# Store derived variable tables on Synapse
# Write and store files to Synapse
store_synapse(ca_dx_derived_redacted, "ca_dx_derived.csv",'Cancer-level dataset')
store_synapse(pt_derived_redacted, "pt_derived.csv",'Patient-level dataset')
store_synapse(ca_drugs_derived_redacted, "ca_drugs_derived.csv",'Regimen-Cancer level dataset')
store_synapse(prissmm_image_derived_redacted, "prissmm_image_derived.csv",'Imaging-level dataset')
store_synapse(prissmm_path_derived_redacted, "prissmm_path_derived.csv",'Pathology-report level dataset')
store_synapse(prissmm_md_derived_redacted, "prissmm_md_derived.csv",'Med Onc Note level dataset')
store_synapse(prissmm_tm_derived_redacted, "prissmm_tm_derived.csv",'PRISSMM Tumor Marker level dataset')
store_synapse(cpt_derived_redacted, "cpt_derived.csv",'Cancer panel test level dataset')
store_synapse(ca_dx_derived_index_redacted, "ca_dx_derived_index.csv",'Cancer-level index dataset')
store_synapse(ca_dx_derived_non_index_redacted, "ca_dx_derived_non_index.csv",'Cancer-level non-index dataset')
store_synapse(ca_radtx_derived_redacted, "ca_radtx_derived.csv", "Radiation Therapy dataset")
synStore(File(path=paste0("derived_data_redacted_", data_cut, ".rdata"),
              parent='syn22296812',
              name='derived_data.rdata'))

# debug problematic column ---------------------------
c1 <- ca_drugs_derived[["dx_drug_start_int_1"]]
c2 <- ca_drugs_derived[["dx_drug_start_int_2"]]
c3 <- ca_drugs_derived[["dx_drug_start_int_3"]]
identical(c1, c2)
head(c1)
head(c2)
length(c1)
length(c2)
typeof(c1)
typeof(c2)
unique(unlist(lapply(c1, length)))
unique(unlist(lapply(c2, length)))

len_c1 <- unlist(lapply(c1, length))
len_c2 <- unlist(lapply(c2, length))
len_c3 <- unlist(lapply(c3, length))
table(len_c1)
table(len_c2)
table(len_c3)

