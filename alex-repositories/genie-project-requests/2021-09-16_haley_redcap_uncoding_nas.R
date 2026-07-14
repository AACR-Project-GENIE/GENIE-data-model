# Description: output data row in which new new NA values appeared in merge and uncoded RCA files.
# Author: Haley Hunter-Zinck
# Date: 2021-09-16
# Request: 
#    Source: Haley Hunter-Zinck
#   Date of request: 2021-09-16
#   Message: I asked Alyssa to manually check these values in the raw upload files
#           to ensure the new NA values were not a result of a coding error in the 
#           file merging and mapping.  

# first run validation script from Genie_processing/bpc/uploads/
source("validation.R")

n_naplus <- length(which(!comp_naplus_result))
to_write <- c()
for(i in 1:n_naplus) {
  
  var_name <- colnames(ref)[which(!comp_naplus_result)][i]
  var_name_root <- get_root_variable_name(var_name)

  field_type <- if (is.element(var_name_root, dd$`Variable / Field Name`)) dd$`Field Type`[which(dd$`Variable / Field Name`  == var_name_root)] else 'unknown'
  
  idx_matched <- match(key_ref, key_upl)
  idx_not_na <- which(!is.na(idx_matched))
  col_remove <- setdiff(colnames(ref), colnames(upl))
  col_rcc <- ref[idx_not_na,which(colnames(ref) == var_name)]
  col_rcc_from_uncoded <- upl[idx_matched[idx_not_na],colnames(ref)[which(colnames(ref) == var_name)]]
  tmp <- cbind(col_rcc, col_rcc_from_uncoded)
  
  na_counts <- apply(tmp, 2, function(x) {return(length(which(is.na(x))))})
  print(glue("NA counts in reference and upload: {na_counts[1]} vs. {na_counts[2]} (difference: {na_counts[2] - na_counts[1]})"))
  idx <- which(!is.na(tmp[,1]) & is.na(tmp[,2]))
  value <- upl[idx,var_name]
  to_write <- rbind(to_write, cbind(upl[idx, c("record_id", "redcap_repeat_instrument", "redcap_repeat_instance")], var_name,value))
}
  
write.csv(to_write, file = "2021-09-16_haley_redcap_uncoding_nas.csv", row.names = F)




save_to_synapse <- function(path, parent_id, file_name = NA, prov_name = NA, prov_desc = NA, prov_used = NA, prov_exec = NA) {
  
  if (is.na(file_name)) {
    file_name = path
  } 
  file <- File(path = path, parentId = parent_id, name = file_name)
  
  if (!is.na(prov_name) || !is.na(prov_desc) || !is.na(prov_used) || !is.na(prov_exec)) {
    act <- Activity(name = prov_name,
                    description = prov_desc,
                    used = prov_used,
                    executed = prov_exec)
    file <- synStore(file, activity = act)
  } else {
    file <- synStore(file)
  }
  
  
  return(T)
}

save_to_synapse(path = "2021-09-16_haley_redcap_uncoding_nas.csv", parent_id = "syn26145645")
file.remove("2021-09-16_haley_redcap_uncoding_nas.csv")

