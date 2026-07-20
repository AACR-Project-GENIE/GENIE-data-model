

# Loaded some datasets like dft_path from main.Rmd.

late_gleasons <- dft_path %>%
  group_by(record_id) %>%
  arrange(dx_path_proc_yrs) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(desc(dx_path_proc_days)) %>%
  filter(!is.na(path_pc_gleason)) %>%
  # filter(dx_path_proc_days >= 30) %>%
  select(record_id, path_pc_gleason, dx_path_proc_yrs) %>%
  # arrange(record_id) %>%
  rename(path_gleas_from_path_data = path_pc_gleason)
#%>%
 # pull(record_id) %>%
#  dput

# Three examples of participants with first pathology reports WAY after
#   the 
#  1 GENIE-MSK-P-0000935              4019 Gleason score 7
#  2 GENIE-DFCI-110132                3815 Gleason score 9
#  3 GENIE-DFCI-080399                1448 Gleason score 7

# Now let's see if these gleason scores are populated:
dft_ca_ind %>%
  select(record_id, ca_pros_clin_gscore, ca_pros_path_gscore) %>%
  # these are pasted from dput above
  inner_join(
    .,
    late_gleasons,
    by = "record_id"
  ) %>%
  filter(ca_pros_path_gscore != path_gleas_from_path_data) %>%
  filter(str_detect(ca_pros_path_gscore, "Gleason")) %>%
  filter(str_detect(path_gleas_from_path_data, "Gleason")) %>%
  arrange(dx_path_proc_yrs) %>%
  glimpse
select(re)