# Generating PFS/OS plots of some relevant regimens in several subgroups.

library(fs); library(purrr); library(here);
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)


dft_pt <- readr::read_rds(
  here('data', 'clin_data_cohort', 'dft_pt.rds')
)
dft_ca_ind <- readr::read_rds(
  here('data', 'clin_data_cohort', 'dft_ca_ind.rds')
)
dft_clin_char_dmet <- readr::read_rds(
  here('data', 'survival', 'v2', 'prepared_data', 'clin_char_dmet.rds')
)
dft_reg <- readr::read_rds(
  here('data', 'clin_data_cohort', 'dft_reg.rds')
)

dir_obj_out <- here('data', 'survival', 'drug', 'simple_km_met')
dir_img_out <- here('output', 'fig', 'manu')




dft_clin_char_dmet %<>%
  filter(!is.na(dx_to_dmets_yrs)) %>%
  # removes some of the survival stuff to avoid confusion
  select(
    -c(os_dx_status, tt_os_dx_yrs,
       pfs_i_and_m_adv_status,
       tt_pfs_i_and_m_adv_yrs
    )
  )

dft_ca_ind %<>% 
  # Just some T/F versions for easier code:
  mutate(
    er_pos = ca_bca_er %in% "Positive/elevated",
    pr_pos = ca_bca_pr %in% "Positive/elevated",
    hr_pos = er_pos | pr_pos,
    her2_pos = ca_bca_her_summ %in% "Positive/elevated/amplified"
  )


dft_clin_char_dmet <- dft_ca_ind %>%
  arrange(desc(her2_pos), desc(er_pos)) |>
  mutate(
    reg_surv_group = case_when(
      her2_pos ~ "HER2+",
      hr_pos ~ "HR+, HER2-/?",
      bca_subtype %in% "Triple Negative" ~ "TNBC",
      T ~ NA_character_ # very heterogeneous group left out here.
    )
  ) %>%
  mutate(reg_surv_group = fct_inorder(reg_surv_group)) %>%
  select(record_id,
         ca_seq,
         reg_surv_group) %>%
  left_join(dft_clin_char_dmet, ., by = c('record_id', 'ca_seq'))


dft_reg_met <- left_join(
  dft_clin_char_dmet,
  dft_reg,
  by = c('record_id', 'ca_seq')
)

dft_reg_met %<>%
  mutate(
    reg_start_fcpt_rep_yrs = dx_fcpt_rep_yrs - dx_reg_start_int_yrs
  ) %>%
  relocate(
    reg_start_fcpt_rep_yrs, .before = dx_reg_start_int
  )



dft_reg_met %<>%
  # only regmiens started on or after dmet
  mutate(dmet_reg_start_yrs = dx_reg_start_int_yrs - dx_to_dmets_yrs) %>%
  filter(dmet_reg_start_yrs > -0.5/365) 

vec_drug_exclusions <- c("Leuprolide Acetate", "Goserlin Acetate")

dft_reg_met %<>%
  filter(
    !str_detect(regimen_drugs, "Investigational"),
    !(regimen_drugs %in% vec_drug_exclusions)
  )


dft_bca_nest <- dft_reg_met %>%
  arrange(reg_surv_group, dmet_reg_start_yrs) %>%
  nest(.by = reg_surv_group) 

dft_bca_nest %<>%
  mutate(
    dat_1L = purrr::map(
      .x = data,
      .f = \(z) {
        z %>%
          group_by(record_id) %>%
          arrange(dmet_reg_start_yrs) %>%
          slice(1) %>%
          ungroup()
      }
    )
  )

dft_bca_nest %<>% 
  filter(!is.na(reg_surv_group))





# Bit of a messy function here - processing is different for each one.
dft_bca_nest %<>%
  mutate(
    dat_1L_select = purrr::map2(
      .x = dat_1L,
      .y = reg_surv_group,
      .f = \(z, subgroup) {
        if (subgroup %in% "HER2+") {
          z %>%
            filter(regimen_drugs %in% c(
              "Docetaxel, Pertuzumab, Trastuzumab",
              "Paclitaxel, Pertuzumab, Trastuzumab"
            )) %>%
              mutate(reg_group = factor("Taxane, pertuzumab, trastuzumab"))
        } else if (str_detect(subgroup, "^HR+")) {
          z %>%
            filter(regimen_drugs %in% c(
              "Letrozole, Palbociclib",
              "Fulvestrant, Palbociclib"
            )) %>%
              mutate(reg_group = factor(str_to_sentence(regimen_drugs))) # keep separate
        } else {
          z # do nothing, will handle this separately (complex)
        }
      }
    )
  )

# For TNBC we have to do some manual shaping 
#   because we have overlapping drug classes.
dft_tnbc <- dft_bca_nest %>%
  filter(reg_surv_group %in% "TNBC") %>%
  pull(dat_1L_select) %>%
  `[[`(.,1)

dft_tnbc_1 <- dft_tnbc %>% 
  filter(str_detect(regimen_drugs, "Capecitabine")) %>%
  mutate(reg_group = "Capecitabine-containing")

dft_tnbc_2 <- dft_tnbc %>%
  # I'm not including oxaliplatin for now.  Something to check in on maybe.
  filter(str_detect(regimen_drugs, "Cisplatin|Carboplatin")) %>%
  mutate(reg_group = "Platinum-containing")

dft_tnbc_3 <- dft_tnbc %>%
  # I'm not including oxaliplatin for now.  Something to check in on maybe.
  filter(str_detect(
    tolower(regimen_drugs), 
    # I don't think they use cabazitaxel in breast but we'll see:
    "paclitaxel|docetaxel|cabazitaxel")
  ) %>%
  mutate(reg_group = "Taxane-containing")

dft_tnbc_replace <- bind_rows(
  dft_tnbc_1, dft_tnbc_2, dft_tnbc_3
) %>%
  mutate(reg_group = fct_inorder(reg_group))

dft_bca_nest[[3, "dat_1L_select"]] <- list(dft_tnbc_replace)


# Now our 5 relevant variables are:
# - reg_start_fcpt_rep_yrs (entry)
# - tt_os_g_yrs
# - os_g_status
# - tt_pfs_i_and_m_g_yrs
# - pfs_i_and_m_g_status




os_km_drug_helper <- function(
    z,
    pal,
    ...
) {
  z %<>%
    remove_trunc_gte_event(
      trunc_var = 'reg_start_fcpt_rep_yrs',
      event_var = 'tt_os_g_yrs'
    )
  
  z %<>%
    mutate(
      reg_start_fcpt_rep_yrs = case_when(
        reg_start_fcpt_rep_yrs < 0 ~ 0,
        T ~ reg_start_fcpt_rep_yrs
      ))
  
  surv_z <- with(
    z,
    Surv(
      time = reg_start_fcpt_rep_yrs,
      time2 = tt_os_g_yrs,
      event = os_g_status
    )
  )
  
  gg_z <- plot_one_survfit(
    dat = z,
    surv_form = surv_z ~ reg_group,
    x_breaks = 0:100,
    add_ci = T,
    pal = pal,
    ...
  )
  
  gg_z <- gg_z + 
    coord_cartesian(xlim = c(0,5))
  
  return(gg_z)
  
}


pfs_km_drug_helper <- function(
    z,
    pal,
    ...
) {
  
  surv_z <- with(
    z,
    Surv(
      time = tt_pfs_i_and_m_g_yrs,
      event = pfs_i_and_m_g_status
    )
  )
  
  gg_z <- plot_one_survfit(
    dat = z,
    surv_form = surv_z ~ reg_group,
    x_breaks = 0:100,
    add_ci = T,
    pal = pal,
    ...
  )
  
  gg_z <- gg_z + 
    coord_cartesian(xlim = c(0,5))
  
  return(gg_z)
  
}



dft_bca_nest %<>% 
  mutate(
    os_plot_title = c(
      "HER2+: OS from 1L THC",
      "HR+, HER2-: OS from 1L CDKi",
      "TNBC: OS from 1L chemo"
    ),
    pfs_plot_title = c(
      "HER2+: PFS from 1L THC",
      "HR+, HER2-: PFS from 1L CDKi",
      "TNBC: PFS from 1L chemo"
    ),
    os_pal = list(
      c("#009988"),
      c("#33bbee", "#cc3311"),
      c('#bb5566', '#004488', '#ddaa33')
    ),
    gg_os = purrr::pmap(
      .l = list(
        z = dat_1L_select,
        pal = os_pal,
        plot_title = os_plot_title
      ),
      .f = os_km_drug_helper
    ),
    gg_pfs = purrr::pmap(
      .l = list(
        z = dat_1L_select,
        pal = os_pal, # reusing this for now - we have the option.
        plot_title = pfs_plot_title
      ),
      .f = pfs_km_drug_helper
    )
  )

readr::write_rds(
  dft_bca_nest,
  here(dir_obj_out, 'bca_nest_everything.rds')
)

save_name_help <- function(str) {
  str %>%
    tolower %>%
    str_replace_all(., "[\\?\\/\\,]|[:blank:]", "_") %>%
    str_replace_all(., "\\+", "_pos") %>%
    str_replace_all(., "\\-", "_neg") %>%
    str_trim %>%
    paste(., ".pdf", sep = "")
}

vec_save_names <- c(
  paste0('km_os_', save_name_help(dft_bca_nest$reg_surv_group)),
  paste0('km_pfs_', save_name_help(dft_bca_nest$reg_surv_group))
)

purrr::walk2(
  .x = c(
    dft_bca_nest[['gg_os']],
    dft_bca_nest[['gg_pfs']]
  ),
  .y = vec_save_names,
  .f = \(x,y) {
    ggsave(
      plot = x,
      filename = here(dir_img_out, y),
      height = 4, width = 4,
      device = 'pdf',
      scale = 1.5,
      # This gets passed onto the graphics device.  It prevents a bug where all
      #   my plots have a blank first page, but you may not need it on your
      #  system.
      onefile = F
    )
  }
)

pdf_combine(
  input = here(dir_img_out, vec_save_names[1:6]),
  output = here(dir_img_out, "km_combined.pdf")
)

# 
# staplr::remove_pages(
#   1,
#   input_filepath = here(dir_img_out, 'km_os_her2_pos.pdf'),
#   output_filepath = here(dir_img_out, 'km_os_her2_pos.pdf'),
#   overwrite = T
# )



    
    


