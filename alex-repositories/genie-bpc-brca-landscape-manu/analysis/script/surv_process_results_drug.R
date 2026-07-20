# Description: Process the raw model fits into results.
# Author: Alex Paynter

library(fs); library(purrr); library(here);
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

raw_folder <- here("data", "survival", 'drug', 'fit_outputs')
# now that I'm using nested models this is totally tractable:
output_folder <- here('data', 'survival', 'drug', 'fit_outputs')

dft_drug_surv_nest <- readr::read_rds(
  here(raw_folder, "fit_dmet_drug_nest.rds")
)

resample_dmet_wrap <- function(dat) {
  resample_coef_helper(
    dat = dat,
    exp_coefs = F,
    estimate_col_rename = "log_hr" 
  )
}

# First just a bit of name cleanup:
dft_drug_surv_nest %<>%
  mutate(
    class_comp_disp = case_when(
      class_comp %in% "CDK inhibitor" ~ "CDK inhibitors",
      class_comp %in% "IC inhibitor" ~ "IC inhibitors",
      class_comp %in% "chemo" ~ "Chemotherapies",
      class_comp %in% "endocrine" ~ "Endocrine therapies",
      class_comp %in% "pi3k pathway inhibitor" ~ "PI3K inhibitors"
    )
  )
      


dft_drug_surv_nest %<>%
  mutate(
    coef_os = purrr::map(
      .x = boots_os,
      .f = resample_dmet_wrap
    ),
    coef_os_no_conf = purrr::map(
      .x = boots_os_no_conf,
      .f = resample_dmet_wrap
    ),
    
    coef_pfs = purrr::map(
      .x = boots_pfs,
      .f = resample_dmet_wrap
    ),
    coef_pfs_no_conf = purrr::map(
      .x = boots_pfs_no_conf,
      .f = resample_dmet_wrap
    )
  )



# dft_drug_surv_nest %>% slice(1) %>% pull(coef_os) %>% `[[`(.,1) %>%
#   arrange(desc(stability))

# Glorious nested data makes me feel OK imbedding the plots right here:

dft_drug_surv_nest %<>%
  mutate(
    plot_os_scatter_title = glue(
      "{class_comp_disp}: Predictive variables for OS"
    ),
    plot_os_scatter = purrr::map2(
      .x = coef_os,
      .y = plot_os_scatter_title,
      .f = (function(x,y) {
        plot_hr_rel_scatter_gene_clin(
          dat = x,
          plot_title = y,
          plot_subtitle = "Limitated to metastatic cases with first drug use after sequencing."
        )
      })
    )
  )

dft_drug_surv_nest %<>%
  mutate(
    plot_pfs_scatter_title = glue(
      "{class_comp_disp}: Predictive variables for PFS (I+M)"
    ),
    plot_pfs_scatter = purrr::map2(
      .x = coef_pfs,
      .y = plot_pfs_scatter_title,
      .f = (function(x,y) {
        plot_hr_rel_scatter_gene_clin(
          dat = x,
          plot_title = y,
          plot_subtitle = "Limitated to metastatic cases with first drug use after sequencing.",
          pal = c("#ee7733", "#0077bb") #paul tol vibrant.
        )
      })
    )
  )



# Add big/little comparison plots.
dft_drug_surv_nest %<>%
  mutate(
    model_compare_os = purrr::map2(
      .x = coef_os,
      .y = coef_os_no_conf,
      .f = (function(x,y) {
        model_compare_boot_lasso(
          big_model_sum = x,
          little_model_sum = y,
          coef_col_name = "log_hr"
        ) %>%
          # take only the top 20 terms for stability, just for tractability.s
          filter(term %in% head(levels(term), 20)) 
      })
    )
  )

dft_drug_surv_nest %<>%
  mutate(    
    plot_big_little_os_title = glue(
      "{class_comp_disp}: OS model comparison"
    ),
    plot_mod_comp = purrr::map2(
      .x = model_compare_os,
      .y = plot_big_little_os_title,
      .f = (function(x,y) {
        plot_lasso_model_comp(
          dat = x,
          plot_title = y
        )
      })
    )
  )


# Trim this out just for conciseness:
dft_drug_surv_nest %<>%
  select(-contains("_title"))


# Create the flextable objects that state each list of agents:
dft_drug_surv_nest %<>%
  mutate(
    ft_agent = purrr::map(
      .x = dat_surv,
      .f = get_agent_ft # in /R
    )
  )

# # If you want to look at one plot:
dft_drug_surv_nest %>%
  slice(1) %>%
  pull(plot_mod_comp) %>%
  `[[`(.,1)



# Write the outputs:
write_wrap_surv_sum <- function(obj, name) {
    readr::write_rds(
      x = obj,
      file = here(output_folder, paste0(name, ".rds"))
    )
}

write_wrap_surv_sum(
  obj = dft_drug_surv_nest,
  name = "processed_dmet_drug_nest"
)


