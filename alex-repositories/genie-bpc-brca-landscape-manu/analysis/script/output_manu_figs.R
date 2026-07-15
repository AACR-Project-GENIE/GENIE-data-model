# Output the figures in an order/label/size that will be appropriate for
#   a manuscript.
# This does some light processing that's originally redunant with 
#   bpc-breast-surv-dmet_2.Rmd.

library(fs); library(purrr); library(here);
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

source_folder_top <- here('data', 'survival', 'v2')
output_folder <- here('output', 'fig', 'manu')

read_wrap_surv_sum <- function(file) {
  readr::read_rds(
    here(source_folder_top, 'fit_outputs', 'fit_summary', paste0(file, '.rds')
    )
  ) 
}


dft_coef_dmet_os_all <- read_wrap_surv_sum(
  "coef_dmet_os_all"
)
dft_coef_dmet_os_trip_neg <- read_wrap_surv_sum(
  "coef_dmet_os_trip_neg"
)
dft_coef_dmet_os_hr_pos_her2_neg <- read_wrap_surv_sum(
  "coef_dmet_os_hr_pos_her2_neg"
)
dft_coef_dmet_os_her2_pos <- read_wrap_surv_sum(
  "coef_dmet_os_her2_pos"
)

dft_model_compare_dmet_os_all <- read_wrap_surv_sum(
  "model_compare_os_all"
)
dft_model_compare_dmet_os_trip_neg <- read_wrap_surv_sum(
  "model_compare_os_trip_neg"
)
dft_model_compare_dmet_os_hr_pos_her2_neg <- read_wrap_surv_sum(
  "model_compare_os_hr_pos_her2_neg"
)
dft_model_compare_dmet_os_her2_pos <- read_wrap_surv_sum(
  "model_compare_os_her2_pos"
)

dft_dmet_surv_all <- readr::read_rds(
  file = here(source_folder_top, 'prepared_data', 'surv_dmet_all.rds')
)
dft_dmet_surv_trip_neg <- readr::read_rds(
  file = here(source_folder_top, 'prepared_data', 'surv_dmet_trip_neg.rds')
)
dft_dmet_surv_hr_pos_her2_neg <- readr::read_rds(
  file = here(source_folder_top, 'prepared_data', 'surv_dmet_hr_pos_her2_neg.rds')
)
dft_dmet_surv_her2_pos <- readr::read_rds(
  file = here(source_folder_top, 'prepared_data', 'surv_dmet_her2_pos.rds')
)





# Add the hazard ratio to all of these.  Easiest way to handle the plot scaling below.
dft_coef_dmet_os_all %<>% mutate(hr = exp(log_hr))
dft_coef_dmet_os_her2_pos %<>% mutate(hr = exp(log_hr))
dft_coef_dmet_os_hr_pos_her2_neg %<>% mutate(hr = exp(log_hr))
dft_coef_dmet_os_trip_neg %<>% mutate(hr = exp(log_hr))


remove_institution_terms <- function(dat, col = 'term', drop_removed = T) {
  dat %<>%
    filter(!str_detect(.data[[col]], "^institution"))
  
  if (drop_removed) {
    dat %<>% mutate({{col}} := fct_drop(.data[[col]]))
  }
  return(dat)
}

term_fixer_manu <- function(dat, col = 'term') {
  dat %>%
    mutate(
      {{col}} := suppressWarnings(
        forcats::fct_recode(
          .data[[col]],
          `HER2+` = "her2_pos",
          `unknown HR/HER2` = "receptor_unk",
          `Age dx` = "age_dx_c",
          `de-novo met.` = "de_novo_met",
          `race oth/unk` = "race_unk_oth"
        )
      )
    )
}

all_manu_cleanup <- function(dat) {
  dat %>%
    remove_institution_terms %>%
    term_fixer_manu
}



# Cleanup for output: 
dft_coef_dmet_os_all %<>% all_manu_cleanup(.)
dft_coef_dmet_os_her2_pos %<>% all_manu_cleanup(.)
dft_coef_dmet_os_hr_pos_her2_neg %<>% all_manu_cleanup(.)
dft_coef_dmet_os_trip_neg %<>% all_manu_cleanup(.)

dft_model_compare_dmet_os_all %<>% all_manu_cleanup(.)
dft_model_compare_dmet_os_trip_neg %<>% all_manu_cleanup(.)
dft_model_compare_dmet_os_her2_pos %<>% all_manu_cleanup(.)
dft_model_compare_dmet_os_hr_pos_her2_neg %<>% all_manu_cleanup(.)




# vec_st <- "Stability is the frequency of selection in a boostrapped regularized LTRC Cox model"
gg_rel_scatter_os_all <- plot_hr_rel_scatter_gene_clin_rescale(
  dft_coef_dmet_os_all,
  plot_title = "OS from dmet (all patients)",
  leg_pos = "none",
  title_pos = "panel"
)

gg_rel_scatter_os_trip_neg <- plot_hr_rel_scatter_gene_clin_rescale(
  dft_coef_dmet_os_trip_neg,
  plot_title = "OS from dmet (TNBC patients)",
  leg_pos = "none",
  title_pos = "panel"
)

gg_rel_scatter_os_hr_pos_her2_neg <- plot_hr_rel_scatter_gene_clin_rescale(
  dft_coef_dmet_os_hr_pos_her2_neg,
  plot_title = "OS from dmet (HR+/HER2- patients)",
  leg_pos = "none",
  title_pos = "panel"
)

gg_rel_scatter_os_her2_pos <- plot_hr_rel_scatter_gene_clin_rescale(
  dft_coef_dmet_os_her2_pos,
  plot_title = "OS from dmet (HER2+ patients)",
  leg_pos = "none",
  title_pos = "panel"
)

gg_fig1_comb <- plot_grid(
  gg_rel_scatter_os_all,
  (gg_rel_scatter_os_hr_pos_her2_neg + 
     theme(axis.title.y = element_blank())),
  rel_widths = c(0.55, 0.45),
  labels = "AUTO"
)


ggsave(
  filename = here(output_folder, 'fig1_combined.pdf'),
  gg_fig1_comb,
  height = 4, width = 10 
)
fig1_caption <- "(A) Mean model coefficients and selection frequency from our penalized, resampled Cox model of all patients, including indicator terms for HR/HER2 subtype (ref is HR+/HER2-).  Genetic (blue) and clinical (red) features are shown, except for institution indicator variables. Sensitivity analyses for models without clinical adjustments and KM curves for select features are available in the supplement. (B) Analogous plot limited to the HR+/HER2- subgroup."
data.table::fwrite(
  x = list(fig1_caption),
  file = here(output_folder, "fig1_caption.txt")
)
# Had a request from team to save individual figures as well.
ggsave(
  filename = here(output_folder, 'fig1_A.pdf'),
  gg_rel_scatter_os_all,
  height = 4, width = 10*0.55
)
ggsave(
  filename = here(output_folder, 'fig1_B.pdf'),
  gg_rel_scatter_os_all,
  height = 4, width = 10*0.55
)



