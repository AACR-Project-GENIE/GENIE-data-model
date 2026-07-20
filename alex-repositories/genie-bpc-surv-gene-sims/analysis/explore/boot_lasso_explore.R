# Description: Playspace for survival lasso models.

library(magrittr)
library(dplyr)
library(janitor)
library(yaml)
library(here)
library(purrr)
library(fs)
library(ggplot2)
library(tidyr)
library(stringr)
library(survival)
library(glmnet)

purrr::walk(.x = fs::dir_ls('R'), .f = source)

dft_dmet_surv <- readr::read_rds(
  file = here('data', 'survival', 'surv_dmet.rds')
)

y_dmet_os <- with(
  dft_dmet_surv,
  Surv(
    time = tt_cpt_dmet_yrs_pos,
    time2 = tt_os_dmet_yrs,
    event = os_dx_status
  )
)

x_dmet_os <- dft_dmet_surv %>%
  # just to get the output in readable order we do an alphabet sort first:
  select(order(colnames(dft_dmet_surv))) %>%
  select(
    matches("_mut$"),
    matches("_cna$"),
    matches("_fus$"),
    age_dx_c,
    stage_dx_iv_num,
    birth_year_c,
    white,
    hispanic
  ) %>%
  as.matrix(.)

# Example of running one time on the whole dataset:
cvfit_dmet_os <- cv.glmnet(
  x = x_dmet_os,
  y = y_dmet_os,
  standardize = T,
  alpha = 0.97, # small bit of ridge for better convergence.
  family = "cox",
  nfolds = 5
)

# Extraction of key components:
dft_coef_dmet_os <- coef(cvfit_dmet_os, s = "lambda.min") %>% 
  tidy_cv_glmnet(., exp_coef = T, remove_zero = T)
plot(cvfit_dmet_os)



n_boots <- 3
set.seed(389) # for drawing each boot seed below
dft_boot_dmet_os <- tibble(boot_ind = 1:n_boots) %>%
  mutate(boot_seed = sample.int(n = 10^7, size = n()))

dft_boot_dmet_os <- dft_boot_dmet_os %>%
  mutate(
    fit = purrr::map(
      .x = boot_seed,
      .f = (function(x) {
        fit_boot_cv_ltrc_lasso(
          x_mat = x_dmet_os,
          y_df = dft_dmet_surv,
          y_t = "tt_cpt_dmet_yrs_pos",
          y_t2 = "tt_os_dmet_yrs",
          y_e = "os_dx_status",
          boot_seed = x,
        )
      })
    )
  )

# Smart to save the outputs at this stage:
readr::write_rds(
  x = dft_boot_dmet_os,
  file = here('analysis', 'explore', 'lasso_fits_dmet_os.rds')
)

dft_coef_dmet_os <- dft_boot_dmet_os %>%
  mutate(
    coef_mat = purrr::map(.x = fit, .f = get_cv_lasso_coefs)
  ) %>%
  select(-fit) %>%
  unnest(coef_mat)

dft_coef_dmet_os %<>%
  mutate(feature = forcats::fct_inorder(feature)) %>%
  group_by(feature) %>%
  summarize(
    reliability = mean(abs(log_hr) > 0.01),
    log_hr = mean(log_hr)
  ) %>%
  mutate(
    hr = exp(log_hr)
  )


gg_coef_flat_dmet_os <- plot_coef_grid_flat(
  dft_coef_dmet_os,
  plot_title = "Model features"
)

gg_coef_flat_dmet_os

gg_coef_reliability_dmet_os <- plot_coef_grid_reliability(
  dft_coef_dmet_os,
)
gg_coef_reliability_dmet_os

gg_hr_forest_dmet_os <- dft_coef_dmet_os %>%
  filter(reliability >= 0.5) %>% # arbitrary
  mutate(feature = forcats::fct_drop(feature)) %>% 
  plot_hr_forest(.,
                 pt_color_col = "reliability")
gg_hr_forest_dmet_os


gg_os_dmet_coef <- plot_coef_grid(dat = dft_coef_dmet_os_plot, 
                                  plot_title = "Overall survival from distant metastasis")

gg_os_dmet_coef


# ggsave(
#   filename = here('output', 'fig', 'lasso_coef_os_dmet.pdf'),
#   plot = gg_os_dmet_coef,
#   width = 5, height = 5
# )


gg_os_dmet_hr <- ggplot(mutate(dft_coef_dmet_os, feature = factor(feature)),
                        aes(x = hr, y = feature)) + 
  geom_point() +
  geom_vline(xintercept = 1, color = "#bb5566") + 
  theme_bw() + 
  scale_x_continuous(n.breaks = 8) + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title.position = 'plot'
  ) + 
  labs(x = "Hazard ratio",
       title = "Overall Survival from distant metastasis",
       subtitle = "Features not shown are all zero.",
       y = NULL)

gg_os_dmet_hr

# ggsave(
#   filename = here('output', 'fig', 'lasso_hr_os_dmet.pdf'),
#   plot = gg_os_dmet_hr,
#   width = 6, height = 3
# )