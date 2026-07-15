# An expansion on an analysis section.
# May want to split this up for specific biomarkers in the future, but for
#   now they end up being very similar.

library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

dft_hdrug_det <- readr::read_rds(
  here('data', 'linked_approvals', 'hdrug_determinations.rds')
)

dft_gen_mark <- readr::read_rds(
  here('data', 'no_ca_seq_filter', 'genomic_sum.rds')
)

dft_ind_mapped <- readr::read_rds(
  here('data', 'warner_materials', 'indications_mapped.rds')
)

dir_output <- here('data', 'stories', 'biomarker_driven_use')

# Trastuzumab first:
tras_string_list <- c(
  "Trastuzumab",
  "Trastuzumab deruxtecan",
  "Trastuzumab emtansine"
)
dft_cohort_any_tras <- dft_ind_mapped %>%
  filter(!is.na(mapped_cohort)) %>%
  filter(regulator %in% "FDA") %>%
  mutate(
    tras = component %in% tras_string_list
  ) %>%
  group_by(mapped_cohort) %>%
  summarize(has_any_tras = sum(tras, na.rm = T) > 0)
# On label indications exist for NSCLC and bladder, off label for everything else.

dft_tras <- dft_hdrug_det %>%
  # filter(cohort %in% pull(filter(dft_cohort_any_tras, !has_any_tras), mapped_cohort))
  # I'm going to go off script here a bit:  ALL of the NSCLC uses happened to be off label too.
  # So it's only breast cancer where this is approved and most were on label.
  filter(!cohort %in% "Breast") %>%
  group_by(cohort, record_id) %>%
  summarize(any_tras = any(agent %in% tras_string_list), .groups = "drop")

dft_tras <- left_join(
  dft_tras,
  select(dft_gen_mark, record_id, any_ERBB2),
  by = "record_id"
) %>%
  replace_na(list(any_ERBB2 = FALSE))

readr::write_rds(
  x = dft_tras,
  file = here(dir_output, 'tras_data.rds')
)


dft_tras_model <- glm(
  data = dft_tras,
  formula = any_tras ~ any_ERBB2,
  family = "binomial"
) %>%
  broom::tidy(., conf.int = T)

readr::write_rds(
  x = dft_tras_model,
  here(dir_output, 'tras_model.rds')
)

# for plotting:
mosaic_cohort_order <- c(
  "All",
  "Bladder",
  "Prostate",
  "Pancreas",
  "Breast",
  "CRC",
  "NSCLC"
)

dft_tras_stacked <- bind_rows(
  mutate(dft_tras, cohort = "All"),
  dft_tras
) %>%
  mutate(
    cohort_mosaic = factor(cohort, levels = mosaic_cohort_order),
    cohort = fct_inorder(cohort)
  ) %>%
  mutate(
    erbb2_disp = if_else(
      any_ERBB2,
      "ERBB2 altered (mut/amp)",
      "ERBB2 not altered"
    )
  )

# Decided I didn't want the 'all' group afterall.
dft_tras_stacked %<>%
  filter(!(cohort_mosaic %in% 'All')) %>%
  mutate(cohort_mosaic = fct_drop(cohort_mosaic))


gg_tras_mosaic <- ggplot(
  data = dft_tras_stacked,
) +
  geom_mosaic(
    aes(x = product(cohort_mosaic), fill = any_tras)
  ) +
  theme_mosaic() +
  facet_wrap(~erbb2_disp) +
  scale_fill_viridis_d(option = "magma", begin = 0, end = 0.5) +
  coord_flip() +
  guides(
    fill = guide_legend(title = "Trastuzumab used")
  ) +
  theme(
    axis.title.y = element_blank(),
    legend.position = "bottom",
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
  )

gg_tras_mosaic

readr::write_rds(
  x = gg_tras_mosaic,
  file = here(dir_output, 'tras_gg_mosaic.rds')
)


dft_tras_tabs <- dft_tras_stacked %>%
  rename(Trastuzumab = any_tras, ERBB2 = any_ERBB2) %>%
  nest(.by = cohort) %>%
  mutate(
    ft_obj = purrr::map(
      .x = data,
      .f = \(z) ft_two_by_two(z, var1 = "Trastuzumab", var2 = "ERBB2")
    ),
    gg_tab = purrr::map(
      .x = ft_obj,
      .f = ft_to_gg
    )
  )

gg_tras_tabs <- cowplot::plot_grid(
  plotlist = pull(dft_tras_tabs, gg_tab),
  labels = pull(dft_tras_tabs, cohort),
  label_size = 10,
  ncol = 3
)

readr::write_rds(
  x = gg_tras_tabs,
  file = here(dir_output, 'tras_gg_tab.rds')
)


################
# Vermurafenib #
################

# Trastuzumab first:
vemura_string_list <- c(
  "Vemurafenib"
)
dft_cohort_any_vemura <- dft_ind_mapped %>%
  filter(!is.na(mapped_cohort)) %>%
  filter(regulator %in% "FDA") %>%
  mutate(
    vemura = component %in% vemura_string_list
  ) %>%
  group_by(mapped_cohort) %>%
  summarize(has_any_vemura = sum(vemura, na.rm = T) > 0)
# On label indications do not exist for any of our cohorts (melanoma is the cannonical example)

dft_vemura <- dft_hdrug_det %>%
  group_by(cohort, record_id) %>%
  summarize(any_vemura = any(agent %in% vemura_string_list), .groups = "drop")

dft_vemura <- left_join(
  dft_vemura,
  select(dft_gen_mark, record_id, any_BRAF),
  by = "record_id"
) %>%
  replace_na(list(any_BRAF = FALSE))

readr::write_rds(
  x = dft_vemura,
  file = here(dir_output, 'vemura_data.rds')
)


# Fit a basic model for this:
dft_vemura_model <- glm(
  data = dft_vemura,
  formula = any_vemura ~ any_BRAF,
  family = "binomial"
) %>%
  broom::tidy(., conf.int = T)

readr::write_rds(
  x = dft_vemura_model,
  here(dir_output, 'vemura_model.rds')
)


dft_vemura_stacked <- bind_rows(
  mutate(dft_vemura, cohort = "All"),
  dft_vemura
) %>%
  mutate(
    cohort_mosaic = factor(cohort, levels = mosaic_cohort_order),
    cohort = fct_inorder(cohort)
  ) %>%
  mutate(
    BRAF_disp = if_else(
      any_BRAF,
      "BRAF altered (mut/amp)",
      "BRAF not altered"
    )
  )

# Decided I didn't want the 'all' group afterall.
dft_vemura_stacked %<>%
  filter(!(cohort_mosaic %in% 'All')) %>%
  mutate(cohort_mosaic = fct_drop(cohort_mosaic))

gg_vemura_mosaic <- ggplot(
  data = dft_vemura_stacked,
) +
  geom_mosaic(
    aes(x = product(cohort_mosaic), fill = any_vemura)
  ) +
  theme_mosaic() +
  facet_wrap(~BRAF_disp) +
  scale_fill_viridis_d(option = "magma", begin = 0, end = 0.5) +
  coord_flip() +
  guides(
    fill = guide_legend(title = "Vemurafenib used")
  ) +
  theme(
    axis.title.y = element_blank(),
    legend.position = "bottom",
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
  )

readr::write_rds(
  x = gg_vemura_mosaic,
  file = here(dir_output, 'vemura_gg_mosaic.rds')
)


dft_vemura_tabs <- dft_vemura_stacked %>%
  rename(Vemurafenib = any_vemura, BRAF = any_BRAF) %>%
  mutate(across(.cols = c(Vemurafenib, BRAF), .fns = factor)) %>%
  nest(.by = cohort) %>%
  mutate(
    ft_obj = purrr::map(
      .x = data,
      .f = \(z) ft_two_by_two(z, var1 = "Vemurafenib", var2 = "BRAF")
    ),
    gg_tab = purrr::map(
      .x = ft_obj,
      .f = ft_to_gg
    )
  )

gg_vemura_tabs <- cowplot::plot_grid(
  plotlist = pull(dft_vemura_tabs, gg_tab),
  labels = pull(dft_vemura_tabs, cohort),
  label_size = 10,
  ncol = 3
)

readr::write_rds(
  x = gg_vemura_tabs,
  file = here(dir_output, 'vemura_gg_tab.rds')
)
