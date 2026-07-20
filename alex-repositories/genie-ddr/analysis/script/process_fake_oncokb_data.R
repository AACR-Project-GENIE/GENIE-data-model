library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

mut_orig <- read_rds(
  here('analysis', 'explore', 'unreproducible_variant_sample.rds')
)
mut_orig %<>% ungroup(.)
  
mut_fake <- fread(
  here('analysis', 'explore', 'mut_test_onco.txt')
)

mut <- bind_rows(
  (mut_fake %>%
     mutate(actual_data = F) %>%
     select(actual_data, everything())),
  (mut_orig %>%
    mutate(actual_data = T) %>%
    select(actual_data, everything()))
)

mut %<>% replace_na(list(HGVSp_Short = ""))

mut %<>%
  mutate(
    ONCOGENIC = factor(
      ONCOGENIC, 
      levels = c("Unknown", "Inconclusive", "Resistance", "Likely Neutral", "Likely Oncogenic", "Oncogenic")
    ),
    var = glue('{Hugo_Symbol}:{HGVSp_Short}'),
    onco_code = factor(ONCOTREE_CODE)
  )

gg <- ggplot(
  mut,
  aes(x = ONCOGENIC, y = var, fill = onco_code, stroke = actual_data) 
) + 
  theme_bw() +
  geom_jitter(shape = 21, width = 0.2, height= 0.4, alpha = 0.8) +
  scale_fill_viridis_d(option = 'turbo') +
  labs(
    title = "30 random variants tagged with different oncotree codes",
    subtitle = "The black border is the true oncotree code"
  ) + 
  theme(
    plot.title.position = 'plot',
    legend_title = element_text(size = 4),
    legend.text = element_text(size = 4),
    legend.key.spacing = unit(0.01, 'cm'),
    legend.position = 'bottom'
  )

ggsave(
  gg,
  filename =here('analysis', 'explore', 'oncotag_test.pdf'),
  height = 10, width = 8
)

