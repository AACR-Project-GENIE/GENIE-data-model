library(tidyverse)
library(ggforce)
library(ggtext)

coh_dat <- readr::read_rds('cohort_data.rds')

# Specify some positions for these:
pos_addon <- tribble(
  ~name,
  ~x_pos,
  ~y_pos,
  ~x_lab_pos,
  ~y_lab_pos,

  "GENIE",
  0,
  0,
  0,
  0,

  "BPC public",
  0.75,
  0,
  0.75,
  0,

  "BPC consortium",
  0.75,
  0,
  0.625,
  0.35,

  "Sponsored projects",
  0.2,
  -0.6,
  0.3,
  -0.4,

  "Additional projects",
  -0.2,
  -0.6,
  -0.3,
  -0.4
)

coh_dat <- coh_dat %>%
  mutate(
    lab = paste0(
      lab,
      "<br>[",
      formatC(n, format = 'f', big.mark = ",", digits = 0),
      "]"
    )
  )


coh_dat <- left_join(
  coh_dat,
  pos_addon,
  by = "name",
  relationship = "one-to-one"
)


coh_dat <- coh_dat %>%
  mutate(
    n_prop = n / max(n),
    n_rad = sqrt(n_prop)
  )

coh_dat <- coh_dat |>
  mutate(
    name = fct_inorder(name),
    color = fct_inorder(color)
  )


gg_with_labs <- ggplot(coh_dat) +
  ggforce::geom_circle(
    aes(fill = color, r = n_rad, x0 = x_pos, y0 = y_pos)
  ) +
  geom_richtext(
    aes(label = lab, x = x_lab_pos, y = y_lab_pos),
    fill = NA,
    label.color = NA,
    color = 'black',
    vjust = 0.5,
    hjust = 0.5,
    size = 3,
    show.legend = FALSE
  ) +
  scale_fill_identity() +
  coord_fixed() +
  theme_void()

ggsave(plot = gg_with_labs, "circles_labels.svg", height = 5, width = 5)

gg_no_labs <- ggplot(coh_dat) +
  ggforce::geom_circle(
    aes(fill = color, r = n_rad, x0 = x_pos, y0 = y_pos)
  ) +
  scale_fill_identity() +
  coord_fixed() +
  theme_void()

ggsave(plot = gg_no_labs, "circles_no_labels.svg", height = 5, width = 5)
