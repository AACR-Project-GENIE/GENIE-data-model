library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

library(khroma)

all_dat <- readr::read_rds(
  here('data', 'all_dat.rds')
)

df <- data.frame(
  'level1'=c('a', 'a', 'a', 'b', 'b', 'b', 'c', 'c'), 
  'level2'=c('AA', 'BB', 'CC', 'AA', 'BB', 'CC', 'AA', 'BB'), 
  'value'=c(12.5, 12.5, 75, 50, 25, 25, 36, 64)
)

df %<>% mutate(id = seq(1, n()))

ggplot(df, 
       aes(y = value, group = id)) +
  geom_col(color = 'black', aes(fill = level1, x = 0.25), width = .25) + 
  geom_col(aes(fill = level2, x = .5), width = .25) #+
#  coord_polar(theta = 'y', start = 0, clip = "off")

# just putting this into a format that's more like what I'd get from our data.
df_long <- tibble(
  'lev_1' = c('a', 'a', 'a', 'b', 'b', 'b', 'c', 'c'),
  'lev_2' = c('a', 'b', 'c', 'a', 'b', 'c', 'a', 'b'),
  'n'=c(12.5, 12.5, 75, 50, 25, 25, 36, 64)
)

# do this while it's "arrange(a,b,c)" etc format.
df_long %<>% mutate(y_pos = seq(1, n()))


df_long %<>%
  pivot_longer(
    cols = contains("lev_"),
    names_to = "regimen_number",
    values_to = "regimen_drugs"
  ) %>%
  select(regimen_drugs, regimen_number, everything()) %>%
  mutate(
    regimen_number = str_replace(regimen_number, "lev_", ""),
    regimen_number = as.numeric(regimen_number)
  ) %>%
  mutate(
    x_pos = regimen_number * 0.25 - 0.25
  )
  
ggplot(df_long,
       aes(x = regimen_number, y = n, group = y_pos)
) +
  geom_col(aes(fill = regimen_drugs), width = 1) 


all_reg <- all_dat %>% 
  filter(dat_name %in% "reg") %>%
  select(cohort, dat) 

all_reg_lim <- all_reg %>%
  mutate(
    dat = purrr::map(
      .x = dat,
      .f = \(x) {
        select(
          x,
          record_id, ca_seq, regimen_number, 
          drugs_num, regimen_drugs
        )
      }
    )
  ) %>%
  unnest(dat)

all_reg_lim_wide <- all_reg_lim %>%
  filter(regimen_number %in% 1:3) 

immuno_list <- c(
  'Pembrolizumab',
  'Nivolumab',
  'Ipilimumab',
  'Atezolizumab',
  'Avelumab',
  'Rituximab'
)

all_reg_lim %<>%
  mutate(
    regimen_group = case_when(
      str_detect(regimen_drugs, paste(immuno_list, collapse = "|")) ~ "IC inhibitor",
      T ~ "non-IC regimens"
    )
  )


# all_reg_lim_wide %<>%
#   mutate(
#     regimen_group = case_when(
#       str_detect(regimen_drugs, "Investigational") ~ "Investigational",
#       T ~ "Approved Only"
#     )
#   ) 

all_reg_lim_wide %<>%
  select(cohort, record_id, ca_seq, regimen_group, regimen_number) %>%
  pivot_wider(
    names_from = "regimen_number",
    names_prefix = "reg_",
    values_from = "regimen_group"
  )

# Some problems since we can't use regimen_number within cancer - just removing
#. for now.  
# NOT A LONG TERM SOLUTION.
all_reg_lim_wide %<>%
  filter(!is.na(reg_1))

all_reg_lim_wide %<>%
  count(cohort, reg_1, reg_2, reg_3) %>%
  arrange(reg_1, reg_2, reg_3) %>%
  group_by(cohort) %>%
  mutate(
    y_pos_max = cumsum(n),
    y_pos_min = lag(y_pos_max, default = 0)
  ) %>%
  ungroup(.)

all_reg_plot <- all_reg_lim_wide %>%
  pivot_longer(
    cols = contains("reg_"),
    names_to = "regimen_number",
    values_to = "regimen_group"
  ) %>%
  select(cohort, regimen_group, regimen_number, everything()) %>%
  mutate(
    regimen_number = str_replace(regimen_number, "reg_", ""),
    regimen_number = as.numeric(regimen_number)
  ) %>%
  mutate(
    x_pos_min = regimen_number - 1,
    x_pos_max = regimen_number 
  )

all_reg_plot %<>% filter(!is.na(regimen_group))

gg_flat <- ggplot(
  all_reg_plot,
  aes(
    xmin = x_pos_min, 
    xmax = x_pos_max,
    ymin = y_pos_min,
    ymax = y_pos_max,
    fill = regimen_group)
) +
  geom_rect() +
  geom_vline(
    xintercept = 0:max(all_reg_plot$regimen_number),
    color = 'gray20', size = 0.25
  ) +
  facet_wrap(vars(cohort)) + 
  scale_fill_vibrant() + 
  coord_cartesian(expand = F) + 
  theme_void()


gg_flat


cp <- coord_polar(theta = "y")
cp$is_free <- function() TRUE




gg_polar <- ggplot(
  all_reg_plot,
  aes(
    xmin = x_pos_min, 
    xmax = x_pos_max,
    ymin = y_pos_min,
    ymax = y_pos_max,
    fill = regimen_group,
    color = regimen_group)
) +
  geom_rect() +
  geom_vline(
    xintercept = 0:max(all_reg_plot$regimen_number),
    color = 'gray20', size = 0.25
  ) +
  facet_wrap(vars(cohort), scales = "free_y") + 
  scale_fill_vibrant() + 
  scale_color_vibrant() + 
  cp + 
  theme_void()

gg_polar
  
