library(ggplot2)








dft_cast_blocks_sub <- dft_cast_blocks %>% filter(record_id %in% pt_subset)
dft_event_sub <- dft_event %>% filter(record_id %in% pt_subset)

dft_cast_blocks_sub %<>%
  arrange(tt_os_dx_yrs) %>%
  mutate(record_id = forcats::fct_inorder(record_id))
dft_event_sub %<>% 
  mutate(record_id = factor(
    record_id, 
    levels = levels(dft_cast_blocks_sub$record_id))
  )

ggplot(filter(dft_cast_blocks_sub),
       aes(y = record_id, yend = record_id)) + 
  geom_segment(aes(x = dx_block_start, 
                   xend = dx_block_end, 
                   color = md_cast_status_f),
               linewidth = 1) +
  geom_point(data = dft_event_sub,
             aes(x = t_yrs, shape = event),
             stroke = 1) + 
  scale_x_continuous(
    name = "Time from diagnosis (years)",
    expand = expansion(add = c(0,0), mult = c(0, 0.05))) + 
  theme_bw() + 
  theme(
    legend.position = "top",
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank()
  ) + 
  scale_color_manual(
    name = NULL, 
    values = c("gray80", "#6699cc", "#ee99aa"),
    guide = guide_legend(ncol = 1)
  ) + 
  scale_shape_manual(
    name = NULL,
    values = c(18,4,21),
    guide = guide_legend(ncol = 1)
  )


dft_reg_sub <- dft_regimen %>% 
  filter(record_id %in% pt_subset$record_id) %>%
  left_join(., pt_subset, by = "record_id") %>%
  select(-record_id)

dft_ca_ind %>%
  select(os_dx_status, tt_os_dx_yrs)



ggplot(filter(dft_cast_blocks, record_id %in% pt_subset),
       aes(y = record_id, yend = record_id)) + 
  geom_segment(aes(x = dx_md_visit_yrs, 
                   xend = dx_block_end, 
                   color = md_pca_status)) + 
  theme_bw() + 
  theme(legend.position = "bottom")

# Todos:
#   - Get the "make blocks" steps into a function - optional filter on the CRPC HSPC designation.
#   - Tune up the plot specs.  Consider doing a plot with X = age as well.










tabyl(md_pca_status)


data_process_swimmer <- function(ca_ind, reg, pt_selection) {
  # assign a random order to participants:
  record_id_levs = factor(sample(pt_selection))
  
  ca_ind_sub <- ca_ind %>%
    select(record_id, os_dx_status, tt_os_dx_yrs)
  
  dft_reg_sub <- dft_regimen %>% 
    filter(record_id %in% pt_selection) %>%
    mutate(record_id_f = factor(record_id, levels = record_id_levs)) %>%
    left_join(., ca_ind_sub, by = record_id)
}


library(ggplot2)



ggplot(data = dft_reg_sub) + 
  geom_segment(aes(x = dx_reg_start_int_yrs,
                   xend = dx_reg_end_all_int_yrs,
                   y = scrambled_id,
                   yend = scrambled_id)) + 
  theme_classic()
