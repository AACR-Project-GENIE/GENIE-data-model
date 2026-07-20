plot_hr_rel_scatter_gene_clin_rescale <- function(
    dat,
    legend_title = NULL,
    plot_subtitle = NULL,
    plot_title = NULL,
    pal = c("#004488", "#bb5566"),
    txt_size = 2.5,
    x_var = "hr",
    x_lab = "Mean cumulative hazard ratio",
    y_lab = "Selection<br>frequency",
    x_trans = scales::log_trans(base = 2),
    leg_pos = "right",
    title_pos = "plot"
) {
  
  dat %<>%
    mutate(
      type = case_when(
        str_detect(term, "_gene$") ~ "Genetic",
        str_detect(term, "_gof$") ~ "Genetic",
        str_detect(term, "_lof$") ~ "Genetic",
        str_detect(term, "_mut$") ~ "Genetic",
        str_detect(term, "_cna$") ~ "Genetic",
        # Amp is the same as CNA in this analysis, renamed for collaborator preference
        str_detect(term, "_amp$") ~ "Genetic",
        str_detect(term, "_fus$") ~ "Genetic",
        T ~ "Clinical"
      ),
      type = factor(type, levels = c("Genetic", "Clinical")),
      term = as.character(term),
      term = str_replace(term, 
                         # here we keep the mut/cna/fus and gof/lof labels.
                         pattern = "_gene",
                         replacement = "")
    )
  
  gg <- ggplot(data = dat,
               aes(x = .data[[x_var]], y = stability, color = type)) +
    geom_vline(xintercept = 1, linewidth = 2, color = "#009988", alpha = 0.2) + 
    geom_point() +
    geom_text_repel(aes(label = term), color = "black",
                    size = txt_size) + 
    theme_bw() +
    scale_color_manual(
      name = legend_title, 
      values = pal,
      drop = F
    ) + 
    labs(title = plot_title,
         subtitle = plot_subtitle, 
         x = "Mean cumulative hazard ratio",
         y = y_lab) + 
    scale_y_continuous(
      limits = c(NA, 1),
      labels = scales::percent_format()) + 
    scale_x_continuous(
      n.breaks = 8,
      expand = expansion(add = 0, mult = 0.1)
    ) + 
    coord_trans(
      x = x_trans
    ) + 
    theme(
      axis.title.y = element_markdown(vjust = 0.5, angle = 0, hjust = 0),
      plot.title.position = title_pos,
      legend.position = leg_pos
    )
  
  
  return(gg)
  
}

