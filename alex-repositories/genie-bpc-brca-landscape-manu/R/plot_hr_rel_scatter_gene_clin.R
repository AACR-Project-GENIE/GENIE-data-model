plot_hr_rel_scatter_gene_clin <- function(
    dat,
    legend_title = NULL,
    plot_subtitle = NULL,
    plot_title = NULL,
    pal = c("#004488", "#bb5566"),
    txt_size = 2.5) {
  
  dat %<>%
    mutate(
      type = case_when(
        str_detect(term, "_gene$") ~ "Genetic",
        str_detect(term, "_gof$") ~ "Genetic",
        str_detect(term, "_lof$") ~ "Genetic",
        str_detect(term, "_mut$") ~ "Genetic",
        str_detect(term, "_cna$") ~ "Genetic",
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
               aes(x = log_hr, y = stability, color = type)) +
    geom_vline(xintercept = 0, linewidth = 2, color = "#009988", alpha = 0.2) + 
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
         x = "Log cumulative hazard ratio",
         y = "Stability") + 
    scale_y_continuous(limits = c(NA, 1)) + 
    theme(
      axis.title.y = element_markdown(vjust = 0.5, angle = 0),
      plot.title.position = "plot"
    )
  
  
  return(gg)
  
}
