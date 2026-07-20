plot_hr_rel_scatter_gof_lof <- function(
    dat,
    legend_title = NULL,
    plot_subtitle = NULL,
    plot_title = NULL,
    pal = c("#ddaa33", "#bb5566", "#004488", "gray80"),
    txt_size = 2.5) {
  
  dat %<>%
    mutate(
      type = case_when(
        str_detect(term, "_gene$") ~ "All",
        str_detect(term, "_gof$") ~ "GOF",
        str_detect(term, "_lof$") ~ "LOF",
        T ~ "Other"
      ),
      type = factor(type, levels = c("All", "GOF", "LOF", "Other")),
      term = as.character(term),
      term = str_replace(term, 
                         pattern = "_gene|_gof|_lof",
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
