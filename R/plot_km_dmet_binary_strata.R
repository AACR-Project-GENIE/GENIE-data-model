

plot_km_dmet_binary_strata <- function(
    dat,
    strata_var, 
    x_lab = "Years from distant metastasis",
    plot_title = "Overall survival (from metastasis)",
    pal = c("#004488", "#bb5566", "#ddaa33", "gray80")
) {
  
  sf2 <- survfit2(
    as.formula(
      paste0("Surv(
    time = tt_cpt_dmet_yrs_pos, 
    time2 = tt_os_dmet_yrs,
    event = os_dx_status
  ) ~ `", strata_var, "`")
    ),
    data = dat
  )
  
  gg <- sf2 %>%
    ggsurvfit() +
    add_risktable(
      risktable_stats = c(
        "n.risk",
        "cum.censor",
        "cum.event"
      ),
    ) + 
    scale_color_manual(
      name = strata_var,
      values = pal 
    ) +   
    add_quantile(y_value = 0.5,
                 linetype = 1,
                 alpha = 0.5,
                 size = 0.5,
                 color = pal[3]) + 
    scale_y_continuous(
      expand = c(0,0), 
      label = scales::label_percent()
    ) + 
    scale_x_continuous(
      name = "Years from distant metastasis",
      expand = c(0.025,0),
      breaks = seq(0, 100, by = 2.5)
    ) + 
    coord_cartesian(
      xlim = c(0, NA),
      ylim = c(0,1)
    ) + 
    labs(
      title = plot_title,
    ) + 
    theme(
      axis.title.y = element_blank(),
      plot.subtitle = element_markdown()
    )
  
  return(gg)
}
