# There's a bug in survminer's facet function.  This is a workaround 
#   where we make each plot individually.  As it turns this was needed to plot
#   everyone alongside anyway.
plot_surv_age <- function(
    dat,
    bca_subgroup = NULL,
    bca_subgroup_var = 'bca_subtype_f_simple',
    pal = c("#8bc86a", "#319b47", "#027f85")
) {
  if (!is.null(bca_subgroup)) {
    dat %<>%
      filter(.data[[bca_subgroup_var]] %in% bca_subgroup)
  }
  
  if (nrow(dat) < 1) {
    cli_abort("Filtered down to zero rows with bca_subgroup = {bca_subgroup}")
  }
  n_for_title <- nrow(dat)
  
  surv_obj <- with(
    dat,
    Surv(
      time = tt_cpt_dmet_yrs_pos,
      time2 = tt_os_dmet_yrs,
      event = os_dmet_status
    )
  )
  
  fit <- survfit2(
    surv_obj ~ age_custom,
    data = dat
  )
  
  plt_title <- if (is.null(bca_subgroup)) {
    paste0("All participants", " (n=", n_for_title, ")")
  } else {
    paste0(bca_subgroup, " (n=", n_for_title, ")")
  }
  
  gg <- ggsurvfit(
    x = fit
  ) + 
    add_risktable(
      risktable_stats = c(
        "n.risk",
        "cum.censor",
        "cum.event"
      ),
      hjust = 0,
      risktable_height = 0.3,
      size = 3 
    ) +
    add_quantile(
      y_value = 0.5, linetype = 'solid', alpha = 0.3,
      color = "#ddaa33", linewidth = 0.5
    ) + 
    scale_y_continuous(
      expand = c(0,0),
      label = scales::label_percent(),
      name = "Survival"
    ) +
    scale_x_continuous(
      name = "OS from dmet (yrs)",
      expand = expansion(add = 0, mult = c(0, 0.15)), # needed to prevent clipping
      breaks = 0:100
    ) +
    scale_color_manual(
      values = pal
    ) +
    coord_cartesian(
      xlim = c(0, 5.01),
      ylim = c(0,1.01),
      expand = T
    ) +
    labs(
      title = plt_title
    ) +
    theme(
      axis.title.y = element_blank(),
      plot.title.position = "plot",
      title = element_markdown(),
      # prevents the axis tick label clipping:
      plot.margin=unit(c(.2,.2,.2,.2),"cm")
    )
  
  return(gg)
  
}
