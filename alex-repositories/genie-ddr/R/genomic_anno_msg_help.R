genomic_anno_msg_help <- function(dat) {
  dat_name <- deparse(substitute(dat))

  tab <- tabyl(dat, ANNOTATED) %>%
    filter(ANNOTATED) # T/F so this grabs the annotated pct line.

  n_levels_oncogenic <- count(dat, ONCOGENIC) %>%
    nrow(.)

  cli::cli_alert_info(
    text = glue(
      "{dat_name}: Of the {tab$n} rows, {round(tab$percent*100,1)}% were annotated."
    )
  )

  # just a check that everything wasn't tagged the same way,
  #   or something equally nonsensical.
  cli::cli_alert_info(
    text = glue(
      "{dat_name}: {n_levels_oncogenic} levels for ONCOGENIC (just hoping >= 2)."
    )
  )
}
