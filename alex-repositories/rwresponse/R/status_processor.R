#' @title Process the status column from med onc or imaging data.
#' @description
#' Takes the long names curators need like Progressing/Worsening/Enlarging and makes them into shorter things like "worsening"
#'
#' @param dat Any dataframe with a status column.
#' @param col_name String column name
#' @export
status_processor <- function(
  dat,
  col_name,
  output_name = "status_change"
) {
  resp_lev <- c(
    'worsening',
    'stable',
    'mixed',
    'improving'
  )

  rtn <- dat |>
    dplyr::mutate(
      .temp = dplyr::case_when(
        .data[[col_name]] %in% "Progressing/Worsening/Enlarging" ~ resp_lev[1],
        .data[[col_name]] %in% "Stable/No change" ~ resp_lev[2],
        .data[[col_name]] %in% "Mixed" ~ resp_lev[3],
        .data[[col_name]] %in% "Improving/Responding" ~ resp_lev[4],
        T ~ NA_character_
      ),
      .temp = factor(.temp, levels = resp_lev)
    )

  rtn <- dplyr::rename(rtn, {{ output_name }} := .temp)

  return(rtn)
}
