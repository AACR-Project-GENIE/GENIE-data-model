add_id <- function(dat, prefix = "id-", name = "id") {
  dat %>%
    mutate(
      {{name}} := paste0(
        prefix, stringr::str_pad(
          1:n(),
          width = ceiling(log10(n()))+1,
          side = "left",
          pad = "0")
      )
    )
}