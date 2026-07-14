save_table_as_csv <- function(
  synid,
  save_file
) {
  dat <- as.data.frame(synTableQuery(
    paste0('select * from ', synid)
  ))
  readr::write_rds(dat, save_file)
}
