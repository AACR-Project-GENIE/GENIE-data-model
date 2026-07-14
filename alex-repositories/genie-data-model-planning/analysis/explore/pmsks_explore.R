# "redacted" is my old name for the pre-msk stats.
dir_redacted <- here('data-raw', 'bpc', 'step3-redacted')

path_help <- function(num) {
  readr::read_csv(
    here(dir_redacted, paste0('prissmm_pathology_part_', num, '.csv'))
  )
}
path_1 <- path_help(1)
path_2 <- path_help(2)
path_3 <- path_help(3)
path_4 <- path_help(4)
path_5 <- path_help(5)
path_6 <- path_help(6)

colnames(path_1)
colnames(path_2)
colnames(path_3)
colnames(path_4)
colnames(path_5)
colnames(path_6)

# This is pointless now, these tables won't even exist for the Bayer HER2 project.
