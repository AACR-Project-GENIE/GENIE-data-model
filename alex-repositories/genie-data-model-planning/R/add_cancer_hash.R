# A fairly hacky function that defines a cancer based on a few variables.  This is not fully unique, so we'll filter to unique cases after using it to get a convenient validation sample.
add_cancer_hash <- function(dat) {
  dat %>%
    mutate(
      .hash_str = paste(
        record_id,
        dob_ca_dx_days,
        naaccr_laterality_cd,
        naaccr_clin_stage_cd,
        sep = "|"
      ),
      cancer_hash = purrr::map_chr(
        .x = .hash_str,
        .f = rlang::hash
      )
    ) %>%
    drop_dots(.)
}
