#' Redcap magic numbers to the data dictionary meaning.
#'
#' @param vec The vector to operate on.
#' @param key_val_pairs Pre-processed key value pairs stored as a named vector.
#' @param remove_names Remove the names from the output vector?
#'
#' @returns A vector with the conversions completed.
#' @export
#'
#' @examples
redcap_numbers_to_meaning <- function(
  vec,
  key_val_pairs,
  remove_names = F
) {
  ind <- match(vec, names(key_val_pairs))
  if (remove_names) {
    names(key_val_pairs) <- NULL
  }
  key_val_pairs[ind]
}
