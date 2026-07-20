# A very small helper to get only the elements of a vector that occur one time.
vec_unique_only <- function(x) {
  x[!duplicated(x) & !duplicated(x, fromLast = TRUE)]
}
