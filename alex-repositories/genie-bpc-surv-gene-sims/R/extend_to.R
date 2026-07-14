
#' @description A simple utility file that repeats a vector (x) until a desired length is achieved.
extend_to <- function(x, len) {
  rtn <- rep(x, times = ceiling(len/length(x)))
  rtn[1:len]
}