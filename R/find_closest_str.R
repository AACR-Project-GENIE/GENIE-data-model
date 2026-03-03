#' Find the closest string match
#'
#' @param str string(s) to match
#' @param dict the selection of strings to match to.
#' @param method passed on to `stringdist::amatch()`
#'
#' @returns The closest match for each `str`
#' @export
#'
#' @examples
find_closest_str <- function(
  str,
  dict,
  method = 'lcs'
) {
  dict[
    stringdist::amatch(
      method = method,
      maxDist = Inf,
      x = str,
      table = dict
    )
  ]
}
