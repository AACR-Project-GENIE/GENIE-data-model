#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' Injection operator
#'
#' See \code{rlang::\link[rlang:inject]{:=}} for details.
#'
#' @name :=
#' @rdname inject
#' @keywords internal
#' @export
#' @importFrom rlang :=
#' @usage x := y
#' @param x Name of the new variable.
#' @param y An expression of what value to assign to the new variable.
#' @return Similar to a = b except it works in special cases.
NULL
