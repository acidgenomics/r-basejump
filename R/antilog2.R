#' Log base 2 antilog conversion
#'
#' Convert a string of log2 normalized values (e.g. fold changes) for better
#' readability.
#'
#' @param log2 \code{numeric} vector of log base 2 normalized values.
#'
#' @return Antilog \code{numeric} vector with proper sign.
#' @export
antilog2 <- function(log2) {
  # Sign is -1 if log2 < 0; 1 if log2 >= 0
  sgn <- (-1) ^ (1 + as.numeric(log2 >= 0))
  fc <- sgn * 2 ^ abs(log2)
  return(fc)
}
