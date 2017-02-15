#' log2 antilog conversion
#' Convert a string of log2 normalized values (e.g. fold changes) for better
#' readability.
#' @export
#' @keywords math
#' @param log2 \code{numeric} vector of log base 2 normalized values.
#' @return Antilog \code{numeric} vector with proper sign.
#' @examples
#' antilog2(c(-2, -1, 0, 1, 2))
antilog2 <- function(log2) {
    # Sign is -1 if log2 < 0; 1 if log2 >= 0
    sign <- (-1) ^ (1 + as.numeric(log2 >= 0))
    sign * 2 ^ abs(log2)
}
