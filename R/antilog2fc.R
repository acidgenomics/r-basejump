#' Log base2 fold change antilog conversion
#'
#' Convert a string of log2 normalized fold change values for better
#' readability.
#'
#' @param log2fc Numeric vector of log normalized fold change values.
#'
#' @return Antilog numeric vector of fold change values with proper sign.
#' @export
antilog2fc <- function(log2fc) {
  # Sign is -1 if log2fc < 0; 1 if log2fc >= 0
  sgn <- (-1)^(1 + as.numeric(log2fc >= 0))
  fc <- sgn * 2^abs(log2fc)
  return(fc)
}

# sgn = rep(-1, length(log2fc))
# sgn[log2fc >= 0] = 1
