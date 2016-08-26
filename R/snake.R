#' snake_case
#'
#' @param alphanum Alphanumeric \code{string}.
#'
#' @return \code{string} with snake_case formatting.
#' @export
snake <- function(alphanum) {
  x <- alphanum
  x <- gsub("(\\(|\\))", "", x)
  x <- gsub("(\\.|-| )", "_", x)
  x <- gsub("([A-Z]{1})([A-Z]+)", "\\1\\L\\2", x, perl = TRUE)

  # Convert camelCase to snake_case
  x <- gsub("([a-z0-9])([A-Z])", "\\1_\\L\\2", x, perl = TRUE)

  # Lowercase all
  x <- tolower(x)
  return(x)
}
