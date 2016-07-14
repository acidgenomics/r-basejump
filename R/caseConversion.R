#' camelCase
#'
#' @param x Alphanumeric string
#'
#' @return stringWithCamelCaseFormatting
#' @export
camel <- function(x) {
  # Convert all periods and dashes to underscores
  x <- gsub("(\\.|-)", "_", x)

  # Convert acronymes to Mixed Case
  x <- gsub("([A-Z]{1})([A-Z]+)", "\\1\\L\\2", x, perl = TRUE)

  # Lowercase first letter
  x <- paste0(tolower(substr(x, 1, 1)), substr(x, 2, nchar(x)))

  # Convert snake_case to camelCase
  x <- gsub("_(\\w?)", "\\U\\1", x, perl = TRUE)
  return(x)
}

#' snake_case
#'
#' @param x Alphanumeric string
#'
#' @return string_with_snake_case_formatting
#' @export
snake <- function(x) {
  x <- gsub("(\\.|-)", "_", x)
  x <- gsub("([A-Z]{1})([A-Z]+)", "\\1\\L\\2", x, perl = TRUE)
  # Convert camelCase to snake_case
  x <- gsub("([a-z0-9])([A-Z])", "\\1_\\L\\2", x, perl = TRUE)

  x <- tolower(x)
  return(x)
}
