#' camelCase
#'
#' @param alphanum Alphanumeric string
#'
#' @return stringWithCamelCaseFormatting
#' @export
camel <- function(alphanum) {
  x <- alphanum

  # Remove parentheses
  x <- gsub("(\\(|\\))", "", x)

  # Convert all periods, dashes, spaces to underscores
  x <- gsub("(\\.|-| )", "_", x)

  # Convert acronymes to Mixed Case
  x <- gsub("([A-Z]{1})([A-Z]+)", "\\1\\L\\2", x, perl = TRUE)

  # Lowercase first letter
  x <- paste0(tolower(substr(x, 1, 1)), substr(x, 2, nchar(x)))

  # Convert snake_case to camelCase
  x <- gsub("_(\\w?)", "\\U\\1", x, perl = TRUE)
  return(x)
}
