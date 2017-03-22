#' camelCase
#'
#' @author Michael Steinbaugh
#'
#' @param string String
#'
#' @return String with camelCase formatting
#' @export
#'
#' @examples
#' camel("RNAi clone")
camel <- function(string) {
    string %>%
        snake %>%
        gsub("_(\\w?)", "\\U\\1", ., perl = TRUE)
}
