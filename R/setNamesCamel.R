#' Set names as camelCase
#'
#' @author Michael Steinbaugh
#' @keywords general
#'
#' @param data \code{data.frame}, \code{list}, or \code{tibble}
#'
#' @return data Same data but with reformatted camelCase names
#' @export
setNamesCamel <- function(data) {
    setNames(data, camel(colnames(data)))
}
