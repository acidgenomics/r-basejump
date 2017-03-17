#' Set names as camelCase
#'
#' @author Michael Steinbaugh
#' @keywords general
#'
#' @param data Data frame, list, tibble, etc.
#'
#' @return data Same data but with reformatted camelCase names
#' @export
setNamesCamel <- function(data) {
    setNames(data, camel(colnames(data)))
}
