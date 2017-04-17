#' Clear warnings
#'
#' @author Michael Steinbaugh
#'
#' @export
#'
#' @examples
#' \dontrun{
#' clearWarnings()
#' }
clearWarnings <- function() {
    assign("last.warning", NULL, envir = baseenv())
}
