#' Clear warnings
#'
#' @author Michael Steinbaugh
#'
#' @keywords internal
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
