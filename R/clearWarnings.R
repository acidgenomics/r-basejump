#' Clear warnings
#'
#' @author Michael Steinbaugh
#'
#' @keywords internal
#'
#' @export
clearWarnings <- function() {
    assign("last.warning", NULL, envir = baseenv())
}
