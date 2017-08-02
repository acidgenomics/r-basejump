#' Clear Warnings
#'
#' @keywords internal
#'
#' @return No value.
#' @export
clearWarnings <- function() {
    assign("last.warning", NULL, envir = baseenv())
}
