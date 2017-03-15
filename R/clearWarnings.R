#' Clear warnings
#'
#' @export
clearWarnings <- function() {
    assign("last.warning", NULL, envir = baseenv())
}
