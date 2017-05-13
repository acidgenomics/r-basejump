#' Clear warnings
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



#' @rdname clearWarnings
#' @usage NULL
#' @export
clear_warnings <- clearWarnings
