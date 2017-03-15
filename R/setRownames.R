#' Set row names
#'
#' Useful utility function for setting rownames in a chain operation.
#'
#' @author Michael Steinbaugh
#' @keywords general
#'
#' @param df \code{data.frame}
#' @param column Column to use for \code{rownames}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' setRownames(df, "description")
#' }
setRownames <- function(df, column) {
    df <- as.data.frame(df)
    rownames(df) <- df[[column]]
    return(df)
}
