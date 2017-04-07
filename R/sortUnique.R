#' Quickly perform sort unique on a vector
#'
#' The function also strips \code{NA} values. This is useful for gene list
#' server queries, for example.
#'
#' @author Michael Steinbaugh
#'
#' @importFrom stats na.omit
#'
#' @param vector Vector with duplicates, \code{NA} values
#'
#' @return Unique vector
#' @export
#'
#' @examples
#' sortUnique(c("milk", "eggs", "eggs", NA))
sortUnique <- function(vector) {
    vector %>%
        na.omit %>%
        sort %>%
        unique
}
