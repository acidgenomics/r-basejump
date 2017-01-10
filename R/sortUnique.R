#' Sort unique.
#'
#' @export
#' @importFrom stats na.omit
#' @param vector \code{vector} with duplicates, \code{NA} values.
#' @return vector
sortUnique <- function(vector) {
    vector %>%
        stats::na.omit(.) %>%
        sort %>%
        unique
}
