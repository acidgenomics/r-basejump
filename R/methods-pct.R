#' Convert Numeric to Percentage
#'
#' @param number Number.
#'
#' @return Percentage.
#' @export
#'
#' @examples
#' pct(0.1)
setMethod("pct", "numeric", function(object) {
    sprintf("%1.1f%%", object * 100L)
})
