#' Convert Numeric to Percentage
#'
#' @rdname pct
#' @name pct
#' @family Math Utilities
#'
#' @inheritParams AllGenerics
#'
#' @return Percentage.
#' @export
#'
#' @examples
#' pct(0.1)
NULL



# Methods ====
#' @rdname pct
#' @export
setMethod("pct", "numeric", function(object) {
    sprintf("%1.1f%%", object * 100L)
})
