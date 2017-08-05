#' Fix Character Strings Missing `NA`
#'
#' @rdname fixNA
#' @name fixNA
#'
#' @return Object containing proper `NA` values.
#'
#' @examples
#' fixNA(c(1L, "x", "", "NA"))
NULL



#' @rdname fixNA
#' @export
setMethod("fixNA", "ANY", function(object) {
    object
})



#' @rdname fixNA
#' @export
setMethod("fixNA", "character", function(object) {
    gsub("^$|^\\s+$|^NA$", NA, object)
})
