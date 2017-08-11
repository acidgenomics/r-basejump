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
    warning("'fixNA()' only modifies a character vector")
    object
})



#' @rdname fixNA
#' @export
setMethod("fixNA", "character", function(object) {
    gsub("^$|^\\s+$|^NA$", NA, object)
})
