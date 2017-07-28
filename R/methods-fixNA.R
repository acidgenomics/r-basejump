#' Fix Character Strings Missing `NA`
#'
#' @rdname fixNA
#'
#' @param string String missing `NA`.
#'
#' @return String containing `NA`.
#'
#' @examples
#' fixNA(c(1, "x", "", "NA"))



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
