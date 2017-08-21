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



# Constructors ====
.fixNAVec <- function(object) {
    gsub("^$|^\\s+$|^NA$", NA, object)
}



.fixNADim <- function(object) {
    apply(object, 2L, .fixNAVec)
}



# Methods ====
#' @rdname fixNA
#' @export
setMethod("fixNA", "ANY", function(object) {
    object
})



#' @rdname fixNA
#' @export
setMethod("fixNA", "character", .fixNAVec)



#' @rdname fixNA
#' @export
setMethod("fixNA", "data.frame", .fixNADim)



#' @rdname fixNA
#' @export
setMethod("fixNA", "DataFrame", .fixNADim)



#' @rdname fixNA
#' @export
setMethod("fixNA", "tbl_df", .fixNADim)
