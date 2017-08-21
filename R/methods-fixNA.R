#' Fix Character Strings Missing `NA`
#'
#' @rdname fixNA
#' @name fixNA
#'
#' @return Object containing proper `NA` values.
#'
#' @examples
#' fixNA(c(1L, "x", "", "NA"))
#'
#' data.frame(a = c("foo", ""),
#'            b = c(NA, "bar")) %>%
#'     fixNA
NULL



# Constructors ====
.fixNAVec <- function(object) {
    if (is.character(object)) {
        gsub("^$|^\\s+$|^NA$", NA, object)
    } else {
        object
    }
}



.fixNATidy <- function(object) {
    if (!is.data.frame(object)) {
        stop("Object must be 'data.frame' class")
    }
    mutate_all(object, funs(.fixNAVec))
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
setMethod("fixNA", "data.frame", .fixNATidy)



#' @rdname fixNA
#' @export
setMethod("fixNA", "DataFrame", function(object) {
    apply(object, 2L, .fixNAVec) %>%
        as("DataFrame")
})



#' @rdname fixNA
#' @export
setMethod("fixNA", "tbl_df", .fixNATidy)
