#' Fix Character Strings Missing `NA`
#'
#' @rdname fixNA
#' @name fixNA
#' @family Cleanup Utilities
#'
#' @inheritParams AllGenerics
#'
#' @return Object containing proper `NA` values.
#'
#' @examples
#' fixNA(c(1L, "x", "", "NA"))
#'
#' data.frame(a = c("foo", ""),
#'            b = c(NA, "bar")) %>%
#'     fixNA()
NULL



# Constructors ====
.fixNAVec <- function(object) {
    gsub("^$|^\\s+$|^NA$", NA, object)
}



.fixNATidy <- function(object) {
    mutate_all(object, funs(.fixNAVec))
}



# Methods ====
#' @rdname fixNA
#' @export
setMethod(
    "fixNA",
    signature("ANY"),
    function(object) {
    # Return unmodified by default
    object
})



#' @rdname fixNA
#' @export
setMethod(
    "fixNA",
    signature("character"),
    .fixNAVec)



#' @rdname fixNA
#' @export
setMethod(
    "fixNA",
    signature("data.frame"),
    .fixNATidy)



#' @rdname fixNA
#' @export
setMethod(
    "fixNA",
    signature("DataFrame"),
    function(object) {
        apply(object, 2L, .fixNAVec) %>%
            as("DataFrame")
    })



#' @rdname fixNA
#' @export
setMethod(
    "fixNA",
    signature("tbl_df"),
    .fixNATidy)
