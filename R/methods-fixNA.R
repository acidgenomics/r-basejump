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
#' fixNA(c(1L, "x", "", "NA", "NULL"))
#'
#' data.frame(a = c("foo", ""),
#'            b = c(NA, "bar")) %>%
#'     fixNA()
NULL



# Constructors =================================================================
.fixNAString <- function(object) {
    patterns <- c(
        "^$",
        "^\\s+$",
        "^NA$",
        "^NULL$")
    gsub(x = object,
         pattern = paste(patterns, collapse = "|"),
         replacement = NA)
}



#' @importFrom dplyr funs mutate_if
.fixNATidy <- function(object) {
    mutate_if(object, is.character, funs(.fixNAString))
}



# Methods ======================================================================
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
    .fixNAString)



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
        object %>%
            as.data.frame() %>%
            .fixNATidy() %>%
            as("DataFrame")
    })



#' @rdname fixNA
#' @export
setMethod(
    "fixNA",
    signature("tbl_df"),
    .fixNATidy)
