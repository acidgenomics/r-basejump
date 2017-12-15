#' Remove Rows and Columns Containing Only `NA` Values
#'
#' @rdname removeNA
#' @name removeNA
#' @family Cleanup Utilities
#'
#' @inheritParams AllGenerics
#'
#' @return Sanitized data.
#'
#' @examples
#' # Remove NA only rows and columns
#' matrix(c(1, NA, 3,
#'          NA, NA, NA,
#'          2, NA, 4),
#'        nrow = 3, ncol = 3) %>%
#'     removeNA()
#'
#' data.frame(a = c("A", NA, "C"),
#'            b = c(NA, NA, NA),
#'            c = c("B", NA, "D")) %>%
#'     removeNA()
#'
#' tibble(a = c("A", NA, "C"),
#'        b = c(NA, NA, NA),
#'        c = c("B", NA, "D")) %>%
#'     removeNA()
#'
#' # Support for vectors
#' removeNA(c("hello", "world", NA))
#' removeNA(c(1, 2, NA))
NULL



# Constructors =================================================================
#' @importFrom stats na.omit
.removeNAVec <- function(object) {
    na.omit(object)
}

.removeNADim <- function(object) {
    object %>%
        # Remove all `NA` rows
        .[apply(., 1L, function(a) !all(is.na(a))), ] %>%
        # Remove all `NA` columns
        .[, apply(., 2L, function(a) !all(is.na(a)))]
}



# Methods ======================================================================
#' @rdname removeNA
#' @export
setMethod(
    "removeNA",
    signature("ANY"),
    function(object) {
        # Return unmodified by default
        object
    })



#' @rdname removeNA
#' @export
setMethod(
    "removeNA",
    signature("character"),
    .removeNAVec)



#' @rdname removeNA
#' @export
setMethod(
    "removeNA",
    signature("numeric"),
    .removeNAVec)



#' @rdname removeNA
#' @export
setMethod(
    "removeNA",
    signature("matrix"),
    .removeNADim)



#' @rdname removeNA
#' @export
setMethod(
    "removeNA",
    signature("data.frame"),
    .removeNADim)



#' @rdname removeNA
#' @export
setMethod(
    "removeNA",
    signature("DataFrame"),
    .removeNADim)



#' @rdname removeNA
#' @export
setMethod(
    "removeNA",
    signature("tbl_df"),
    .removeNADim)
