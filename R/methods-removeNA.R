#' Remove Rows and Columns Containing Only `NA` Values
#'
#' @name removeNA
#' @family Sanitization Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#'
#' @return Sanitized object.
#'
#' @examples
#' # matrix ====
#' x <- matrix(
#'     data = c(1, NA, 3, NA, NA, NA, 2, NA, 4),
#'     nrow = 3,
#'     ncol = 3
#' )
#' print(x)
#' removeNA(x)
#'
#' # data.frame ====
#' x <- data.frame(
#'     a = c("A", NA, "C"),
#'     b = c(NA, NA, NA),
#'     c = c("B", NA, "D"),
#'     stringsAsFactors = FALSE
#' )
#' print(x)
#' removeNA(x)
#'
#' # atomic ====
#' removeNA(c("hello", "world", NA))
#' removeNA(c(1, 2, NA))
NULL



# Methods ======================================================================
#' @rdname removeNA
#' @export
setMethod(
    "removeNA",
    signature("ANY"),
    function(object) {
        object
    }
)



#' @rdname removeNA
#' @export
setMethod(
    "removeNA",
    signature("atomic"),
    function(object) {
        na.omit(object)
    }
)



#' @rdname removeNA
#' @export
setMethod(
    "removeNA",
    signature("matrix"),
    function(object) {
        object %>%
            # Drop rows that are all `NA`
            .[apply(., 1L, function(a) !all(is.na(a))), , drop = FALSE] %>%
            # Drop columns that are all `NA`
            .[, apply(., 2L, function(a) !all(is.na(a))), drop = FALSE]
    }
)



#' @rdname removeNA
#' @export
setMethod(
    "removeNA",
    signature("data.frame"),
    getMethod("removeNA", "matrix")
)



#' @rdname removeNA
#' @export
setMethod(
    "removeNA",
    signature("DataFrame"),
    getMethod("removeNA", "matrix")
)
