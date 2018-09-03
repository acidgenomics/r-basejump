#' Remove Rows and Columns Containing Only `NA` Values
#'
#' @name removeNA
#' @family Sanitization Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#'
#' @return Sanitized object.
#'
#' @examples
#' # atomic ====
#' removeNA(c("hello", "world", NA))
#' removeNA(c(1, 2, NA))
#'
#' # matrix ====
#' object <- matrix(
#'     data = c(1, NA, 3, NA, NA, NA, 2, NA, 4),
#'     nrow = 3,
#'     ncol = 3
#' )
#' print(object)
#' x <- removeNA(object)
#' print(x)
#'
#' # DataFrame ====
#' object <- DataFrame(
#'     a = c("A", NA, "C"),
#'     b = c(NA, NA, NA),
#'     c = c("B", NA, "D")
#' )
#' print(object)
#' x <- removeNA(object)
#' print(x)
NULL



#' @rdname removeNA
#' @export
setMethod(
    f = "removeNA",
    signature = signature("atomic"),
    definition = function(object) {
        na.omit(object)
    }
)



#' @rdname removeNA
#' @export
setMethod(
    f = "removeNA",
    signature = signature("matrix"),
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
    f = "removeNA",
    signature = signature("sparseMatrix"),
    definition = getMethod("removeNA", "matrix")
)



#' @rdname removeNA
#' @export
setMethod(
    f = "removeNA",
    signature = signature("data.frame"),
    definition = getMethod("removeNA", "matrix")
)



#' @rdname removeNA
#' @export
setMethod(
    f = "removeNA",
    signature = signature("DataFrame"),
    definition = getMethod("removeNA", "matrix")
)
