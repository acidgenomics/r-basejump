#' Remove Rows and Columns Containing Only `NA` Values
#'
#' @name removeNA
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
#' from <- matrix(
#'     data = c(1, NA, 3, NA, NA, NA, 2, NA, 4),
#'     nrow = 3,
#'     ncol = 3
#' )
#' print(from)
#' to <- removeNA(from)
#' print(to)
#'
#' # DataFrame ====
#' from <- DataFrame(
#'     a = c("A", NA, "C"),
#'     b = c(NA, NA, NA),
#'     c = c("B", NA, "D")
#' )
#' print(from)
#' to <- removeNA(from)
#' print(to)
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
    definition = function(object) {
        object %>%
            # Drop rows that are all `NA`.
            .[apply(., 1L, function(a) !all(is.na(a))), , drop = FALSE] %>%
            # Drop columns that are all `NA`.
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
