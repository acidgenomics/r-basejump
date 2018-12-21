#' @name removeNA
#' @inherit bioverbs::removeNA
#' @inheritParams params
#' @examples
#' ## atomic ====
#' removeNA(c("hello", "world", NA))
#' removeNA(c(1, 2, NA))
#'
#' ## matrix ====
#' from <- matrix(
#'     data = c(1, NA, 3, NA, NA, NA, 2, NA, 4),
#'     nrow = 3,
#'     ncol = 3
#' )
#' print(from)
#' to <- removeNA(from)
#' print(to)
#'
#' ## DataFrame ====
#' from <- S4Vectors::DataFrame(
#'     a = c("A", NA, "C"),
#'     b = c(NA, NA, NA),
#'     c = c("B", NA, "D")
#' )
#' print(from)
#' to <- removeNA(from)
#' print(to)
NULL



#' @importFrom bioverbs removeNA
#' @aliases NULL
#' @export
bioverbs::removeNA



removeNA.atomic <-  # nolint
    function(object) {
        na.omit(object)
    }



#' @rdname removeNA
#' @export
setMethod(
    f = "removeNA",
    signature = signature("atomic"),
    definition = removeNA.atomic
)



removeNA.matrix <-  # nolint
    function(object) {
        object %>%
            # Drop rows that are all `NA`.
            .[apply(., 1L, function(a) !all(is.na(a))), , drop = FALSE] %>%
            # Drop columns that are all `NA`.
            .[, apply(., 2L, function(a) !all(is.na(a))), drop = FALSE]
    }



#' @rdname removeNA
#' @export
setMethod(
    f = "removeNA",
    signature = signature("matrix"),
    definition = removeNA.matrix
)



#' @rdname removeNA
#' @export
setMethod(
    f = "removeNA",
    signature = signature("sparseMatrix"),
    definition = removeNA.matrix
)



#' @rdname removeNA
#' @export
setMethod(
    f = "removeNA",
    signature = signature("data.frame"),
    definition = removeNA.matrix
)



#' @rdname removeNA
#' @export
setMethod(
    f = "removeNA",
    signature = signature("DataFrame"),
    definition = removeNA.matrix
)
