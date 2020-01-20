#' Select multiple columns
#'
#' @name select
#' @note Updated 2019-08-26.
#'
#' @inheritParams acidroxygen::params
#' @inheritParams mutate
#' @param ... Additional arguments.
#'
#' @return Modified object.
#'
#' @seealso
#' These functions are inspired by dplyr. However, they are designed to only
#' work on `DataFrame` class, and use base R code internally.
#'
#' ```r
#' `help(topic = "select_all", package = "dplyr")`
#' ```
#'
#' @examples
#' data(iris, package = "datasets")
#'
#' ## DataFrame ====
#' x <- as(iris, "DataFrame")
#' selectIf(x, predicate = is.factor)
NULL



#' @rdname select
#' @name selectIf
#' @importFrom acidgenerics selectIf
#' @usage selectIf(object, predicate, ...)
#' @export
NULL



`selectIf,DataFrame` <-  # nolint
    function(object, predicate) {
        keep <- bapply(X = object, FUN = predicate)
        object[, keep, drop = FALSE]
    }



#' @rdname select
#' @export
setMethod(
    f = "selectIf",
    signature = signature(
        object = "DataFrame",
        predicate = "function"
    ),
    definition = `selectIf,DataFrame`
)
