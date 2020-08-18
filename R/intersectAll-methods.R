#' @name intersectAll
#' @inherit acidgenerics::intersectAll
#' @note Updated 2020-08-18.
#'
#' @inheritParams acidroxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' object <- list(
#'     a = c("a", "b", "c", "d", "e", "f"),
#'     b = c("b", "c", "d", "e", "f", "g"),
#'     c = c("c", "d", "e", "f", "g", "h")
#' )
#' intersectAll(object)
NULL



#' @rdname intersectAll
#' @name intersectAll
#' @importFrom acidgenerics intersectAll
#' @usage intersectAll(object, ...)
#' @export
NULL



## Updated 2020-08-18.
`intersectAll,list` <-  # nolint
    function(object) {
        Reduce(f = intersect, x = object)
    }



#' @rdname intersectAll
setMethod(
    f = "intersectAll",
    signature = signature("list"),
    definition = `intersectAll,list`
)
