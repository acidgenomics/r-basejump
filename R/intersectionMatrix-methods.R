#' @name intersectionMatrix
#' @inherit AcidGenerics::intersectionMatrix
#' @note Updated 2021-01-15.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' object <- list(
#'     "aaa" = c("a", "b", "c", "d", "e", "f"),
#'     "bbb" = c("b", "c", "d", "e", "f", "g"),
#'     "ccc" = c("c", "d", "e", "f", "g", "h")
#' )
#' mat <- intersectionMatrix(object)
#' rowSums(mat)
NULL



## Updated 2020-08-18.
`intersectionMatrix,list` <-  # nolint
    function(object) {
        assert(length(object) > 1L)
        elements <- sort(unique(unlist(object)))
        mat <- vapply(
            X = object,
            FUN = function(x, elements) {
                elements %in% x
            },
            elements = elements,
            FUN.VALUE = logical(length(elements)),
            USE.NAMES = TRUE
        )
        rownames(mat) <- elements
        mat
    }



#' @rdname intersectionMatrix
setMethod(
    f = "intersectionMatrix",
    signature = signature("list"),
    definition = `intersectionMatrix,list`
)
