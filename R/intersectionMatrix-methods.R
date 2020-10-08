#' @name intersectionMatrix
#' @inherit AcidGenerics::intersectionMatrix
#' @note Updated 2020-08-18.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' object <- list(
#'     a = c("a", "b", "c", "d", "e", "f"),
#'     b = c("b", "c", "d", "e", "f", "g"),
#'     c = c("c", "d", "e", "f", "g", "h")
#' )
#' mat <- intersectionMatrix(object)
#' rowSums(mat)
NULL



## Updated 2020-08-18.
`intersectionMatrix,list` <-  # nolint
    function(object) {
        assert(length(object) > 1L)
        rownames <- sort(unique(unlist(object)))
        mat <- vapply(
            X = object,
            FUN = function(x) {
                rownames %in% x
            },
            FUN.VALUE = logical(length(rownames)),
            USE.NAMES = TRUE
        )
        rownames(mat) <- rownames
        mat
    }



#' @rdname intersectionMatrix
setMethod(
    f = "intersectionMatrix",
    signature = signature("list"),
    definition = `intersectionMatrix,list`
)
