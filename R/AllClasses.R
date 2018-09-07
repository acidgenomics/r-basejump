#' @include basejump-package.R



# tibble
setOldClass(Classes = class(tibble()))



# gene2symbol ==================================================================
#' @rdname gene2symbol
#' @aliases NULL
#' @exportClass gene2symbol
#' @usage NULL
gene2symbol <- setClass(
    Class = "gene2symbol",
    contains = "DataFrame"
)

setValidity(
    Class = "gene2symbol",
    method = function(object) {
        assertIsGene2symbol(object)
        TRUE
    }
)
