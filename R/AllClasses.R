#' S4 Classes
#'
#' @name AllClasses
#' @keywords internal
#' @include basejump-package.R
#'
#' @inheritParams general
#'
#' @return Varies, depending upon the method.
NULL



# tibble =======================================================================
setOldClass(Classes = class(tibble()))



# gene2symbol ==================================================================
#' @rdname AllClasses
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



# tx2gene ==================================================================
#' @rdname AllClasses
#' @aliases NULL
#' @exportClass tx2gene
#' @usage NULL
tx2gene <- setClass(
    Class = "tx2gene",
    contains = "DataFrame"
)

setValidity(
    Class = "tx2gene",
    method = function(object) {
        assertIsTx2gene(object)
        TRUE
    }
)
