#' @include basejump-package.R



# tibble
setOldClass(Classes = class(tibble()))



#' @rdname gene2symbol
#' @aliases NULL
#' @exportClass gene2symbol
#' @usage NULL
gene2symbol <- setClass(
    Class = "gene2symbol",
    contains = "DataFrame"
)

# TODO Add working example
# new("gene2symbol", g2s)
