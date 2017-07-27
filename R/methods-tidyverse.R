#' [tidyverse](http://tidyverse.org/) S4 Methods
#'
#' Avoid `NAMESPACE` collisions with
#' [Bioconductor](https://www.bioconductor.org/) generics by using [setMethod()]
#' to relevant `signature`.
#'
#' @rdname tidyverse
#'
#' @param .data Data object.
#'
#' @seealso
#' - [Bioconductor](https://www.bioconductor.org/):
#'     [Biobase], [BiocGenerics], S4Vectors.
#' - [tidyverse](http://tidyverse.org/):
#'     [dplyr], [httr], [tidyr].



# arrange ====
#' @rdname tidyverse
#' @export
setMethod(
    "arrange",
    signature(object = "data.frame"),
    function(object, ...) {
        dplyr::arrange(object, ...)
    })



# as_tibble ====
#' @rdname tidyverse
#' @usage NULL
#' @note For tibble coercion with [as()] or [as_tibble()], rownames are always
#'   moved to the `rowname` column, using [rownames_to_column()] internally.
#'   This provides more consistent behavior in the tidyverse, which can
#'   inadvertently strip rownames during filtering operations.
.asTibble <- function(from) {
    from <- as.data.frame(from)
    if (has_rownames(from)) {
        from <- rownames_to_column(from)
    }
    tibble::as_tibble(from)
}

setAs("data.frame", "tibble", .asTibble)
setAs("DataFrame", "tibble", .asTibble)
setAs("matrix", "tibble", .asTibble)

#' @rdname tidyverse
#' @export
setMethod("as_tibble", "data.frame", .asTibble)

#' @rdname tidyverse
#' @export
setMethod("as_tibble", "DataFrame", .asTibble)

#' @rdname tidyverse
#' @export
setMethod("as_tibble", "matrix", .asTibble)



# content ====
#' @rdname tidyverse
#' @export
setMethod("content", "response", function(object) {
    httr::content(object)
})



# expand ====
#' @rdname tidyverse
#' @export
setMethod(
    "expand",
    signature(x = "data.frame"),
    function(x, ...) {
        tidyr::expand(x, ...)
    })



# filter ====
#' @rdname tidyverse
#' @export
setMethod(
    "filter",
    signature(object = "data.frame"),
    function(object, ...) {
        dplyr::filter(object, ...)
    })



# first ====
#' @rdname tidyverse
#' @export
setMethod(
    "first",
    signature(x = "data.frame"),
    function(x, ...) {
        dplyr::first(x, ...)
    })



# intersect ====
#' @rdname tidyverse
#' @export
setMethod(
    "intersect",
    signature(x = "data.frame", y = "data.frame"),
    function(x, y, ...) {
        dplyr::intersect(x, y, ...)
    })



# mutate ====
#' @rdname tidyverse
#' @export
setMethod(
    "mutate",
    signature(object = "data.frame"),
    function(object, ...) {
        dplyr::mutate(object, ...)
    })



# rename ====
#' @rdname tidyverse
#' @export
setMethod(
    "rename",
    signature(x = "data.frame"),
    function(x, ...) {
        dplyr::rename(x, ...)
    })



# setdiff ====
#' @rdname tidyverse
#' @export
setMethod(
    "setdiff",
    signature(x = "data.frame", y = "data.frame"),
    function(x, y, ...) {
        dplyr::setdiff(x, y, ...)
    })



# setequal ====
#' @rdname tidyverse
#' @export
setMethod(
    "setequal",
    signature(x = "data.frame", y = "data.frame"),
    function(x, y) {
        dplyr::setequal(x, y)
    })



# union ====
#' @rdname tidyverse
#' @export
setMethod(
    "union",
    signature(x = "data.frame", y = "data.frame"),
    function(x, y, ...) {
        dplyr::union(x, y, ...)
    })
