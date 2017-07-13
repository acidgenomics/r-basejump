#' [tidyverse](http://tidyverse.org/) S4 methods
#'
#' Avoid NAMESPACE collisions with [Bioconductor](https://www.bioconductor.org/)
#' generics by using [setMethod()] to relevant `signature`.
#'
#' @rdname tidy
#' @docType methods
#'
#' @param object Primary object.
#' @param from Object to coerce.
#' @param x Primary object.
#' @param y Secondary object.
#' @param ... Additional parameters.
#'
#' @seealso
#' - [Bioconductor](https://www.bioconductor.org/):
#'     [Biobase], [BiocGenerics], S4Vectors.
#' - [tidyverse](http://tidyverse.org/):
#'     [dplyr], [httr], [tidyr].



# arrange ====
#' @rdname tidy
#' @export
setMethod(
    "arrange",
    signature(object = "data.frame"),
    function(object, ...) {
        dplyr::arrange(object, ...)
    })



# as_tibble ====
#' @rdname tidy
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

#' @rdname tidy
#' @export
setMethod("as_tibble", "data.frame", .asTibble)

#' @rdname tidy
#' @export
setMethod("as_tibble", "DataFrame", .asTibble)

#' @rdname tidy
#' @export
setMethod("as_tibble", "matrix", .asTibble)



# content ====
#' @rdname tidy
#' @export
setMethod("content", "response", function(object) {
    httr::content(object)
})



# expand ====
#' @rdname tidy
#' @export
setMethod(
    "expand",
    signature(x = "data.frame"),
    function(x, ...) {
        tidyr::expand(x, ...)
    })



# filter ====
#' @rdname tidy
#' @export
setMethod(
    "filter",
    signature(object = "data.frame"),
    function(object, ...) {
        dplyr::filter(object, ...)
    })



# first ====
#' @rdname tidy
#' @export
setMethod(
    "first",
    signature(x = "data.frame"),
    function(x, ...) {
        dplyr::first(x, ...)
    })



# intersect ====
#' @rdname tidy
#' @export
setMethod(
    "intersect",
    signature(x = "data.frame", y = "data.frame"),
    function(x, y, ...) {
        dplyr::intersect(x, y, ...)
    })



# mutate ====
#' @rdname tidy
#' @export
setMethod(
    "mutate",
    signature(object = "data.frame"),
    function(object, ...) {
        dplyr::mutate(object, ...)
    })



# rename ====
#' @rdname tidy
#' @export
setMethod(
    "rename",
    signature(x = "data.frame"),
    function(x, ...) {
        dplyr::rename(x, ...)
    })



# select ====
#' @rdname tidy
#' @usage NULL
#' @export
setMethod("select",
          signature(x = "data.frame"),
          function(x) {
              stop("Use [tidy_select()] on data.frame.
         NAMESPACE collison with [AnnotationDbi::select()].")
          })



# setdiff ====
#' @rdname tidy
#' @export
setMethod(
    "setdiff",
    signature(x = "data.frame", y = "data.frame"),
    function(x, y, ...) {
        dplyr::setdiff(x, y, ...)
    })



# setequal ====
#' @rdname tidy
#' @export
setMethod(
    "setequal",
    signature(x = "data.frame", y = "data.frame"),
    function(x, y) {
        dplyr::setequal(x, y)
    })



# union ====
#' @rdname tidy
#' @export
setMethod(
    "union",
    signature(x = "data.frame", y = "data.frame"),
    function(x, y, ...) {
        dplyr::union(x, y, ...)
    })
