# arrange ====
#' @rdname tidy
#' @export
setMethod("arrange", "data.frame", function(object, ...) {
    dplyr::arrange(object, ...)
})



# content ====
#' @rdname tidy
#' @export
setMethod("content", "response", function(object) {
    httr::content(object)
})



# expand ====
#' @rdname tidy
#' @export
setMethod("expand", "data.frame", function(x, ...) {
    tidyr::expand(x, ...)
})



# filter ====
#' @rdname tidy
#' @export
setMethod("filter", "data.frame", function(object, ...) {
    dplyr::filter(object, ...)
})



# first ====
#' @rdname tidy
#' @export
setMethod("first", "data.frame", function(x, ...) {
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



# filter ====
#' @rdname tidy
#' @export
setMethod("mutate", "data.frame", function(object, ...) {
    dplyr::mutate(object, ...)
})



# rename ====
#' @rdname tidy
#' @export
setMethod("rename", "data.frame", function(x, ...) {
    dplyr::rename(x, ...)
})



# select ====
#' @rdname tidy
#' @usage NULL
#' @export
setMethod("select", "data.frame", function(x) {
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
