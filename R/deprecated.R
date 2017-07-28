#' Deprecated Functions
#'
#' @rdname deprecated
#' @keywords internal
#'
#' @param ... Additional parameters.



#' @rdname deprecated
#' @export
createNewProject <- function(...) {
    .Deprecated("createProjectDirs")
    createProjectDirs(...)
}



#' @rdname deprecated
#' @export
dotNotation <- function(...) {
    .Deprecated("dotted")
    dotted(...)
}



#' @rdname deprecated
#' @export
get_objs_from_dots <- function(...) {  # nolint
    .Deprecated("getObjsFromDots")
    getObjsFromDots(...)
}



#' @rdname deprecated
#' @export
grepToString <- function() {
    .Deprecated("grepString")
}



#' @rdname deprecated
#' @export
loadDataRaw <- function(...) {
    .Deprecated("loadData")
}



#' @rdname deprecated
#' @export
toStringSummarize <- function(...) {
    .Deprecated("collapse")
}
