# nolint begin

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
get_objs_from_dots <- function(...) {
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
save_data <- function(...) {
    .Deprecated("saveData")
    saveData(...)
}



#' @rdname deprecated
#' @export
toStringSummarize <- function(...) {
    .Deprecated("collapse")
    collapse(...)
}



#' @rdname deprecated
#' @export
write_counts <- function(...) {
    .Deprecated("writeCounts")
    writeCounts(...)
}

# nolint end
