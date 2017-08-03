# nolint start



#' Deprecated Functions
#'
#' @rdname deprecated
#' @name deprecated
#' @inherit AllGenerics
#' @keywords internal
#'
#' @return Varies by function.
NULL



# v0.0.19 ====
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
grepToString <- function(...) {
    .Deprecated("grepString")
    grepString(...)
}

#' @rdname deprecated
#' @export
loadDataRaw <- function(...) {
    .Deprecated("loadData")
    loadData(...)
}

#' @rdname deprecated
#' @export
toStringSummarize <- function(...) {
    .Deprecated("collapse")
    collapse(...)
}



# v0.0.20 ====
#' @rdname deprecated
#' @export
assignAsNewEnv <- function() {
    .Deprecated("assignIntoNewEnv")
}

#' @rdname deprecated
#' @export
getObjsFromDots <- function(...) {
    .Deprecated("dots")
    dots(..., character = TRUE)
}

#' @rdname deprecated
#' @usage NULL
#' @export
getObjsFromDots -> get_objs_from_dots

#' @rdname deprecated
#' @export
sanitizeNames <- function(...) {
    .Deprecated("snake")
    snake(...)
}

#' @rdname deprecated
#' @export
save_data <- function(...) {
    .Deprecated("saveData")
    saveData(...)
}

#' @rdname deprecated
#' @export
write_counts <- function(...) {
    .Deprecated("writeCounts")
    writeCounts(...)
}



# nolint end
