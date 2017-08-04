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
dotNotation <- function() {
    .Deprecated("dotted")
}

#' @rdname deprecated
#' @export
grepToString <- function() {
    .Deprecated("grepString")
}

#' @rdname deprecated
#' @export
loadDataRaw <- function() {
    .Deprecated("loadData")
}

#' @rdname deprecated
#' @export
toStringSummarize <- function() {
    .Deprecated("collapse")
}



# v0.0.20 ====
#' @rdname deprecated
#' @export
assignAsNewEnv <- function() {
    .Deprecated("assignIntoNewEnv")
}

#' @rdname deprecated
#' @export
getObjsFromDots <- function() {
    .Deprecated("dots")
}

#' @rdname deprecated
#' @export
sanitizeNames <- function() {
    .Deprecated("snake")
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
