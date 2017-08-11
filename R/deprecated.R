# nolint start



#' Deprecated Functions
#'
#' @rdname deprecated
#' @name deprecated
#' @inherit AllGenerics
#' @keywords internal
#'
#' @return Varies by function.
#'
#' @examples
#' \dontrun{
#' # snake_case variants used in RMarkdown
#' save_data()
#' write_counts()
#'
#' # 0.0.19
#' dotNotation()
#' grepToString()
#' loadDataRaw()
#' toStringSummarize()
#'
#' # 0.0.20
#' assignAsNewEnv()
#' getObjsFromDots()
#' readDataRaw()
#' sanitizeNames()
#' saveDataRaw()
#' wash()
#' }
NULL



# snake_case variants used in RMarkdown ====
#' @rdname deprecated
#' @export
save_data <- function(...) {
    .Deprecated("saveData")
}

#' @rdname deprecated
#' @export
write_counts <- function(...) {
    .Deprecated("writeCounts")
}



# 0.0.19 ====
#' @rdname deprecated
#' @export
dotNotation <- function(...) {
    .Deprecated("dotted")
}

#' @rdname deprecated
#' @export
grepToString <- function(...) {
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



# 0.0.20 ====
#' @rdname deprecated
#' @export
assignAsNewEnv <- function(...) {
    .Deprecated("assignIntoNewEnv")
}

#' @rdname deprecated
#' @export
getObjsFromDots <- function(...) {
    .Deprecated("dots")
}

#' @rdname deprecated
#' @export
readDataRaw <- function(...) {
    .Deprecated()
}

#' @rdname deprecated
#' @export
sanitizeNames <- function(...) {
    .Deprecated("snake")
}

#' @rdname deprecated
#' @export
saveDataRaw <- function(...) {
    .Deprecated()
}

#' @rdname deprecated
#' @export
wash <- function(...) {
    .Deprecated()
}



# nolint end
