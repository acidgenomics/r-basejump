#' Deprecated functions
#'
#' @rdname deprecated
#' @name deprecated
#' @keywords internal
#'
#' @param ... Additional parameters.
NULL



#' @rdname deprecated
#' @export
createNewProject <- function(...) {
    .Deprecated("createProjectDirs")
    createProjectDirs(...)
}

#' @rdname deprecated
#' @usage NULL
#' @export
createNewProject -> create_new_project  # nolint



#' @rdname deprecated
#' @export
dotNotation <- function(...) {
    .Deprecated("dotted")
    dotted(...)
}

#' @rdname deprecated
#' @usage NULL
#' @export
dotNotation -> dot_notation  # nolint



#' @rdname deprecated
#' @export
grepToString <- function() {
    .Deprecated("grepString")
}

#' @rdname deprecated
#' @usage NULL
#' @export
grepToString -> grep_to_string  # nolint



#' @rdname deprecated
#' @export
loadDataRaw <- function(...) {
    .Deprecated("loadData")
}

#' @rdname deprecated
#' @usage NULL
#' @export
loadDataRaw -> load_data_raw  # nolint



#' @rdname deprecated
#' @export
toStringSummarize <- function(...) {
    .Deprecated("summarizeRows")
    summarizeRows(...)
}

#' @rdname deprecated
#' @usage NULL
toStringSummarize -> to_string_summarize  # nolint

#' @rdname deprecated
#' @usage NULL
toStringSummarize -> toStringSummarise

#' @rdname deprecated
#' @usage NULL
toStringSummarize -> to_string_summarise  # nolint
