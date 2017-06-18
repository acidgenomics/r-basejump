#' Renamed functions
#'
#' @rdname renamed
#' @name renamed
NULL



#' @rdname renamed
#' @usage NULL
#' @export
createNewProject <- function(...) {
    .Deprecated("createProjectDirs")
    createProjectDirs(...)
}

#' @rdname snake_aliases
#' @usage NULL
#' @export
create_new_project <- createNewProject



#' @rdname renamed
#' @usage NULL
#' @export
dotNotation <- function(...) {
    .Deprecated("dotted")
    dotted(...)
}

#' @rdname snake_aliases
#' @export
dot_notation <- dotNotation



#' @rdname renamed
#' @usage NULL
#' @export
grepToString <- function() {
    .Deprecated("grepString")
}

#' @rdname renamed
#' @usage NULL
#' @export
grep_to_string <- grepToString



#' @rdname renamed
#' @usage NULL
#' @export
loadDataRaw <- function(...) {
    .Deprecated("loadData")
}

#' @rdname snake_aliases
#' @usage NULL
#' @export
load_data_raw <- loadDataRaw



#' @rdname renamed
#' @usage NULL
#' @export
toStringSummarize <- function(...) {
    .Deprecated("summarizeRows")
    summarizeRows(...)
}

#' @rdname snake_aliases
#' @usage NULL
to_string_summarize <- toStringSummarize

#' @rdname british_aliases
#' @usage NULL
toStringSummarise <- toStringSummarize

#' @rdname british_aliases
#' @usage NULL
to_string_summarise <- toStringSummarize
