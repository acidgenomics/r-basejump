#' Save data objects.
#'
#' Quick save to \code{data} directory.
#'
#' @param ... Data objects.
#' @param dir Output directory.
#'
#' @export
saveData <- function(..., dir = "data") {
    if (!isString(dir)) {
        stop("dir must be a string")
    }
    if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    }
    names <- as.character(substitute(list(...)))[-1L]
    objs <- get_objs_from_dots(dots(...))
    paths <- file.path(dir, paste0(objs, ".rda"))
    message(paste("Saving", toString(names), "to", dir))
    mapply(save, list = objs, file = paths)
    invisible()
}



#' @rdname saveData
#' @usage NULL
#' @export
save_data <- saveData



#' @rdname saveData
#' @description Quick save to \code{data-raw} directory.
#' @export
saveDataRaw <- function(...) {
    saveData(..., dir = "data-raw")
}



#' @rdname saveData
#' @usage NULL
#' @export
save_data_raw <- saveDataRaw
