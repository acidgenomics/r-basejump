#' Save data objects.
#'
#' Quick save to `data` (default) or `data-raw` directory.
#'
#' @param ... Objects.
#' @param dir Save directory.
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
#' @export
save_data <- saveData



#' @rdname saveData
#' @export
saveDataRaw <- function(...) {
    saveData(..., dir = "data-raw")
}



#' @rdname saveData
#' @export
save_data_raw <- saveDataRaw
