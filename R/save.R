#' Save data objects
#'
#' Quick save to `data` (default) or `data-raw` directory.
#'
#' @rdname save
#'
#' @param ... Objects.
#' @param dir Save directory.
#' @param compress Compression method, supporting `xz` (**preferred**), `bzip2`,
#'   or `gzip`. Compression can be disabled by setting as `FALSE`, although this
#'   is not generally recommended.
#'
#' @export
#' @seealso [base::save()].
saveData <- function(..., dir = "data", compress = "xz") {
    if (!is_string(dir)) {
        stop("dir must be a string")
    }
    if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    }
    objs <- getObjsFromDots(dots(...))
    paths <- file.path(dir, str_c(objs, ".rda"))
    message(paste("Saving", toString(objs), "to", dir))
    mapply(save, list = objs, file = paths, compress = compress)
    invisible()
}



#' @rdname save
#' @export
saveDataRaw <- function(...) {
    saveData(..., dir = "data-raw")
}
