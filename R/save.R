#' Save data objects
#'
#' Quick save to `data` (default) or `data-raw` directory.
#'
#' @rdname save
#'
#' @param ... Objects.
#' @param dir Save directory.
#' @param compress Compression method, supporting `xz` (preferred), `bzip2`, or
#'   `gzip`. Compression can be disabled by setting as `FALSE`, although this is
#'   not generally recommended.
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
    objs <- get_objs_from_dots(dots(...))
    paths <- file.path(dir, paste0(objs, ".rda"))
    paste("Saving", toString(objs), "to", dir) %>%
        paste0("...") %>%
        message
    mapply(save, list = objs, file = paths, compress = compress)
    invisible()
}

#' @rdname snake_aliases
#' @usage NULL
#' @export
save_data <- saveData



#' @rdname save
#' @export
saveDataRaw <- function(...) {
    saveData(..., dir = "data-raw")
}

#' @rdname snake_aliases
#' @usage NULL
#' @export
save_data_raw <- saveDataRaw
