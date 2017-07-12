#' Save data objects
#'
#' Quickly save objects in a project.
#'
#' @rdname save
#'
#' @param ... Dot objects.
#' @param dir Output directory. Defaults to **data**.
#' @param compress Compression method, supporting `xz` (**preferred**), `bzip2`,
#'   or `gzip`. Compression can be disabled by setting as `FALSE`, although this
#'   is not generally recommended.
#'
#' @export
#' @seealso [base::save()].



#' @rdname save
#' @export
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
#' @description [saveDataRaw()] is a wrapper for [saveData()] that enables quick
#'   saving of objects to the `data-raw` directory.
#' @export
saveDataRaw <- function(...) {
    saveData(..., dir = "data-raw")
}



#' @rdname save
#' @description [assignAndSaveData()] assigns a new object by name to the
#'   current working environment then saves the newly assigned object, specified
#'   by `dir`.
#'
#' @param name Desired variable name.
#' @param object Object.
#'
#' @export
assignAndSaveData <- function(name, object, dir = "data", compress = "xz") {
    envir <- parent.frame()
    assign(name, object, envir = envir)
    save(list = name,
         file = file.path(dir, str_c(name, ".rda")),
         envir = envir,
         compress = compress)
}
