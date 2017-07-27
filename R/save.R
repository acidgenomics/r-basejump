#' Save Data Objects
#'
#' Quickly save objects in a project.
#'
#' @rdname save
#'
#' @param ... Object names as symbols.
#' @param dir Output directory. Defaults to **data**.
#' @param compress Compression method, supporting `xz` (**preferred**), `bzip2`,
#'   or `gzip`. Compression can be disabled by setting as `FALSE`, although this
#'   is not generally recommended.
#'
#' @note These functions will *overwrite* existing saved data, following the
#'   same conventions as [base::save()]. Conversely, [devtools::use_data()] does
#'   not overwrite by default if that behavior is preferred.
#'
#' @seealso
#' - [base::save()].
#' - [devtools::use_data()].
#' - [devtools::use_data_raw()].



#' @rdname save
#' @details
#' - [saveData()]: Wrapper for [base::save()] supporting quick saving of object
#'   names passed as symbols. This function saves each object into a separate
#'   `.rda` file rather than combining into a single file.
#' @export
saveData <- function(..., dir = "data", compress = TRUE) {
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

# Snake variant for RMarkdown templates
#' @rdname save
#' @usage NULL
#' @export
saveData -> save_data  # nolint



#' @rdname save
#' @details
#' - [saveDataRaw()]: Wrapper for [saveData()] that enables quick saving of
#'   objects to the `data-raw` directory.
#' @export
saveDataRaw <- function(...) {
    saveData(..., dir = "data-raw")
}



#' @rdname save
#'
#' @param name Desired variable name.
#' @param object Object.
#'
#' @details
#' - [assignAndSaveData()]: Assigns a new object by name to the
#'   current working environment then saves the newly assigned object, specified
#'   by `dir`.
#'
#' @export
assignAndSaveData <- function(name, object, dir = "data", compress = TRUE) {
    envir <- parent.frame()
    assign(name, object, envir = envir)
    save(list = name,
         file = file.path(dir, str_c(name, ".rda")),
         envir = envir,
         compress = compress)
}
