#' Save Data
#'
#' Wrapper for [base::save()] supporting quick saving of object names passed as
#' symbols. This function saves each object into a separate `.rda` file rather
#' than combining into a single file.
#'
#' @family Save Utilities
#'
#' @param ... Object names as symbols.
#' @param dir Output directory. Defaults to **data**.
#' @param compress Compression method, supporting `xz` (**preferred**), `bzip2`,
#'   or `gzip`. Compression can be disabled by setting as `FALSE`, although this
#'   is not generally recommended.
#'
#' @note These function will *overwrite* existing saved data, following the
#'   same conventions as [base::save()]. Conversely, [devtools::use_data()] does
#'   not overwrite by default if that behavior is preferred.
#'
#' @seealso
#' - [base::save()]
#' - [devtools::use_data()].
#'
#' @return No value.
#' @export
saveData <- function(..., dir = "data", compress = TRUE) {
    if (!is_string(dir)) stop("dir must be a string")
    if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    }
    objs <- getObjsFromDots(dots(...))
    paths <- file.path(dir, paste0(objs, ".rda"))
    message(paste("Saving", toString(objs), "to", dir))
    mapply(save, list = objs, file = paths, compress = compress)
    invisible()
}



#' Save Raw Data
#'
#' Wrapper for [saveData()] that enables quick saving of objects to the
#' `data-raw/` directory.
#'
#' @family Save Utilities
#' @inherit saveData
#'
#' @seealso [devtools::use_data_raw()].
#'
#' @export
saveDataRaw <- function(...) {
    saveData(..., dir = "data-raw")
}
