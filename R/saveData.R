#' Save Data
#'
#' Wrapper for [base::save()] supporting quick saving of object names passed as
#' symbols. This function saves each object into a separate `.rda` file rather
#' than combining into a single file.
#'
#' @family Write Utilities
#'
#' @importFrom rlang is_string
#'
#' @inheritParams loadData
#' @inheritParams base::save
#'
#' @param overwrite Overwrite existing file.
#'
#' @note These function will *overwrite* existing saved data, following the
#'   same conventions as [base::save()]. Conversely, [devtools::use_data()] does
#'   not overwrite by default if that behavior is preferred.
#'
#' @seealso
#' - [base::save()].
#' - [usethis::use_data()].
#'
#' @return Silent named character vector of file paths.
#' @export
#'
#' @examples
#' saveData(mtcars, starwars)
saveData <- function(
    ...,
    dir = getwd(),
    ext = "rda",
    overwrite = TRUE,
    compress = "bzip2",
    quiet = FALSE) {
    if (!is_string(dir)) {
        stop("'dir' must be a string", call. = FALSE)
    }
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    objectNames <- dots(..., character = TRUE)
    files <- file.path(dir, paste0(objectNames, ".", ext))
    names(files) <- objectNames
    # If overwrite = FALSE, message skipped files
    if (identical(overwrite, FALSE) &
        any(file.exists(files))) {
        skip <- files[file.exists(files)]
        message(paste("Skipping", toString(names(skip))))
        files <- files[!file.exists(files)]
    }
    if (!isTRUE(quiet)) {
        message(paste("Saving", toString(names(files)), "to", dir))
    }
    mapply(save,
           list = names(files),
           file = files,
           MoreArgs = list(envir = parent.frame(),
                           compress = compress))
    # Silently return the file paths as a named character vector
    invisible(files)
}
