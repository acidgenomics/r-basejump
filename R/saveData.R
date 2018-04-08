#' Save Data
#'
#' Wrapper for [base::save()] supporting quick saving of object names passed as
#' symbols. This function saves each object into a separate `.rda` file rather
#' than combining into a single file.
#'
#' @note This function is desired for interactive use and interprets object
#' names using non-standard evaluation.
#'
#' @family Write Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams loadData
#' @inheritParams base::save
#' @param overwrite Overwrite existing file.
#'
#' @note These function will *overwrite* existing saved data, following the
#'   same conventions as [base::save()]. Conversely, [devtools::use_data()] does
#'   not overwrite by default if that behavior is preferred.
#'
#' @seealso
#' - [base::save()].
#' - `usethis::use_data()`.
#'
#' @return Silent named character vector of file paths.
#' @export
#'
#' @examples
#' saveData(rnaseqCounts, singleCellCounts, dir = "example")
#' list.files("example")
#'
#' # Clean up
#' unlink("example", recursive = TRUE)
saveData <- function(
    ...,
    dir = ".",
    overwrite = TRUE,
    compress = TRUE
) {
    objectNames <- dots(..., character = TRUE)
    assert_is_character(objectNames)
    dir <- initializeDirectory(dir)
    assert_is_a_bool(overwrite)
    assertFormalCompress(compress)

    files <- file.path(dir, paste0(objectNames, ".rda"))
    names(files) <- objectNames

    message(paste("Saving", toString(basename(files)), "to", dir))

    # If `overwrite = FALSE`, inform the user which files were skipped
    if (identical(overwrite, FALSE) && any(file.exists(files))) {
        skip <- files[file.exists(files)]
        warning(paste("Skipping", toString(basename(skip))))
        files <- files[!file.exists(files)]
    }

    if (!length(files)) {
        warning("No files were saved")
        return(invisible())
    }

    mapply(
        FUN = save,
        list = names(files),
        file = files,
        MoreArgs = list(
            envir = parent.frame(),
            compress = compress
        )
    )

    invisible(files)
}
