#' Prepare R Markdown Template File
#'
#' If the required template dependency files aren't present, copy them from the
#' requested package. Existing files are not overwritten by default. This
#' function will copy dependency files from a requested package inside the
#' `rmarkdown/shared` directory. If a package doesn't contain this
#' subdirectory, the function will return an error.
#'
#' This code is used internally by:
#'
#' - `bcbioRNASeq::prepareRNASeqTemplate()`.
#' - `bcbioSingleCell::prepareSingleCellTemplate()`.
#'
#' @family Prepare Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#'
#' @param package `string`. Name of package containing the R Markdown template.
#' @param overwrite `boolean`. Should existing destination files be overwritten?
#'
#' @return Invisible `logical` indicating which files were copied.
#' @export
#'
#' @examples
#' # RNA-seq template
#' \dontrun{
#' prepareTemplate(package = "bcbioRNASeq")
#' }
#'
#' # Single-cell RNA-seq template
#' \dontrun{
#' prepareTemplate(package = "bcbioSingleCell")
#' }
prepareTemplate <- function(
    package,
    overwrite = FALSE,
    ...
) {
    # Legacy arguments
    dots <- list(...)
    # `prepareTemplate()`: Use `package` instead of `sourceDir`
    if (missing(package) && "sourceDir" %in% names(dots)) {
        sourceDir <- dots[["sourceDir"]]
        stopifnot(grepl(file.path("rmarkdown", "shared"), sourceDir))
        package <- basename(dirname(dirname(sourceDir)))
    }

    # Assert checks
    assert_is_a_string(package)
    assert_is_a_bool(overwrite)

    # Shared file source directory
    sourceDir <- system.file("rmarkdown/shared", package = package)
    assert_all_are_dirs(sourceDir)

    # Get vector of all shared files
    files <- list.files(sourceDir, full.names = TRUE)
    assert_is_non_empty(files)
    assert_all_are_non_empty_files(files)

    # Copy files to working directory
    copied <- vapply(
        X = files,
        FUN = function(file) {
            file.copy(
                from = file,
                to = basename(file),
                overwrite = overwrite
            )
        },
        FUN.VALUE = logical(1L)
    )
    names(copied) <- basename(files)

    invisible(copied)
}
