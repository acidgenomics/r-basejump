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
#' @param sourceDir `string`. File path to shared source file directory.
#'   Normally this can be left `NULL` when working in a standard interactive
#'   session, but is necessary when developing code in a devtools package
#'   environment loaded with `devtools::load_all()`.
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
    sourceDir = NULL,
    ...
) {
    # Assert checks
    assert_is_a_string(package)
    # Package must be installed.
    stopifnot(package %in% rownames(installed.packages()))
    assertIsAStringOrNULL(sourceDir)
    assert_is_a_bool(overwrite)

    # Shared file source directory
    # Keeping the `sourceDir` argument because devtools attempts to intercept
    # `system.file`, and this can cause path issues during development.
    if (is.null(sourceDir)) {
        sourceDir <- system.file(
            "rmarkdown/shared",
            package = package,
            mustWork = TRUE
        )
    }
    assert_all_are_dirs(sourceDir)

    # We're covering the code below in bcbioRNASeq and bcbioSingleCell.
    # nocov start

    # Get vector of all shared files.
    files <- list.files(sourceDir, full.names = TRUE)
    assert_is_non_empty(files)
    assert_all_are_non_empty_files(files)

    # Copy files to working directory.
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
    # nocov end
}
