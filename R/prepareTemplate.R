# We're covering the code below in bcbioRNASeq and bcbioSingleCell.
# nocov start



#' Prepare R Markdown Template File
#'
#' If the required template dependency files aren't present, copy them from the
#' requested package. Existing files are not overwritten by default. This
#' function will copy dependency files from a requested package inside the
#' `rmarkdown/shared/` directory. If a package doesn't contain this
#' subdirectory, the function will return an error.
#'
#' This code is used internally by:
#'
#' - `bcbioRNASeq::prepareRNASeqTemplate`.
#' - `bcbioSingleCell::prepareSingleCellTemplate`.
#'
#' @export
#'
#' @inheritParams params
#'
#' @param package `character(1)`.
#'   Name of package containing the R Markdown template.
#' @param overwrite `logical(1)`.
#'   Should existing destination files be overwritten?
#'
#' @return Invisible `logical`.
#'   Was the file copied?.
#'
#' @examples
#' ## RNA-seq template.
#' \dontrun{
#' # prepareTemplate(package = "bcbioRNASeq")
#' }
#'
#' ## Single-cell RNA-seq template.
#' \dontrun{
#' # prepareTemplate(package = "bcbioSingleCell")
#' }
prepareTemplate <- function(package, overwrite = FALSE) {
    assert(
        isString(package),
        isSubset(package, rownames(installed.packages())),
        isFlag(overwrite)
    )

    copySharedFiles <- function(sourceDir, overwrite) {
        assert(isADirectory(sourceDir))
        files <- list.files(sourceDir, full.names = TRUE)
        assert(hasLength(files))
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
        copied
    }

    files <- character()

    # Copy the shared files from the requested package.
    sourceDir <-
        system.file("rmarkdown/shared", package = package, mustWork = TRUE)
    files1 <- copySharedFiles(sourceDir, overwrite = overwrite)

    # Copy the shared files from basejump.
    sourceDir <-
        system.file("rmarkdown/shared", package = "basejump", mustWork = TRUE)
    files2 <- copySharedFiles(sourceDir, overwrite = overwrite)

    invisible(c(files1, files2))
}



# nocov end
