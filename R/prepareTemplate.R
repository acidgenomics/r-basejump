# nocov start



#' Prepare R Markdown Template File
#'
#' If the required template dependency files aren't present, download latest
#' versions from the package website. Existing files are never overwritten.
#'
#' By default, this function will create local copies of these files:
#'
#' - `_footer.Rmd`
#' - `_header.Rmd`
#' - `_output.yaml`
#' - `_setup.R`
#' - `bibliography.bib`
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
#' @param file `character`. File name(s).
#' @param overwrite `boolean`. Should existing destination files be overwritten?
#' @param sourceDir `string`. Source directory path.
#'
#' @return Invisible `logical` indicating which files were copied.
#' @export
#'
#' @examples
#' # RNA-seq pipeline
#' \dontrun{
#' prepareTemplate(
#'     sourceDir = system.file("rmarkdown/shared", package = "bcbioRNASeq")
#' )
#' }
prepareTemplate <- function(
    file = c(
        "_output.yaml",
        "_footer.Rmd",
        "_header.Rmd",
        "_setup.R",
        "bibliography.bib"
    ),
    sourceDir,
    overwrite = FALSE
) {
    assert_is_character(file)
    assert_all_are_dirs(sourceDir)
    assert_is_a_string(sourceDir)
    assert_all_are_existing_files(file.path(sourceDir, file))
    invisible(mapply(
        file = file,
        MoreArgs = list(sourceDir = sourceDir),
        FUN = function(file, sourceDir) {
            file.copy(
                from = file.path(sourceDir, file),
                to = file,
                overwrite = overwrite
            )
        },
        SIMPLIFY = TRUE,
        USE.NAMES = TRUE
    ))
}



# nocov end
