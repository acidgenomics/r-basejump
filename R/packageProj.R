#' Build package and website
#'
#' @author Michael Steinbaugh
#'
#' @param install Install package
#' @param test Run tests with \code{testthat}
#'
#' @export
packageProj <- function(
    install = FALSE,
    test = FALSE) {
    # Ensure package is up to date
    document()
    build_vignettes()
    load_all()

    # Run integrity checks
    BiocCheck(getwd())
    check()
    if (isTRUE(test)) {
        devtools::test()
    }

    # Save the build to disk
    build()

    # Install the package
    if (isTRUE(install)) {
        devtools::install()
    }

    # Ensure safe developer environment
    biocValid()
}
