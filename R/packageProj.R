#' Build package and website.
#'
#' @param install Install package.
#' @param test Run tests with [testthat::testthat()].
#'
#' @export
packageProj <- function(
    install = FALSE,
    test = FALSE) {
    # Ensure package is up to date
    devtools::document()
    devtools::build_vignettes()
    devtools::load_all()

    # Run integrity checks
    BiocCheck(getwd())
    devtools::check()
    if (isTRUE(test)) {
        devtools::test()
    }

    # Save the build to disk
    devtools::build()

    # Install the package
    if (isTRUE(install)) {
        devtools::install()
    }

    # Ensure safe developer environment
    biocValid()
}



#' @rdname packageProj
#' @export
package_proj <- packageProj
