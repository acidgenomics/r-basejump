#' Build package and website
#'
#' @author Michael Steinbaugh
#'
#' @export
packageProj <- function() {
    document()
    build_vignettes()
    BiocCheck(getwd())
    load_all()
    check()
    # test()
    build()
    install()
}
