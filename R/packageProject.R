#' Build package and website
#'
#' @author Michael Steinbaugh
#'
#' @export
packageProject <- function() {
    document()
    build_vignettes()
    BiocCheck(getwd())
    load_all()
    check()
    build()
    install()
    build_site()
}
