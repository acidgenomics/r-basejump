#' Build package and website
#'
#' @author Michael Steinbaugh
#'
#' @import devtools
#' @importFrom BiocCheck BiocCheck
#' @importFrom pkgdown build_site
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
