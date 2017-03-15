#' Build package and website
#'
#' @author Michael Steinbaugh
#' @keywords dev package website
#'
#' @import BiocCheck
#' @import devtools
#' @import pkgdown
#'
#' @export
pack <- function() {
    #` devtools::load_all()
    devtools::document()
    devtools::build_vignettes()
    BiocCheck::BiocCheck(getwd())
    devtools::check()
    devtools::build()
    #` devtools::install()
    pkgdown::build_site()
}
